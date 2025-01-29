-----------------------------------------------------------------------------
-- |
-- Module      :  Finite.PowerSet
-- Maintainer  :  Felix Klein
--
-- Encodes the powerset of a finite range type.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , LambdaCase
  , MultiWayIf
  , BangPatterns

  #-}

-----------------------------------------------------------------------------

module Finite.PowerSet
  ( PowerSet
  ) where

-----------------------------------------------------------------------------

import Finite.Type
  ( T
  , (#)
  , (\#)
  , (#<<)
  , v2t
  )

import Finite.Class
  ( Finite(..)
  )

import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

-- | Powersets are just lists of the correpsonding elements. The type
-- has only been added for clearification. Consider the corresponding
-- instance of 'Finite' for possible applications.

type PowerSet a = [a]

-----------------------------------------------------------------------------

-- | If the number of elements associated with a type is finite, then
-- it also has finite number of powersets.

instance Finite b a => Finite b (PowerSet a) where

  elements =
    pow2 2 . elements . ((\#) :: T (PowerSet a) -> T a)

    where
      pow2 !a !n = case n of
        0 -> 1
        1 -> a
        _ -> pow2 (2*a) (n-1)

  index = \case
    []     -> 0
    (y:yr) -> powsum (0,2,idx y,yr)

    where
      idx x = index x - offset #<< x

      powsum !p = case p of
        (a,_,0,[])   ->
          a + (1 - (a `mod` 2))
        (a,p,1,[])   ->
          a + ((1 - ((a `mod` (2*p)) `div` p)) * p)
        (a,_,0,x:xr) ->
          powsum (a + (1 - (a `mod` 2)),2,idx x,xr)
        (a,p,1,x:xr) ->
          powsum (a + ((1 - ((a `mod` (2*p)) `div` p)) * p), 2, idx x, xr)
        (a,p,n,xs)   ->
          powsum (a,2*p,n-1,xs)

  value n =
    let ty :: T [a] -> T a
        ty _ = (#)
        bs = map (value . (+ (offset $ ty $ v2t bs))) $ bin n
    in assert (n >= 0 && n < (elements #<< bs)) bs

  offset = offset . ((\#) :: T (PowerSet a) -> T a)

  values = powerset values

-----------------------------------------------------------------------------

-- | Converts an Int value to a list of Int values of logarithmic size
-- encoding the original value.

bin
  :: Int -> [Int]

bin x =
  let
    bin (a,!s,!n)
      | n <= 0         = reverse a
      | n `mod` 2 == 1 = bin (s:a, s+1, n `div` 2)
      | otherwise     = bin (a, s+1, n `div` 2)
  in
    bin ([],0,x)

-----------------------------------------------------------------------------

-- | Creates the powerset of a set, for sets represented as lists. If
-- the given list is sorted, the created powerset will be sorted
-- lexographically and the elements themselve will be sorted as well.

powerset
  :: [a] -> [[a]]

powerset =
  let f x a = [x] : foldr ((:) . (x:)) a a
  in  ([]:) . foldr f []

-----------------------------------------------------------------------------
