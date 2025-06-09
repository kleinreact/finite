-----------------------------------------------------------------------------
-- |
-- Module      :  Finite.Class
-- Maintainer  :  Felix Klein
--
-- 'Finite' main class decleration including generics support.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ConstraintKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , ImplicitParams
  , LambdaCase
  , MultiParamTypeClasses
  , TypeOperators
  , ScopedTypeVariables
  , RequiredTypeArguments
  , ViewPatterns

  #-}

-----------------------------------------------------------------------------

module Finite.Class
  ( FiniteBounds
  , Finite(..)
  , GFinite(..)
  ) where

-----------------------------------------------------------------------------

import Control.Exception
  ( assert
  )

import GHC.Generics
  ( Generic
  , Rep
  , (:*:)(..)
  , (:+:)(..)
  , U1(..)
  , M1(..)
  , K1(..)
  , from
  , to
  )

import qualified Data.IntSet as S
  ( toList
  , fromList
  , fromAscList
  , difference
  )

-----------------------------------------------------------------------------

-- | A better looking constraint specifier.

type FiniteBounds b = (?bounds :: b)

-----------------------------------------------------------------------------

-- | The 'Finite' class.
class Finite b a where
  -- | Returns the number of elements associated with the given type.
  elements :: forall c -> (a ~ c, FiniteBounds b) => Int

  default elements
    :: (Generic a, GFinite b (Rep a)) =>
    forall c -> (a ~ c, FiniteBounds b) => Int
  elements _ = gelements (Rep a)

  -- | Turns the value in the associated range into an Int uniquely
  -- identifiying the value.
  index :: FiniteBounds b => a -> Int

  default index ::
    (Generic a, GFinite b (Rep a), FiniteBounds b) =>
    a -> Int
  index v = (+ (offset a)) $ gindex $ from v

  -- | Turns an Int back to the value that is associated with it.
  value :: FiniteBounds b => Int -> a

  default value ::
    (Generic a, GFinite b (Rep a), FiniteBounds b) => Int -> a

  value v = assert (v >= o && v < o + e) $ to $ gvalue (v - o)
   where
    o = offset a
    e = elements a

  -- | Allows to put an offset to the integer mapping. Per default the
  -- offset is zero.
  offset :: forall c -> (a ~ c, FiniteBounds b) => Int
  offset _ = 0

  -- | Returns a finite list of all elements of that type.
  values :: FiniteBounds b => [a]
  values = value <$> [o, o + 1 .. o + n - 1]
   where
    n = elements a
    o = offset a


  -- | Complements a given list of elements of that type
  complement :: FiniteBounds b => [a] -> [a]
  complement xs = value <$> ys
   where
    o  = offset a
    n  = elements a
    s  = S.fromList $ map index xs
    as = S.fromAscList [o, o + 1 .. o + n - 1]
    ys = S.toList $ S.difference as s

  -- | Less than operator according to the implicit total index order.
  (|<|) :: FiniteBounds b => a -> a -> Bool
  x |<| y = index x < index y
  infixr |<|

  -- | Less or equal than operator according to the implicit total
  -- index order.
  (|<=|) :: FiniteBounds b => a -> a -> Bool
  x |<=| y = index x <= index y
  infixr |<=|

  -- | Greater or equal than operator according to the implicit total
  -- index order.
  (|>=|) :: FiniteBounds b => a -> a -> Bool
  x |>=| y = index x >= index y
  infixr |>=|

  -- | Greater than operator according to the implicit total index order.
  (|>|) :: FiniteBounds b  => a -> a -> Bool
  x |>| y = index x > index y
  infixr |>|

  -- | Equal operator according to the implicit total index order.
  (|==|) :: FiniteBounds b => a -> a -> Bool
  x |==| y = index x == index y
  infixr |==|

  -- | Unequal operator according to the implicit total index order.
  (|/=|) :: FiniteBounds b  => a -> a -> Bool
  x |/=| y = index x /= index y
  infixr |/=|

  -- | First element according to the total index order.
  initial :: forall c -> (a ~ c, FiniteBounds b) => a
  initial x = value $ offset x

  -- | Last element according to the total index order.
  final :: forall c -> (a ~ c, FiniteBounds b) => a
  final x = value $ offset x + elements x - 1

  -- | Next element according to the total index order (undefined for
  -- the last element).
  next :: FiniteBounds b => a -> a
  next (index -> i) = assert (i < offset a + elements a - 1) $ value (i + 1)

  -- | Previous element according to the total index order (undefined
  -- for the first element).
  previous :: FiniteBounds b => a -> a
  previous (index -> i) = assert (i > offset a) $ value (i - 1)

  -- | The upper and lower bounds of the instance.
  bounds :: forall c -> (c ~ a, FiniteBounds b) => (a, a)
  bounds x = (initial x, final x)

-----------------------------------------------------------------------------

-- | Generics implementation for the 'Finite' class. The
-- realization is closely related to the one presented at
-- https://wiki.haskell.org/GHC.Generics.
class GFinite b f where
  gelements :: forall c -> (c ~ f, FiniteBounds b) => Int
  gindex :: FiniteBounds b => f a -> Int
  gvalue :: FiniteBounds b => Int -> f a

-----------------------------------------------------------------------------

-- | :*: instance.
instance (GFinite b f, GFinite b g) => GFinite b (f :*: g) where
  gelements _ = gelements f * gelements g
  gindex (f :*: g) = gindex f * gelements (type g) + gindex g
  gvalue n = f :*: g
   where
    m = gelements (type g)
    f = gvalue (n `div` m)
    g = gvalue (n `mod` m)

-----------------------------------------------------------------------------

-- | :+: instance.
instance (GFinite b f, GFinite b g) => GFinite b (f :+: g) where
  gelements _ = gelements f + gelements g
  gindex = \case
    R1 x -> gindex x
    L1 x -> gindex x + gelements g
  gvalue n
    | n < m     = R1 g
    | otherwise = L1 f
   where
    m = gelements (type g)
    g = gvalue (n `mod` m)
    f = gvalue (n - m)

-----------------------------------------------------------------------------

-- | U1 instance.
instance GFinite c U1 where
  gelements _ = 1
  gindex U1 = 0
  gvalue _ = U1

-----------------------------------------------------------------------------

-- | M1 instance.
instance GFinite c f => GFinite c (M1 i v f) where
  gelements _ = gelements f
  gindex (M1 x) = gindex x
  gvalue = M1 . gvalue

-----------------------------------------------------------------------------

-- | K1 instance.
instance Finite b a => GFinite b (K1 i a) where
  gelements _ = elements a
  gindex (K1 x) = index x - offset a
  gvalue n = K1 $ value (n + offset a)

-----------------------------------------------------------------------------
