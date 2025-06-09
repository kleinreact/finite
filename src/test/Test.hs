-----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Felix Klein
--
-- Simple TestSuite.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    DeriveGeneric
  , ExplicitNamespaces
  , ImplicitParams
  , LambdaCase
  , MultiParamTypeClasses
  , RankNTypes
  , RecordWildCards
  , TemplateHaskell

  #-}

-----------------------------------------------------------------------------

module Test
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite
  ( TestInstance(..)
  , Progress(..)
  , Result(..)
  , Test(..)
  )

import Data.Hashable
  ( hash
  )

import Data.Ix
  ( range
  )

import Control.Exception
  ( assert
  )

import GHC.Generics
  ( Generic
  )

import Test.QuickCheck
  ( Result
      ( Success
      , Failure
      , reason
      )
  , quickCheckResult
  )

import Finite.TH

import Finite

-----------------------------------------------------------------------------

newInstance "AInst"

data Bounds = Bounds { size :: Int }

baseInstance [t|Bounds|] [|size|] "AInst"

newBaseInstance [t|Bounds|] [|size|] "BInst"

data BBounds = BBounds { bnds :: Bounds }

extendInstance [t|AInst|] [t|BBounds|] [|bnds|]

data GInst =
    AData
  | BData AInst
  | CData AInst BInst
  deriving (Eq, Ord, Generic)

instance Finite Bounds GInst

newtype OInst = OInst { oInst :: Int } deriving (Eq, Ord)

instance Finite Bounds OInst where
  elements _ = size ?bounds
  offset _ = 3
  value = OInst
  index = oInst

data TInst =
    DData
  | EData OInst
  | FData AInst BInst
  deriving (Eq, Ord, Generic)

instance Finite Bounds TInst

-----------------------------------------------------------------------------

tests
  :: IO [Test]

tests = return
  [ Test t01
  , Test t02
  , Test t03
  , Test t04
  , Test t05
  , Test t06
  ]

  where
    t01 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> x == aInst (AInst x)
              , \x -> AInst x == AInst x
              , \x -> AInst x < AInst (x + 1)
              , \x -> show (AInst x) == show (AInst x)
              , \x -> show (AInst x) == show x
              , \x -> hash (AInst x) == hash (AInst x)
              , \x -> not $ null $ range (AInst 0, AInst $ abs x)
              , \x -> (AInst x) + (AInst 1) == (AInst 1) + (AInst x)
              ] ++
              [ quickCheckResult $ \x -> aInst x == aInst x
              ])
      , name = "TH: newInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t01
      }

    t02 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> sb x $ elements (type AInst) == abs x + 1
              , \x -> sb x $ offset (type AInst) == 0
              , \x -> sb x $ map index vs == [0,1..abs x]
              , \x -> sb x $ map value [0,1..abs x] == vs
              , \x -> sb x $ all (\x -> x == value (index x)) vs
              , \x -> sb x $ complement (filter (odd . index) vs)
                               == filter (even . index) vs
              , \x -> sb x $ initial (type AInst) |<=| final (type AInst)
              , \x -> sb x $ abs x < 1 ||
                               next (initial (type AInst))
                                 |>=| initial (type AInst)
              , \x -> sb x $ abs x < 1 ||
                               previous (final (type AInst))
                                 |/=| final (type AInst)
              ])
      , name = "TH: baseInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t02
      }
     where
      vs :: FiniteBounds Bounds => [AInst]
      vs = values

      sb :: Int -> (FiniteBounds Bounds => c) -> c
      sb x = withBounds $ Bounds $ abs x + 1

    t03 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> x == bInst (BInst x)
              , \x -> BInst x == BInst x
              , \x -> BInst x < BInst (x + 1)
              , \x -> show (BInst x) == show (BInst x)
              , \x -> show (BInst x) == show x
              , \x -> hash (BInst x) == hash (BInst x)
              , \x -> not $ null $ range (BInst 0, BInst $ abs x)
              , \x -> (BInst x) + (BInst 1) == (BInst 1) + (BInst x)
              , \x -> sb x $ elements (type BInst) == abs x + 1
              , \x -> sb x $ offset (type BInst) == 0
              , \x -> sb x $ map index vs == [0,1..abs x]
              , \x -> sb x $ map value [0,1..abs x] == vs
              , \x -> sb x $ all (\x -> x == value (index x)) vs
              , \x -> sb x $ complement (filter (odd . index) vs)
                               == filter (even . index) vs
              , \x -> sb x $ initial (type BInst) |<=| final (type BInst)
              , \x -> sb x $ abs x < 1 ||
                               next (initial (type BInst))
                                 |>=| initial (type BInst)
              , \x -> sb x $ abs x < 1 ||
                               previous (final (type BInst))
                                 |/=| final (type BInst)
              ] ++
              [ quickCheckResult $ \x -> bInst x == bInst x
              ])
      , name = "TH: newBaseInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t03
      }
     where
      vs :: FiniteBounds Bounds => [BInst]
      vs = values

      sb :: Int -> (FiniteBounds Bounds => c) -> c
      sb x = withBounds $ Bounds $ abs x + 1

    t04 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> sb x $ elements (type AInst) == abs x + 1
              , \x -> sb x $ offset (type AInst) == 0
              , \x -> sb x $ map index vs == [0,1..abs x]
              , \x -> sb x $ map value [0,1..abs x] == vs
              , \x -> sb x $ all (\x -> x == value (index x)) vs
              , \x -> sb x $ complement (filter (odd . index) vs)
                               == filter (even . index) vs
              , \x -> sb x $ initial (type AInst) |<=| final (type AInst)
              , \x -> sb x $ abs x < 1 ||
                             next (initial (type AInst))
                               |>=| initial (type AInst)
              , \x -> sb x $ abs x < 1 ||
                             previous (final (type AInst))
                               |/=| final (type AInst)
              ])
      , name = "TH: extendInstance"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t04
      }
     where
      vs :: FiniteBounds BBounds => [AInst]
      vs = values

      sb :: Int -> (FiniteBounds BBounds => c) -> c
      sb x = withBounds $ BBounds $ Bounds $ abs x + 1

    t05 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> sb x $ elements (type GInst)
                               == 1 + (abs x + 1) + (abs x + 1) * (abs x + 1)
              , \x -> sb x $ offset (type GInst) == 0
              , \x -> sb x $ map index vs == [0,1..elements (type GInst) - 1]
              , \x -> sb x $ map value [0,1..elements (type GInst) - 1] == vs
              , \x -> sb x $ all (\x -> x == value (index x)) vs
              , \x -> sb x $ complement (filter (odd . index) vs)
                               == filter (even . index) vs
              , \x -> sb x $ initial (type GInst) |<=| final (type GInst)
              , \x -> sb x $ abs x < 1 ||
                             next (initial (type GInst))
                               |>=| initial (type GInst)
              , \x -> sb x $ abs x < 1 ||
                             previous (final (type GInst))
                               |/=| final (type GInst)
              ])
      , name = "Generics"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t05
      }
     where
      vs :: FiniteBounds Bounds => [GInst]
      vs = values

      sb :: Int -> (FiniteBounds Bounds => c) -> c
      sb x = withBounds $ Bounds $ abs x + 1

    t06 = TestInstance
      { run =
         (Finished . allPass) <$> sequence
           (map quickCheckResult
              [ \x -> sb x $ elements (type OInst) == abs x + 1
              , \x -> sb x $ offset (type OInst) == 3
              , \x -> sb x $ map index ovs == [3,4..abs x + 3]
              , \x -> sb x $ map value [3,4..abs x + 3] == ovs
              , \x -> sb x $ all (\x -> x == value (index x)) ovs
              , \x -> sb x $ complement (filter (odd . index) ovs)
                        == filter (even . index) ovs
              , \x -> sb x $ initial (type OInst) |<=| final (type OInst)
              , \x -> sb x $ abs x < 1 ||
                             next (initial (type OInst))
                               |>=| initial (type OInst)
              , \x -> sb x $ abs x < 1 ||
                             previous (final (type OInst))
                               |/=| final (type OInst)
              , \x -> sb x $ elements (type TInst)
                               == 1 + (abs x + 1) + (abs x + 1) * (abs x + 1)
              , \x -> sb x $ offset (type TInst) == 0
              , \x -> sb x $ map index tvs == [0,1..elements (type TInst) - 1]
              , \x -> sb x $ map value [0,1..elements (type TInst) - 1] == tvs
              , \x -> sb x $ all (\x -> x == value (index x)) tvs
              , \x -> sb x $ complement (filter (odd . index) tvs)
                               == filter (even . index) tvs
              , \x -> sb x $ initial (type TInst) |<=| final (type TInst)
              , \x -> sb x $ abs x < 1 ||
                             next (initial (type TInst))
                               |>=| initial (type TInst)
              , \x -> sb x $ abs x < 1 ||
                             previous (final (type TInst))
                               |/=| final (type TInst)
              ])
      , name = "Offset"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t06
      }
     where
      ovs :: FiniteBounds Bounds => [OInst]
      ovs = values

      tvs :: FiniteBounds Bounds => [TInst]
      tvs = values

      sb :: Int -> (FiniteBounds Bounds => c) -> c
      sb x = withBounds $ Bounds $ abs x + 1

    allPass xs = case dropWhile (isSuccess) xs of
      []  -> Pass
      x:_ -> case x of
        Failure{..} -> Fail reason
        _           -> assert False undefined

    isSuccess = \case
      Success {} -> True
      _          -> False

-----------------------------------------------------------------------------
