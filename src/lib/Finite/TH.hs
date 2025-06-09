-----------------------------------------------------------------------------
-- |
-- Module      :  Finite.TH
-- Maintainer  :  Felix Klein
--
-- Template haskell for easy instance generation using newtypes.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    CPP
  , ImplicitParams
  , ImpredicativeTypes
  , LambdaCase
  , RequiredTypeArguments
  , TemplateHaskell
  , TypeOperators
  , ViewPatterns

  #-}

-----------------------------------------------------------------------------

module Finite.TH
  ( newInstance
  , baseInstance
  , newBaseInstance
  , extendInstance
  , polyType
  ) where

-----------------------------------------------------------------------------

import qualified Data.Ix
  ( Ix
  , index
  , range
  , inRange
  )

import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , shrink
  )

import Data.Hashable
  ( Hashable
  , hashWithSalt
  )

import Finite.Class
  ( FiniteBounds
  , Finite(..)
  )

import Data.Char
  ( toLower
  , isUpper
  )

import Control.Exception
  ( assert
  )

import Language.Haskell.TH
  ( Q
  , Dec
  , Exp
#if MIN_VERSION_template_haskell(2,12,0)
  , DerivClause(..)
#endif
  , Type(..)
  , mkName
  , conT
  , appT
  , conP
  , varP
  , tupP
  , wildP
  , varE
  , conE
  , tupE
  , appE
  , funD
  , newtypeD
  , instanceD
  , recC
  , normalB
  , cxt
  , clause
  , bangType
  , varBangType
  , bang
  , noSourceUnpackedness
  , noSourceStrictness
  )

-----------------------------------------------------------------------------

-- | Creates a new basic type using the name provided as a string. The
-- template defines the corresponding data type using the provided
-- name and a corresponding access function using the same name with
-- the first letter moved to lower case. Furthermore, it also
-- instanciates corresponding `Show`, `Hashable`, 'Ix', 'Arbitrary',
-- and 'Num' instances.
--
-- >>> newInstance "Example"
-- <BLANKLINE>
-- newtype Example =
--   Example { example :: Int }
--   deriving (Eq, Ord)
-- <BLANKLINE>
-- instance Show Example where
--   show (Example x) = show x
-- <BLANKLINE>
-- instance Hashable Example where
--   hashWithSalt s (Example x) = hashWithSalt s x
-- <BLANKLINE>
-- instance Ix Example where
--   range (l,u) = map Example $ range (example l, example u)
--   index (l,u) x = index (example l, example u) (example x)
--   inRange (l,u) x = inRange (example l, example u) (example x)
-- <BLANKLINE>
-- instance Arbitrary Example where
--   arbitrary = Example <$> arbitrary
--   shrink (Example x) = map Example $ shrink x
-- <BLANKLINE>
-- instance Num Example where
--   (Example x) + (Example y) = Example (a + b)
--   (Example x) - (Example y) = Example (a - b)
--   (Example x) * (Example y) = Example (a * b)
--   abs = Example . abs . example
--   negate = Example . negage . example
--   signum = Example . signum . example
--   fromInteger = Example . fromInteger
newInstance :: String -> Q [Dec]
newInstance = \case
  [] -> assert False undefined
  (x:xr) -> assert (isUpper x) $ do
    let
      tmpV = mkName "x"
      conC = mkName $ x : xr
      accV = mkName $ toLower x : xr
      emptyContext = cxt []
      intT = conT (''Int)

    d_newtype <-
      newtypeD
        -- no context
        emptyContext
        -- newtype name
        conC
        -- no type parameters
        []
        -- no kinds
        Nothing
        -- newtype constructor
        (recC -- normalC
           conC
           [varBangType
              accV
              (bangType
                (bang noSourceUnpackedness noSourceStrictness)
                intT)])
        -- derive 'Eq' and 'Ord'
#if MIN_VERSION_template_haskell(2,12,0)
        [ return (DerivClause
                  Nothing
                  [ ConT (''Eq)
                  , ConT (''Ord)
        ])
        ]
#else
        (return [ConT (''Eq), ConT (''Ord)])
#endif

    d_show_instance <-
      instanceD
        -- no context
        emptyContext
        -- instance of 'Show'
        (appT (conT (''Show)) (conT conC))
        -- declare 'show'
        [ funD ('show)
            [ clause
                -- pattern match constructor
                [conP conC [varP tmpV]]
                -- show inner content
                (normalB (appE (varE ('show)) (varE tmpV)))
                --
                [] ] ]

    d_hashable_instance <-
      instanceD
        -- no context
        emptyContext
        -- instance of 'Hashable'
        (appT (conT (''Hashable)) (conT conC))
        -- declare 'hashWithSalt'
        [ funD ('hashWithSalt)
            [ clause
                -- pattern match constructor
                [varP (mkName "s"), conP conC [varP tmpV]]
                -- show inner content
                (normalB (appE (appE (varE ('hashWithSalt))
                                     (varE (mkName "s")))
                               (varE tmpV)))
                [] ] ]

    d_ix_instance <-
      instanceD
        -- no context
        emptyContext
        -- instance of 'Ix'
        (appT (conT (''Data.Ix.Ix)) (conT conC))
        -- declare 'range'
        [ funD ('Data.Ix.range)
            [ clause
                -- pattern match constructor
                [tupP [ varP (mkName "l"), varP (mkName "s") ]]
                -- show inner content
                (normalB
                  (appE
                    (appE (varE ('map)) (conE conC))
                    (appE
                       (varE ('Data.Ix.range))
                       (tupE [ appE (varE accV) (varE (mkName "l"))
                             , appE (varE accV) (varE (mkName "s"))
                             ] ))))
                [] ]

        , funD ('Data.Ix.index)
            [ clause
                -- pattern match constructor
                [tupP [ varP (mkName "l"), varP (mkName "s") ]
                ,conP conC [varP tmpV]
                ]
                -- show inner content
                (normalB
                  (appE
                     (appE
                        (varE ('Data.Ix.index))
                        (tupE [ appE (varE accV) (varE (mkName "l"))
                              , appE (varE accV) (varE (mkName "s"))
                              ] ))
                     (varE tmpV) ))
                [] ]
        , funD ('Data.Ix.inRange)
            [ clause
                -- pattern match constructor
                [tupP [ varP (mkName "l"), varP (mkName "s") ]
                ,conP conC [varP tmpV]
                ]
                -- show inner content
                (normalB
                  (appE
                     (appE
                        (varE ('Data.Ix.inRange))
                        (tupE [ appE (varE accV) (varE (mkName "l"))
                              , appE (varE accV) (varE (mkName "s"))
                              ] ))
                     (varE tmpV) ))
                [] ] ]

    d_num_instance <-
      instanceD
        -- no context
        emptyContext
        -- instance of 'Num'
        (appT (conT (''Num)) (conT conC))
        -- declare '(+)'
        [ funD ('(+))
           [ clause
             -- pattern match constructor
             [ conP conC [varP (mkName "x")]
             , conP conC [varP (mkName "y")]
             ]
             -- (+) inner content
             (normalB
               (appE
                 (conE conC)
                 (appE
                   (appE
                     (varE ('(+)))
                     (varE (mkName "x")))
                   (varE (mkName "y")))))
             [] ]
        , funD ('(-))
           [ clause
             -- pattern match constructor
             [ conP conC [varP (mkName "x")]
             , conP conC [varP (mkName "y")]
             ]
             -- (+) inner content
             (normalB
               (appE
                 (conE conC)
                 (appE
                   (appE
                     (varE ('(-)))
                     (varE (mkName "x")))
                   (varE (mkName "y")))))
             [] ]
         , funD ('(*))
           [ clause
             -- pattern match constructor
             [ conP conC [varP (mkName "x")]
             , conP conC [varP (mkName "y")]
             ]
             -- (+) inner content
             (normalB
               (appE
                 (conE conC)
                 (appE
                   (appE
                     (varE ('(*)))
                     (varE (mkName "x")))
                   (varE (mkName "y")))))
             [] ]
         , funD ('abs)
           [ clause
             -- pattern match constructor
             [conP conC [varP tmpV]]
             -- show inner content
             (normalB (appE (conE conC) (appE (varE ('abs)) (varE tmpV))))
             --
             [] ]
         , funD ('negate)
           [ clause
             -- pattern match constructor
             [conP conC [varP tmpV]]
             -- show inner content
             (normalB
               (appE
                 (conE conC)
                 (appE (varE ('negate)) (varE tmpV))))
             --
             [] ]
         , funD ('signum)
           [ clause
             -- pattern match constructor
             [conP conC [varP tmpV]]
             -- show inner content
             (normalB
               (appE
                 (conE conC)
                 (appE (varE ('signum)) (varE tmpV))))
             --
             [] ]
         , funD ('fromInteger)
           [ clause
             -- pattern match constructor
             [varP tmpV]
             -- show inner content
             (normalB
               (appE
                 (conE conC)
                 (appE (varE ('fromInteger)) (varE tmpV))))
             --
             [] ] ]

    d_arbitrary_instance <-
      instanceD
        -- no context
        emptyContext
        -- instance of 'Hashable'
        (appT (conT (''Arbitrary)) (conT conC))
        -- declare 'hashWithSalt'
        [ funD ('arbitrary)
            [ clause
                -- pattern match constructor
                []
                -- show inner content
                (normalB
                   (appE
                      (appE
                         (varE ('(<$>)))
                         (conE conC))
                      (varE ('arbitrary))))
                [] ]
        , funD ('shrink)
            [ clause
                -- pattern match constructor
                [conP conC [varP tmpV]]
                -- show inner content
                (normalB
                   (appE
                      (appE (varE ('map)) (conE conC))
                      (appE (varE ('shrink)) (varE tmpV))))
                [] ] ]

    return
      [ d_newtype
      , d_show_instance
      , d_hashable_instance
      , d_ix_instance
      , d_num_instance
      , d_arbitrary_instance
      ]

-----------------------------------------------------------------------------

-- | Creates a basic finite instance using the bounds provided via the
-- first argument, the access function provided by the second argument
-- and the name provided as a string.
--
-- >>> baseInstance [t|Bounds|] [|getBound|] "Example"
-- <BLANKLINE>
-- instance Finite Bounds Example where
--   elements _ = getBound ?bounds
--   value = Example
--   index = example
baseInstance :: Q Type -> Q Exp -> String -> Q [Dec]
baseInstance bounds f = \case
  []     -> assert False undefined
  (x:xr) -> assert (isUpper x) $ do
    let
      tmpV = mkName "x"
      conC = mkName $ x : xr
      emptyContext = cxt []

    d_finite_instance <-
      instanceD
        -- no context
        emptyContext
        -- instanc of 'Finite'
        (appT (appT (conT (''Finite)) bounds) (conT conC))
        -- declare
        [ funD ('elements)
            [ clause
                -- ignore the pattern
                [ wildP ]
                -- get the value from the configuartion
                (normalB (appE (varE 'appBounds) f))
                --
                [] ]
        , funD ('value)
            [ clause
                -- get the value
                [ varP tmpV ]
                -- apply the constructor
                (normalB
                  (appE
                    (appE
                      (varE 'assert)
                      (appE
                        (appE (varE 'inRange) (varE tmpV))
                        (appE (varE 'appBounds) f)))
                    (appE (conE conC) (varE tmpV))))
                --
                [] ]
        , funD ('index)
            [ clause
                -- get the value
                [ conP conC [varP tmpV] ]
                -- apply the destructor
                (normalB
                  (appE
                    (appE
                      (varE 'assert)
                      (appE
                        (appE (varE 'inRange) (varE tmpV))
                        (appE (varE 'appBounds) f)))
                    (varE tmpV)))
                --
                [] ]
        ]

    return [ d_finite_instance ]

-----------------------------------------------------------------------------

-- | Combined 'newInstance' with 'baseInstance'.
newBaseInstance :: Q Type -> Q Exp -> String -> Q [Dec]
newBaseInstance bounds f name = do
  xs <- newInstance name
  ys <- baseInstance bounds f name
  return $ xs ++ ys

-----------------------------------------------------------------------------

-- | Extends a Finite instance to an extended parameter space. The
-- first argument takes the type to be extended, the second argument
-- the type of the new parameter space and the third argument a
-- translator function that translates the old parameter space into
-- the new one.
--
-- >>> :i Bounds
-- <BLANKLINE>
-- instance Finite Bounds Example
-- <BLANKLINE>
-- >>> :t derive
-- <BLANKLINE>
-- derive :: NewBounds -> Bounds
-- <BLANKLINE>
-- >>> extendInstance [t|Example|] [t|NewBounds] [|translate|]
-- <BLANKLINE>
-- instance Finite NewBounds Example where
--   elements = let ?bounds = translate ?bounds in elements
--   offset = let ?bounds = translate ?bounds in offset
--   value = let ?bounds = translate ?bounds in value
--   index = let ?bounds = translate ?bounds in index
extendInstance :: Q Type -> Q Type -> Q Exp -> Q [Dec]
extendInstance rtype bounds access = do
  let tmpV = mkName "x"
  d_finite_instance <-
    instanceD
      -- no context
      (cxt [])
      -- instanc of 'Finite'
      (appT (appT (conT (''Finite)) bounds) rtype)
      -- declare
      [ funD ('elements)
        [ clause
          -- ignore the pattern
          [ varP tmpV ]
          -- get the value from the configuartion
          (normalB
            (appE
              (appE
                (varE 'elementsSwitch)
                access)
              (varE tmpV)))
          --
          [] ]
      , funD ('offset)
        [ clause
          -- ignore the pattern
          [ varP tmpV ]
          -- get the value from the configuartion
          (normalB
            (appE
              (appE
                (varE 'offsetSwitch)
                access)
              (varE tmpV)))
          --
          [] ]
      , funD ('value)
        [ clause
          -- ignore the pattern
          [ varP tmpV ]
          -- get the value from the configuartion
          (normalB
            (appE
              (appE
                (varE 'valueSwitch)
                access)
              (varE tmpV)))
          --
          [] ]
      , funD ('index)
        [ clause
          -- ignore the pattern
          [ varP tmpV ]
          -- get the value from the configuartion
          (normalB
            (appE
              (appE
                (varE 'indexSwitch)
                access)
              (varE tmpV)))
          --
          [] ]
        ]
  return [d_finite_instance]

-----------------------------------------------------------------------------

-- | Constructs a polymorph type given a type constructor and a free
-- type variable. Such a construction cannot be expressed in quotation
-- syntax directly.
--
-- >>> polyType [t|Maybe|] "a"
-- <BLANKLINE>
-- Maybe a
polyType :: Q Type -> String -> Q Type
polyType con (mkName -> name) = do
  t <- con
  return $ t `AppT` VarT name

-----------------------------------------------------------------------------

appBounds :: FiniteBounds b=> (b -> a) -> a
appBounds x = x ?bounds

-----------------------------------------------------------------------------

elementsSwitch ::
  (Finite b' a, FiniteBounds b) => (b -> b') ->
  Finite b a => forall c -> (c ~ a, FiniteBounds b) => Int
elementsSwitch f x =
  let ?bounds = f ?bounds
  in elements x

-----------------------------------------------------------------------------

offsetSwitch ::
  (Finite b' a, FiniteBounds b) => (b -> b') ->
  Finite b a => forall c -> (c ~ a, FiniteBounds b) => Int
offsetSwitch f x =
  let ?bounds = f ?bounds
  in offset x

-----------------------------------------------------------------------------

indexSwitch :: (Finite b' a, FiniteBounds b) => (b -> b') -> a -> Int
indexSwitch f = let ?bounds = f ?bounds in index

-----------------------------------------------------------------------------

valueSwitch :: (Finite b' a, FiniteBounds b) => (b -> b') -> Int -> a
valueSwitch f = let ?bounds = f ?bounds in value

-----------------------------------------------------------------------------

inRange :: Int -> Int -> Bool
inRange x y = x >= 0 && x < y

-----------------------------------------------------------------------------
