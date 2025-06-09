-----------------------------------------------------------------------------
-- |
-- Module      :  Finite
-- Maintainer  :  Felix Klein
--
-- A framework for capturing finite ranges with types, where the sizes
-- of the ranges are not fixed statically at compile time, but instead
-- are passed at run-time via implicit parameters. The purpose of the
-- framework is to simplify the handling of objects of bounded size,
-- e.g. finite-state machines, where the number of elements can be
-- defined in the context of the object, e.g. the number of states.
--
-- The framework supports:
--
-- * Easy access to the object's elements via types.
-- * Efficient bidirectional mappings between indices and the elements.
-- * Implicit total orderings on the elements.
-- * Powerset Support.
-- * Extension of a single context to a range of contexts via collections.
-- * Easy passing of the context via implict parameters.
-- * Generics Support: Finite range types can be easily constructed out
--   of other finite range types using Haskell's `data` constructor.
-- * Template Haskell: Easy creation of basic finite instances using
--   short Haskell templates, as well as the extension of existing
--   types to more feature rich parameter spaces (requires the
--   explicit import of @Finite.TH@).
--
-----------------------------------------------------------------------------

module Finite
  ( -- * The Finite Class
    FiniteBounds
  , Finite(..)
  , GFinite(..)
  , withBounds
  , -- * Powersets
    PowerSet
  , -- * Collections
    Collection(..)
  ) where

-----------------------------------------------------------------------------

import Finite.Class
  ( FiniteBounds
  , Finite(..)
  , GFinite(..)
  , withBounds
  )

import Finite.PowerSet
  ( PowerSet
  )

import Finite.Collection
  ( Collection(..)
  )

-----------------------------------------------------------------------------
