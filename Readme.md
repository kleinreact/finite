# Finite

The library provides the Haskell class `Finite`, which allows to
associate types with finite ranges of elements in the context of a
bounding environment. The purpose of the class is to simplify the
handling of objects of bounded size, e.g. finite-state machines, where
the number of elements can be defined in the context of the object,
e.g. the number of states.

## Main Features

* Easy access to the object's elements via types.

* Efficient bidirectional mappings between indices and the elements.

* Implicit total orderings on the elements.

* Powerset Support.

* Extension of a single context to a range of contexts via
  collections.

* Easy passing of the context via [implict
parameters](https://www.haskell.org/hugs/pages/users_guide/implicit-parameters.html).

* [Generics](https://wiki.haskell.org/Generics) Support: Finite range
  types can be easily constructed out of other finite range types
  using Haskell's `data` constructor.

* [Template Haskell](https://wiki.haskell.org/Template_Haskell): Easy
  creation of basic finite instances using short Haskell templates, as
  well as the extension of existing types to more feature rich
  parameter spaces.

## Example: Finite-State Machines

```haskell
import Finite
import Finite.TH

-- create two new basic types for the states and labels of the FSM
newInstance "State"
newInstance "Label"

-- FSM data type
data FSM =
  FSM
    { states :: Int
    , labels :: Int
    , transition :: State -> Label -> State
    , accepting :: State -> Bool
    , labelName :: Label -> String
    }

-- connect the types with corresponding bounds, as defined by the FSM
baseInstance [t|FSM|] [|states|] "State"
baseInstance [t|FSM|] [|labels|] "Label"

-- FSM Show instance for demonstrating the features of 'Finite'
instance Show FSM where
  show fsm =
    -- set the context
    let ?bounds = fsm in
    -- show the data
    unlines $
      [ "The FSM has " ++ show (elements (type State)) ++ " states."
      ] ++
      [ "Labels:"
      ] ++
      [ "  (" ++ show (index l) ++ ") " ++ labelName fsm l
      | l <- values
      ] ++
      [ "Transitions:"
      ] ++
      [ "  " ++ show (index s) ++ " -- " ++ labelName fsm l ++
        " --> " ++ show (index (transition fsm s l))
      | s <- values
      , l <- values
      ] ++
      [ "Accepting:"
      ] ++
      [ "  " ++ show (map index $ filter (accepting fsm) values)
      ]
```
