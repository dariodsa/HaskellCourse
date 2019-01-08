-- ========================================================================== --
{- |
= PUH Level 4 Battle

  Welcome to your fourth PUH Battle. You know the drill. Solve all three tasks
  to win and follow the instructions. If you think something is ambiguous,
  **ask for clarification in the main slack channel (#haskell)**. Follow the
  type signatures and don't modify them.

-}
-- ========================================================================== --
module LevelBattle where
--
import Prelude hiding ( head, tail )
--

{- * DATA TYPES, RECURSIVE DATA STRUCTURES, PARAMETERISED TYPES, MAYBE TYPE, -}

{- ** LB 4.1 -}

{- |
  Define a custom linked list type which is guaranteed to have at least one
  element by construction. This means that it is physically (or rather logically
  in this case) impossible to construct a list with no elements in it.

  You shouldn't use stock list data type, instead you should define everything
  your self.

  Your list data type should have a parameter for a type of data it is storing,
  and it should be a sum type consisting of two constructors.

  First constructor 'Item' is a record containing two fields 'head' and 'tail'.
  'head' should contain a single element of a list and 'tail' should contain the
  rest of the list.

  Second constructor 'Last' should have only one field 'item' which contains a
  single element of a list.

  Along with the 'List' data type you should provide 'fromList' and 'toList'
  functions which can convert standard Haskell list to and from our custom
  'List' data type.
-}

data List a

fromList :: [ a ] -> Maybe ( List a )
fromList = undefined

toList :: List a -> [ a ]
toList = undefined

{- * DERIVING TYPECLASS INSTANCES -}

{- ** LB 4.2 -}

{- *
  Great, now that we have a list, you should also **derive** and **not** define
  'Eq' and 'Show' instances. This proved to be a rather hard task in the first
  version of this LB so be careful ;).
-}

{- * DEFINING TYPECLASS INSTANCES, FMAP -}

{- ** LB 4.3 -}

{- *
  Finally, **define** a 'Functor' instance for our 'List'.
-}

instance Functor List where
  fmap = undefined
