import Control.Lens

data Item a = Item {amount :: Int, location :: String, value :: a}
  deriving Show

_amount :: Lens (Item a) (Item a) Int Int
_amount = lens get set
   where get :: Item a -> Int
         get (Item amount _ _) = amount
         set :: Item a -> Int -> Item a
         set (Item _ loc val) amount = Item amount loc val

_location :: Lens (Item a) (Item a) String String
_location = lens get set
   where get (Item _ loc _) = loc
         set (Item amount _ val) loc = Item amount loc val

_value :: Lens (Item a) (Item b) a b
_value = lens get set
   where get (Item _ _ val) = val
         set (Item amount loc _) val = Item amount loc val
