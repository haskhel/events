{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Data.Functor.Identity
import qualified Data.Text

wholeList :: Applicative f => (a -> f b) -> [a] -> f [b]
wholeList f [] = pure []
wholeList f (x:xs) = pure (:) <*> fx <*> rest
  where fx = f x
        rest = wholeList f xs

_1 :: Functor f => (a -> f b) -> (a,x) -> f (b,x)
_1 f (a,x) = fmap (\b -> (b,x)) $ f a

set :: (forall f. Functor f => (a -> f b) -> s -> f t) -> b -> s -> t
set l value container = runIdentity $ l (\_old -> Identity value) container

-- set _1 3 (True, True) => (3, True)
-- set wholeList 3 [True, True, True] => error

set' :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set' l value container = runIdentity $ l (\_old -> Identity value) container

-- set' _1 3 (True, True) => (3, True)
-- set' wholeList 3 [True, True, True] => [3, 3, 3]

type Traversal s t a b = Applicative f => (a -> f b) -> s -> f t
type Setter s t a b = ((a -> Identity b) -> s -> Identity t)

over :: Setter s t a b -> (a -> b) -> s -> t
over l f container = runIdentity $ l f' container
  where f' = Identity . f

set'' :: Setter s t a b -> b -> s -> t
set'' = set'

both :: Traversal (a,a) (b,b) a b
both f (x,y) = pure (,) <*> f x <*> f y

wholeText :: Traversal Data.Text.Text Data.Text.Text Char Char
wholeText f t = fmap Data.Text.pack $ wholeList f string
  where string = Data.Text.unpack t

-- set'' wholeText 'a' (Data.Text.pack "hello world") => "aaaaaaaaaaa"
-- over both (+1) (2,3) => (3,4)

askNew :: (Show a, Read a) => a -> IO a
askNew old = do putStr ": old "
                print old
                putStrLn ": new?"
                readLn

-- wholeList askNew [1,2,3]
