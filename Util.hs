module Util where

import Control.Arrow;
import Control.Monad;

list :: (a -> [a] -> b) -> b -> [a] -> b;
list f y []     = y;
list f y (x:xs) = f x xs;

swap :: (a,b) -> (b,a);
swap (x,y) = (y,x);

infixr 3 &=&;
(&=&) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m (b, c);
(&=&) = curry $ uncurry (&&&) >>> (uncurry (liftM2 (,)) .);

infixr 3 *=*;
(*=*) :: Monad m => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d);
(*=*) = curry $ uncurry (***) >>> (uncurry (liftM2 (,)) .);

applyA :: ArrowApply a => a b (a b c) -> a b c;
applyA = (&&& (arr id)) >>> (>>> app);

apM :: Monad m => m (a -> m b) -> a -> m b;
apM = fmap join . (. return) . ap;

infixr 1 >*>;
(>*>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c;
f >*> g = f >=> return . g;

infixl 1 >>*;
(>>*) :: Monad m => m a -> (a -> b) -> m b;
(>>*) = flip liftM;

(<<) :: Monad m => m a -> m b -> m a;
(<<) = flip (>>);
