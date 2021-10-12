module Legacy.Tensor {-# DEPRECATED "Use Vector and Tensor instead" #-} where

newtype Tensor' a = Tensor' [a]
    deriving(Show, Eq)

instance Functor Tensor' where
    fmap f (Tensor' t) = Tensor' $ fmap f t

instance Applicative Tensor' where
    (Tensor' f) <*> (Tensor' t) = Tensor' $ f <*> t
    pure a = Tensor' $ pure a

instance Monad Tensor' where
    (Tensor' ts) >>= f = Tensor' $ mconcat result
        where Tensor' result = traverse f ts

instance (Num a) => Num (Tensor' a) where
    (Tensor' x) + (Tensor' y) = Tensor' $ zipWith (+) x y
    (Tensor' x) * (Tensor' y) = Tensor' $ zipWith (*) x y
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure.fromInteger
    negate = fmap negate
