{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Vector where

import Tensor
import Data.Monoid
import Data.Maybe

data Vector a = Scalar a | Row [Vector a] | Column [Vector a]

-- This Eq implementation ignores structural differences
instance Eq a => Eq (Vector a) where
    x == y = wx == wy && hx == hy && go 0 0
        where
            wx = width x
            wy = width y
            hx = height x
            hy = height y
            go i j
                | i == hx = True
                | j == wx = go (i + 1) 0
                | otherwise = atMaybe x (i, j) == atMaybe y (i, j) && go i (j + 1)

instance Functor Vector where
    fmap f (Scalar s) = Scalar $ f s
    fmap f (Row t) = Row $ map (fmap f) t
    fmap f (Column t) = Column $ map (fmap f) t

instance Applicative Vector where
    pure = Scalar
    Scalar s <*> t = fmap s t
    t <*> (Scalar s) = fmap ($s) t
    Row r <*> t = Row $ map (<*> t) r
    Column c <*> t = Column $ map (<*> t) c

instance Monad Vector where
    Scalar s >>= f = f s
    Row r >>= f = Row $ map (>>= f) r
    Column c >>= f = Column $ map (>>= f) c

instance Num a => Num (Vector a) where
    Scalar x + (Scalar y) = Scalar $ x + y
    Row x + (Row y) = Row $ zipWith (+) x y
    Column x + (Column y) = Column $ zipWith (+) x y
    _ + _ = error "Vector addition: dimension mismatch"
    Scalar x * y = fmap (* x) y
    x * Scalar y = fmap (* y) x
    Row x * (Column y) = getSum $ foldMap Sum $ zipWith (*) x y
    Column x * (Column y) = Column $ map (* Column y) x
    x * (Row y) = Row $ map (x *) y
    abs = fmap abs
    signum = fmap signum
    fromInteger = Scalar . fromInteger
    negate = fmap negate

instance Foldable Vector where
  foldr f acc (Scalar s) = f s acc
  foldr f acc (Row r) = foldr (flip (foldr f)) acc r
  foldr f acc (Column c) = foldr (flip (foldr f)) acc c

instance Traversable Vector where
    traverse f (Scalar s) = Scalar <$> f s
    traverse f (Row r) = Row <$> traverse (traverse f) r
    traverse f (Column c) = Column <$> traverse (traverse f) c

instance (Num a) => Semigroup (Vector a) where
    x <> y = (*) <$> x <*> y

instance (Num a) => Monoid (Vector a) where
    mempty = Scalar 1

instance (forall a. Monoid (Vector a)) => Tensor Vector where
    norm t = (/ (sqrt.getSum $ foldMap (Sum . (**2) . abs) t)) <$> t
    fromList xs dim = go (foldr (con . Scalar) (Column []) xs) dim
        where
            go (Column c) n
                | n == 1 = Column [go (Row c) dim]
                | l <= fromIntegral dim = Column c
                | otherwise = go (Row $ take (l `div` fromIntegral n) c) dim
                    `con` go (Column $ drop (l `div` fromIntegral n) c) (n - 1)
                where l = length c
            
            go (Row r) n
                | n == 1 = Row [go (Column r) dim]
                | l <= fromIntegral dim = Row r
                | otherwise = go (Column $ take (l `div` fromIntegral n) r) dim
                    `con` go (Row $ drop (l `div` fromIntegral n) r) (n - 1)
                where l = length r

            go s n = error "error when adding tensor rank"
    rank (Scalar s) = 0
    rank (Row []) = 1
    rank (Column []) = 1
    rank (Row (r:rs)) = rank r + 1
    rank (Column (c:cs)) = rank c + 1

con :: Vector a -> Vector a -> Vector a
con x (Row y) = Row $ x : y
con x (Column y) = Column $ x : y
con _ _ = error "Appending to Scalar is prohibited"

width :: Integral b => Vector a -> b
width (Scalar s) = 1
width (Row r) = getSum $ foldMap (Sum . width) r
width (Column c) = width $ Prelude.head c

height :: Integral b => Vector a -> b
height (Scalar s) = 1
height (Column c) = getSum $ foldMap (Sum . height) c
height (Row r) = height $ Prelude.head r

-- Transform structure retaining the original rank and shape
transformStruct :: Vector a -> Vector a
transformStruct (Row (Column c:cs)) = Column . go $ Column c:cs
    where
        go (Row [] : _) = []
        go (Column [] : _) = []
        go cs = Row (map (transformStruct . Vector.head) cs)
            : go (map Vector.tail cs)

transformStruct (Column (Row r:rs)) = Row . go $ Row r:rs
    where
        go (Row [] : _) = []
        go (Column [] : _) = []
        go rs = Column (map (transformStruct . Vector.head) rs)
            : go (map Vector.tail rs)

transformStruct (Column cs) = Column $ map transformStruct cs
transformStruct (Row rs) = Row $ map transformStruct rs
transformStruct t = t


-- transpose via structure
transposeStruct :: Vector a -> Vector a
transposeStruct (Scalar s) = Scalar s
transposeStruct (Row r) = Column $ map transposeStruct r
transposeStruct (Column c) = Row $ map transposeStruct c

-- transpose retaining structure
transpose :: Vector a -> Vector a
transpose = transformStruct . transposeStruct

head :: Vector a -> Vector a
head (Scalar s) = Scalar s
head (Row r) = Prelude.head r
head (Column c) = Prelude.head c

last :: Vector a -> Vector a
last (Scalar s) = Scalar s
last (Row r) = Prelude.last r
last (Column c) = Prelude.last c

tail :: Vector a -> Vector a
tail (Scalar s) = Scalar s
tail (Row r) = Row $ Prelude.tail r
tail (Column c) = Column $ Prelude.tail c

init :: Vector a -> Vector a
init (Scalar s) = Scalar s
init (Row r) = Row $ Prelude.init r
init (Column c) = Column $ Prelude.init c

atMaybe :: Integral b => Vector a -> (b, b) -> Maybe a
atMaybe (Scalar s) (0, 0) = Just s
atMaybe (Column (c:cs)) (i, j)
    | height (Column (c:cs)) > i = let offset = height c in
                                    if offset > i
                                    then atMaybe c (i, j)
                                    else atMaybe (Column cs) (i - offset, j)
    | otherwise = Nothing

atMaybe (Row (r:rs)) (i, j)
    | width (Row (r:rs)) > j = let offset = width r in
                                    if offset > j
                                    then atMaybe r (i, j)
                                    else atMaybe (Row rs) (i, j - offset)
    | otherwise = Nothing

atMaybe _ _ = Nothing

at :: Integral b => a -> Vector a -> (b, b) -> a
at x y z = fromMaybe x (atMaybe y z)

dot :: Num a => Vector a -> Vector a -> Vector a
dot x y = transposeStruct x * y

{-
    Tensor type:
    All one-dimensional tensors are column vectors. This means matrices,
    which are rank 2 tensors, compose of multiple column tensors, which
    may not follow conventional representation in other traversable types.
-}

-- data Tensor a = Scalar a | Tensor [Tensor a]
--     deriving(Eq)

-- instance (Show a) => Show (Tensor a) where
--     show (Scalar s) = show s
--     show (Tensor ((Tensor t):ts)) = "[\n" ++ unlines (go (Tensor t:ts)) ++ "]"
--         where go = fmap ("  " ++).concatMap(lines . show)
--     show (Tensor s) = show s ++ "\n"

-- instance Functor Tensor where
--     fmap f (Scalar s) = Scalar $ f s
--     fmap f (Tensor t) = Tensor $ map (fmap f) t

-- instance Applicative Tensor where
--     pure = Scalar
--     (Scalar s) <*> t = fmap s t
--     t <*> (Scalar s) = fmap ($ s) t
--     (Tensor []) <*> _ = Tensor []
--     (Tensor (Scalar s : ss)) <*> t = Tensor $ fmap s t : tensorList
--         where Tensor tensorList = Tensor ss <*> t
--     (Tensor (Tensor t : ts)) <*> t2 = Tensor $ (Tensor t <*> t2) : tensorList
--         where Tensor tensorList = Tensor ts <*> t2

-- instance Monad Tensor where
--     (Scalar s) >>= f = f s
--     (Tensor t) >>= f = Tensor $ map (>>= f) t

-- instance (Num a) => Num (Tensor a) where
--     (Scalar s) + t = fmap (+ s) t
--     t + (Scalar s) = fmap (+ s) t
--     (Tensor xs) + (Tensor ys) = Tensor $ zipWith (+) xs ys
--     (Scalar s) * t = fmap (* s) t
--     t * (Scalar s) = fmap (* s) t
--     _ * Tensor[] = Tensor []
--     (Tensor x) * (Tensor y) = getSum $ foldMap Sum (zipWith go x y)
--         where
--             go (Tensor a) b = Tensor $ map (* b) a
--             go a b = a * b
--     abs = fmap abs
--     signum = fmap signum
--     negate = fmap negate
--     fromInteger i = Scalar $ fromInteger i

-- instance Foldable Tensor where
--     foldr f acc (Tensor t) = foldr (flip $ foldr f) acc t
--     foldr f acc (Scalar s) = f s acc

-- instance Traversable Tensor where
--     traverse f (Scalar s) = Scalar <$> f s
--     traverse f (Tensor t) = Tensor <$> traverse (traverse f) t

-- -- TODO: revise function for higher-dimension tensors
-- fromList :: [a] -> Int -> Tensor a
-- fromList xs dim = go (foldr (con . Scalar) (Tensor[]) xs) dim
--     where
--         go (Tensor t) n
--             | n == 1 = Tensor [go (Tensor t) dim]
--             | length t <= dim = Tensor t
--             | otherwise = go (Tensor $ take (length t `div` n) t) dim
--                 `con` go (Tensor $ drop (length t `div` n) t) (n - 1)
--         go s n = error "error when adding tensor rank"


-- product :: (Num a) => Tensor a -> Tensor a -> Tensor a
-- product (Scalar s) t = Scalar s * t
-- product t (Scalar s) = t * Scalar s
-- product (Tensor []) _ = Tensor []
-- product _ (Tensor []) = Tensor []
-- product (Tensor (x:xs)) y = Tensor $ current : rest
--     where
--         current = Tensor.product x y
--         Tensor rest = Tensor.product (Tensor xs) y

-- norm :: (Floating a) => Tensor a -> Tensor a
-- norm t= (/ (sqrt.getSum $ foldMap (Sum . (**2) . abs) t)) <$> t

-- append :: Tensor a -> Tensor a -> Tensor a
-- append (Tensor x) (Tensor y) = Tensor $ x ++ y
-- append _ _ = error "Concatenating scalars is prohibited"

-- con :: Tensor a -> Tensor a -> Tensor a
-- con x (Tensor y) = Tensor $ x : y
-- con _ _ = error "Appending to Scalar is prohibited"

-- dot :: Num a => Tensor a -> Tensor a -> a
-- dot (Tensor x) (Tensor y) = getSum $ foldMap Sum (zipWith dot x y)
-- dot (Scalar x) (Scalar y) = x * y
-- dot _ _ = error "Length mismatch"

-- flatten2d :: Tensor a -> Tensor a
-- flatten2d (Tensor ((Tensor ((Tensor (t:ts)):ss)):xs)) = Tensor . go $ Tensor (Tensor (t:ts):ss):xs
--     where
--         go :: [Tensor a] -> [Tensor a]
--         go (Tensor (Tensor (Scalar s:ss):us):ys) = foldr (\a acc ->
--             let (Tensor result) = Tensor.transpose a
--                 in result ++ acc
--             ) [] (Tensor (Tensor (Scalar s:ss):us):ys)

--         go y = go flattened
--             where flattened = foldr (\a acc ->
--                     let (Tensor next) = a
--                         in Tensor (go next) : acc
--                     ) [] y
-- flatten2d t = t

-- head :: Tensor a -> Tensor a
-- head (Tensor ts) = Prelude.head ts
-- head t = t

-- last :: Tensor a -> Tensor a
-- last (Tensor ts) = Prelude.last ts
-- last t = t

-- tail :: Tensor a -> Tensor a
-- tail (Tensor ts) = Tensor $ Prelude.tail ts
-- tail t = t

-- init :: Tensor a -> Tensor a
-- init (Tensor ts) = Tensor $ Prelude.init ts
-- init t = t

-- transpose :: Tensor a -> Tensor a
-- transpose (Tensor (Tensor t:ts)) = Tensor $ go $ Tensor t:ts
--     where
--         go :: [Tensor a] -> [Tensor a]
--         go (Tensor [] : _) = []
--         go xs = Tensor (map Tensor.head xs) : go (map Tensor.tail xs)
-- transpose (Tensor (Scalar s:ss)) = Tensor $ map (Tensor.(:[])) (Scalar s:ss)
-- transpose t = t

-- isSquare :: Tensor a -> Bool
-- isSquare (Scalar _) = True
-- isSquare (Tensor []) = True
-- isSquare (Tensor t) = getAll $ go (Tensor t) (length t)
--     where
--         go (Tensor x) l
--             | length x == l = foldMap (`go` l) x
--             | otherwise = All False
--         go (Scalar _) _ = All True

-- rank :: Integral b => Tensor a -> b
-- rank (Scalar s) = 0
-- rank (Tensor []) = 1
-- rank (Tensor (t:ts)) = rank t + 1


