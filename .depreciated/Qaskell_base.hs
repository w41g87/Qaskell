module Qaskell_base where

import Data.Complex
import Data.Either
import Data.List
import Control.Applicative

newtype StateVector = StateVector [Complex Double]
    deriving (Eq, Show)

newtype TransformTensor = TransformTensor [[Complex Double]]
    deriving (Eq, Show)

{-| 
    integerLog2 determines if an integer is a power of 2
    If it is, the log base 2 of the integer will be wrapped
    by Right constructor; if not, Left constructor with
    corresponding error will be returned.
    The function takes 1 argument of type 'Int'
-}
integerLog2 :: Int -> Either String Int
integerLog2 x
    | x < 1 = Left "Input less than 0"
    | x == 1 = Right 0
    | y * 2 == x = (+1) <$> integerLog2 y
    | otherwise = Left "Input is not a power of 2"
    where
        y = quot x 2

initQubit :: Complex Double -> Complex Double -> Either String StateVector
initQubit a b
    | magnitude a ** 2 + magnitude b ** 2 == 1 = Right $ StateVector [a, b]
    | otherwise = Left "Magnitude Error"

initQubit0 :: StateVector
initQubit0 = StateVector [1:+0, 0:+0]

initNumQubits :: Int -> Either String StateVector
initNumQubits n
    | n < 1 = Left "Number of Qubits Must be Greater than 1"
    | otherwise = Right $ foldr mergeState (StateVector [1 :+ 0]) $ replicate n initQubit0

initState :: [Complex Double] -> Either String StateVector
initState q
    | isLeft numQubits = Left "Input State Dimension Invalid"
    | otherwise = Right $ StateVector q
    where
        numQubits = integerLog2 $ length q


mergeState :: StateVector -> StateVector -> StateVector
mergeState (StateVector x) (StateVector y) = StateVector $ liftA2 (*) x y

mergeTensor :: TransformTensor -> TransformTensor -> TransformTensor
mergeTensor (TransformTensor x) (TransformTensor y) = TransformTensor $ fmap ((<*>) . fmap (*)) x <*> y

tensorAdd :: TransformTensor -> TransformTensor -> Either String TransformTensor
tensorAdd (TransformTensor i) (TransformTensor j)
    | m == x && n == y = Right $ TransformTensor $ zipWith (zipWith (+)) i j
    | otherwise = Left "Adding Two Tensors with Different Dimensions"
    where
        m = length i
        n = length $ head i
        x = length j
        y = length $ head j

doubleBindEither :: Either String TransformTensor -> Either String TransformTensor -> (TransformTensor -> TransformTensor -> Either String TransformTensor) -> Either String TransformTensor
doubleBindEither x y f
    | isLeft x && isRight y = Left $ fromLeft [] x ++ "\n" ++ fromLeft [] y
    | isLeft x = x
    | isLeft y = y
    | otherwise = f (fromRight (TransformTensor [[]]) x) (fromRight (TransformTensor [[]]) y)

applyTensorToState :: TransformTensor -> StateVector -> Either String StateVector
applyTensorToState (TransformTensor g) (StateVector q)
    | length f == length q && length q == length g =
        Right $ StateVector $ foldr (zipWith (+)) acc $ zipWith fmap (fmap (*) q) f
    | otherwise = Left "Transformation Matrix Dimension Mismatch"
    where
        acc = replicate (length q) $0:+0
        f = transpose g

-- The qubits are 1-indexed
applyGateToQubit :: TransformTensor -> Int -> StateVector -> Either String StateVector
applyGateToQubit g n (StateVector q) = t >>= (`applyTensorToState` StateVector q)
    where
        numQubits = integerLog2 $ length q
        t = numQubits >>= flip (genTensor g) n

-- Gate to insert -> Number of Qubits -> Index of Qubit
genTensor :: TransformTensor -> Int -> Int -> Either String TransformTensor
genTensor g n i
    | n < i || i < 1 = Left "Index Out of Range"
    | otherwise = Right $ foldr mergeTensor (TransformTensor [[1:+0]]) expGate
    where
        expGate = fstId ++ [g] ++ lstId
        fstId = replicate (i - 1) pauliId
        lstId = replicate (n - i) pauliId

getQubitState :: Int -> StateVector -> Either String (Double, Double)
getQubitState x (StateVector q)
    | x < 1 = Left "Index Out of Bounds"
    | otherwise = Right (zeroP, oneP)
    where
        numRow = 2 ^ (x - 1)
        q2 = fmap (** 2) q
        zeroP = magnitude $ sum $ takeHalf q2 numRow
        oneP = magnitude $ sum $ takeHalf (drop numRow q2) numRow
        takeHalf :: [Complex Double] -> Int -> [Complex Double]
        takeHalf xs n
            | length xs < n = []
            | otherwise = take n xs ++ takeHalf (drop (2 * n) xs) n

-- Gate to Apply -> Control Bit Index -> Application Bit Index -> Circuit To Apply
applyControlToQubits :: TransformTensor -> Int -> Int -> StateVector -> Either String StateVector
applyControlToQubits g m n (StateVector q)
    | m < n = t m n mask1 g >>= (`applyTensorToState` (StateVector q))
    | m > n = t n m g mask1 >>= (`applyTensorToState` (StateVector q))
    | otherwise = Left "Control and Application Qubit Cannot be Identical"
    where
        zeroMask = numQubits >>= flip (genTensor mask0) m
        numQubits = integerLog2 $ length q
        t :: Int -> Int -> TransformTensor -> TransformTensor -> Either String TransformTensor
        t i j fstGate sndGate = doubleBindEither (fmap mergeTensor fstTensor <*> sndTensor) zeroMask tensorAdd
            where
                fstTensor = genTensor fstGate i i
                sndTensor = numQubits >>= (flip (genTensor sndGate) (j - i)) . (flip (-) i)

pauliX :: TransformTensor
pauliX = TransformTensor [[0 :+ 0, 1 :+ 0],
                      [1 :+ 0, 0 :+ 0]]

pauliY :: TransformTensor
pauliY = TransformTensor [[0 :+ 0, 0 :+ (-1)],
                      [0 :+ 1, 0 :+ 0]]

pauliZ :: TransformTensor
pauliZ = TransformTensor [[1 :+ 0, 0 :+ 0],
                      [0 :+ 0, (-1) :+ 0]]

pauliId :: TransformTensor
pauliId = TransformTensor [[1 :+ 0, 0 :+ 0],
                      [0 :+ 0, 1 :+ 0]]

hadamard :: TransformTensor
hadamard = TransformTensor [[1 / sqrt 2 :+ 0, 1 / sqrt 2 :+ 0],
                        [1 / sqrt 2 :+ 0, (-1) / sqrt 2 :+ 0]]

mask0 :: TransformTensor
mask0 = TransformTensor [[1 :+ 0, 0 :+ 0],
                    [0 :+ 0, 0 :+ 0]]

mask1 :: TransformTensor
mask1 = TransformTensor [[0 :+ 0, 0 :+ 0],
                    [0 :+ 0, 1 :+ 0]]

main :: IO()
main = print result
    where q0 = initQubit0
          crt = mergeState q0 q0
          flip1 = applyGateToQubit hadamard 1 crt
          flip2 = flip1 >>= applyControlToQubits pauliY 1 2
          --result = flip2 >>= (getQubitState 2)
          result = TransformTensor [[0 :+ 0, 0 :+ 0],
                                    [0 :+ 0, 1 :+ 0]]


-- Linear algebra versions


-- safeMultiply :: Num a => T.Tensor a -> T.Tensor a -> Either String (T.Tensor a)
-- safeMultiply (T.Tensor x) (T.Tensor y)
--     | T.isSquare (T.Tensor x)
--         && T.isSquare (T.Tensor y)
--         && length x == length y = Right $ T.Tensor x * T.Tensor y
--     | otherwise = Left "Dimension mismatch"
-- safeMultiply x y = Right $ x * y

-- -- combine two qubit circuits
-- addQubit :: Num a => T.Tensor a -- ^ first half statevector
--   -> T.Tensor a -- ^ second half statevector
--   -> T.Tensor a
-- addQubit (T.Tensor x) (T.Tensor y)
--     | T.rank (T.Tensor x) /= 1 || T.rank (T.Tensor y) /= 1 = error "One of the elements is not a valid statevector"
--     | otherwise = T.Tensor $ liftA2 (*) x y
-- addQubit _ _ = error "One of the elements is not a valid statevector"

-- Applies input gate to qubit of index

-- -- Builds a transformation tensor to apply input gate to selected qubit
-- build :: RealFloat a => Int -- ^ Number of qubits this tensor applies to
--   -> [(Int, T.Tensor (Complex a))]-- ^ Index / Gate pair (Pre-sorted)
--   -> T.Tensor (Complex a)-- ^ Default Gate
--   -> T.Tensor (Complex a)
-- build len lst dft = T.flatten2d $ foldr T.product (T.fromList [1]) (go len lst 0)
--     where 
--         go l (x:xs) offset
--             | fst x >= l = go l [] offset
--             | otherwise = replicate (fst x - offset) dft ++ [snd x] ++ go l xs (fst x + 1)
--         go l [] offset = replicate (l - offset) dft

-- applyGate :: RealFloat a => T.Tensor (Complex a) -- ^ Gate tensor
--   -> Int -- ^ Index
--   -> T.Tensor (Complex a) -- ^ State vector
--   -> Either String (T.Tensor (Complex a))
-- applyGate (T.Tensor xs) i (T.Tensor ys)
--     | T.rank x /= 2 || not (T.isSquare x) || numT == 0 = Left "Invalid Gate Tensor"
--     | T.rank y /= 1 || numV == 0 = Left "Invalid Statevector"
--     | i < 0 || i >= numV = Left "Index out of bounds"
--     | otherwise = Right $ build numV [(i, x)] pauliId * y
--     where
--         x = T.Tensor xs
--         y = T.Tensor ys
--         numT = fromRight 0 $ integralLog 2 (length xs)
--         numV = fromRight 0 $ integralLog 2 (length ys)
-- applyGate _ _ _ = Left "Input is not a Tensor"

-- applyGateAll :: RealFloat a => T.Tensor (Complex a) -> T.Tensor (Complex a) -> Either String (T.Tensor (Complex a))
-- applyGateAll (T.Tensor xs) (T.Tensor ys)
--     | T.rank x /= 2 || not (T.isSquare x) || numT == 0 = Left "Invalid Gate Tensor"
--     | T.rank y /= 1 || numV == 0 = Left "Invalid Statevector"
--     | otherwise = Right $ build numV [] x * y
--     where
--         x = T.Tensor xs
--         y = T.Tensor ys
--         numT = fromRight 0 $ integralLog 2 (length xs)
--         numV = fromRight 0 $ integralLog 2 (length ys)
-- applyGateAll _ _ = Left "Input is not a Tensor"

-- applyControl :: RealFloat a => T.Tensor (Complex a) -- ^ Gate tensor
--   -> Int -- ^ Control qubit index
--   -> Int -- ^ Apply qubit index
--   -> T.Tensor (Complex a) -- ^ State vector
--   -> Either String (T.Tensor (Complex a))
-- applyControl (T.Tensor xs) ctl i (T.Tensor ys)
--     | T.rank x /= 2 || not (T.isSquare x) || numT == 0 = Left "Invalid Gate Tensor"
--     | T.rank y /= 1 || numV == 0 = Left "Invalid Statevector"
--     | i < 0 || i >= numV = Left "Application qubit index out of bounds"
--     | ctl < 0 || ctl >= numV = Left "Control qubit index out of bounds"
--     | ctl == i = Left "Control and application qubit cannot be the same"
--     | otherwise = Right $ (build numV [(ctl, mask0)] pauliId
--         + build numV (sortBy (compare `on` fst) [(ctl, mask1), (i, x)]) pauliId) * y
--     where
--         x = T.Tensor xs
--         y = T.Tensor ys
--         numT = fromRight 0 $ integralLog 2 (length xs)
--         numV = fromRight 0 $ integralLog 2 (length ys)

-- applyControl _ _ _ _ = Left "Input is not a Tensor"