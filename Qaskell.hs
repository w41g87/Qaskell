module Qaskell where

import Data.Complex
import Data.Either
import Data.List
import Control.Applicative

newtype QuantumRegisters = QuantumRegisters [Complex Double]
    deriving (Eq, Show)

newtype QuantumGate = QuantumGate [[Complex Double]]
    deriving (Eq, Show)

{-| 
    intergerLog2 determines if an integer is a power of 2
    If it is, the log base 2 of the integer will be wrapped
    by Right constructor; if not, Left constructor with
    corresponding error will be returned.
    The function takes 1 argument of type 'Int'
-}
intergerLog2 :: Int -> Either String Int
intergerLog2 x
    | x < 1 = Left "Input less than 0"
    | x == 1 = Right 0
    | y * 2 == x = fmap (+1) $ intergerLog2 y
    | otherwise = Left "Input is not a power of 2"
    where 
        y = quot x 2

initQubit :: Complex Double -> Complex Double -> Either String QuantumRegisters
initQubit a b
    | magnitude a ** 2 + magnitude b ** 2 == 1 = Right $ QuantumRegisters [a, b]
    | otherwise = Left "Magnitude Error"

initQubit0 :: QuantumRegisters
initQubit0 = QuantumRegisters [(1:+0), (0:+0)]

initNumQubits :: Int -> Either String QuantumRegisters
initNumQubits n
    | n < 1 = Left "Number of Qubits Must be Greater than 1"
    | otherwise = Right $ foldr mergeCircuit (QuantumRegisters [1 :+ 0]) $ replicate n initQubit0

initState :: [Complex Double] -> Either String QuantumRegisters
initState q
    | isLeft numQubits = Left "Input State Dimension Invalid"
    | otherwise = Right $ QuantumRegisters q
    where
        numQubits = intergerLog2 $ length q

mergeCircuit :: QuantumRegisters -> QuantumRegisters -> QuantumRegisters
mergeCircuit (QuantumRegisters x) (QuantumRegisters y) = QuantumRegisters $ liftA2 (*) x y

mergeGate :: QuantumGate -> QuantumGate -> QuantumGate
mergeGate (QuantumGate x) (QuantumGate y) = QuantumGate $ (fmap (<*>) (fmap (fmap (*)) x)) <*> y

tensorAdd :: QuantumGate -> QuantumGate -> Either String QuantumGate
tensorAdd (QuantumGate i) (QuantumGate j)
    | m == x && n == y = Right $ QuantumGate $ zipWith (zipWith (+)) i j
    | otherwise = Left "Adding Two Tensors with Different Dimensions"
    where
        m = length i
        n = length $ head i
        x = length j
        y = length $ head j

doubleBindEither :: Either String QuantumGate -> Either String QuantumGate -> (QuantumGate -> QuantumGate -> Either String QuantumGate) -> Either String QuantumGate
doubleBindEither x y f
    | isLeft x && isRight y = Left $ (fromLeft [] x) ++ "\n" ++ (fromLeft [] y)
    | isLeft x = x
    | isLeft y = y
    | otherwise = f (fromRight (QuantumGate [[]]) x) (fromRight (QuantumGate [[]]) y)

applyGate :: QuantumGate -> QuantumRegisters -> Either String QuantumRegisters
applyGate (QuantumGate g) (QuantumRegisters q)
    | length f == length q && length q == length g = 
        Right $ QuantumRegisters $ foldr (zipWith (+)) acc $ zipWith(fmap) (fmap (*) q) f
    | otherwise = Left "Transformation Matrix Dimension Mismatch"
    where
        acc = replicate (length q) $0:+0
        f = transpose g

-- The qubits are 1-indexed
applyGateToQubit :: QuantumGate -> Int -> QuantumRegisters -> Either String QuantumRegisters
applyGateToQubit g n (QuantumRegisters q) = transTensor >>= (flip applyGate (QuantumRegisters q))
    where
        numQubits = intergerLog2 $ length q
        transTensor = numQubits >>= (flip (genTensor g) n)

-- Gate to insert -> Number of Qubits -> Index of Qubit
genTensor :: QuantumGate -> Int -> Int -> Either String QuantumGate
genTensor g n i
    | n < i || i < 1 = Left "Index Out of Range"
    | otherwise = Right $ foldr mergeGate (QuantumGate [[1:+0]]) expGate
    where
        expGate = fstId ++ [g] ++ lstId
        fstId = replicate (i - 1) pauliId
        lstId = replicate (n - i) pauliId

getQubitState :: QuantumRegisters -> Int -> Either String (Double, Double)
getQubitState (QuantumRegisters q) x 
    | x < 1 = Left "Index Out of Bounds"
    | otherwise = Right (zeroP, oneP)
    where 
        numRow = 2 ^ (x - 1)
        numQubits = intergerLog2 $ length q
        q2 = fmap (** 2) q
        zeroP = magnitude $ foldr (+) 0 $ takeHalf q2 numRow
        oneP = magnitude $ foldr (+) 0 $ takeHalf (drop numRow q2) numRow
        takeHalf :: [Complex Double] -> Int -> [Complex Double]
        takeHalf xs n
            | length xs < n = []
            | otherwise = (take n xs) ++ (takeHalf (drop (2 * n) xs) n)

-- Gate to Apply -> Control Bit Index -> Application Bit Index -> Circuit To Apply
applyControlToQubits :: QuantumGate -> Int -> Int -> QuantumRegisters -> Either String QuantumRegisters
applyControlToQubits g m n (QuantumRegisters q)
    | m < n = (transTensor m n mask1 g) >>= (flip applyGate (QuantumRegisters q))
    | m > n = (transTensor n m g mask1) >>= (flip applyGate (QuantumRegisters q))
    | otherwise = Left "Control and Application Qubit Cannot be Identical"
    where
        zeroMask = numQubits >>= (flip (genTensor mask0) m)
        numQubits = intergerLog2 $ length q
        transTensor :: Int -> Int -> QuantumGate -> QuantumGate -> Either String QuantumGate
        transTensor i j fstGate sndGate = doubleBindEither ((fmap mergeGate fstTensor) <*> sndTensor) zeroMask tensorAdd
            where
                fstTensor = genTensor fstGate i i
                sndTensor = (fmap (flip (-) i) numQubits) >>= (flip (genTensor sndGate) (j - i))

pauliX :: QuantumGate
pauliX = QuantumGate [[0 :+ 0, 1 :+ 0],
                      [1 :+ 0, 0 :+ 0]]

pauliY :: QuantumGate
pauliY = QuantumGate [[0 :+ 0, 0 :+ (-1)],
                      [0 :+ 1, 0 :+ 0]]

pauliZ :: QuantumGate
pauliZ = QuantumGate [[1 :+ 0, 0 :+ 0],
                      [0 :+ 0, (-1) :+ 0]]

pauliId :: QuantumGate
pauliId = QuantumGate [[1 :+ 0, 0 :+ 0],
                      [0 :+ 0, 1 :+ 0]]

hadamard :: QuantumGate
hadamard = QuantumGate [[1 / sqrt 2 :+ 0, 1 / sqrt 2 :+ 0],
                        [1 / sqrt 2 :+ 0, (-1) / sqrt 2 :+ 0]]

mask0 :: QuantumGate
mask0 = QuantumGate [[1 :+ 0, 0 :+ 0],
                    [0 :+ 0, 0 :+ 0]]

mask1 :: QuantumGate
mask1 = QuantumGate [[0 :+ 0, 0 :+ 0],
                    [0 :+ 0, 1 :+ 0]]

main :: IO()
main = do print result
    where q0 = initQubit0
          crt = mergeCircuit q0 q0
          flip1 = applyGateToQubit hadamard 1 crt
          result = flip1 >>= (applyControlToQubits pauliX 1 2)