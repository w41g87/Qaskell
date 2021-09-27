module QuantumCircuit where

import Data.Complex
import Data.Either
import Data.List
import Data.Function
import Control.Applicative
import Data.Bifunctor
import Data.Monoid
import System.Random
import Control.Monad.IO.Class

import Helper

import qualified Tensor as T

mask0 :: (RealFloat a) => T.Tensor (Complex a)
mask0 = T.Tensor[
            T.Tensor[T.Scalar (1 :+ 0), T.Scalar (0 :+ 0)],
            T.Tensor[T.Scalar (0 :+ 0), T.Scalar (0 :+ 0)]
        ]

mask1 :: (RealFloat a) => T.Tensor (Complex a)
mask1 = T.Tensor[
            T.Tensor[T.Scalar (0 :+ 0), T.Scalar (0 :+ 0)],
            T.Tensor[T.Scalar (0 :+ 0), T.Scalar (1 :+ 0)]
        ]

initQubit :: (RealFloat a, T.Tensor t) => Complex a -> Complex a -> Either String (t (Complex a))
initQubit zero one
    | magnitude zero ** 2 + magnitude one ** 2 == 1 = Right $ T.fromList [zero, one] 2
    | otherwise = Left "Sum of probability of |0> and |1> state does not equal to 1"

initQubit0 :: (RealFloat a) => T.Tensor (Complex a)
initQubit0 = T.fromList [1, 0] 2

initState :: (RealFloat a) => [Complex a] -> Either String (T.Tensor (Complex a))
initState q
    | isLeft num = Left "Input list length is not a power of 2"
    | otherwise = Right . T.norm $ T.fromList q 2
    where num = integralLog 2 (length q)

initNumQubits :: RealFloat a => Int -> Either String (T.Tensor (Complex a))
initNumQubits n
    | n < 1 = Left "Invalid qubit number"
    | otherwise = Right $ foldr (|*|) initQubit0 $ replicate (n-1) initQubit0

-- Builds a transformation tensor to apply input gate to selected qubit
build :: (RealFloat a, T.Tensor t) => Int -- ^ Number of qubits this tensor applies to
  -> [(Int, t (Complex a))]-- ^ Index / Gate pair (Pre-sorted)
  -> t (Complex a)-- ^ Default Gate
  -> t (Complex a)
build len lst dft = foldr (|*|) mempty (go len lst 0)
    where
        go l (x:xs) offset
            | fst x >= l = go l [] offset
            | otherwise = replicate (fst x - offset) dft ++ [snd x] ++ go l xs (fst x + 1)
        go l [] offset = replicate (l - offset) dft


applyGate :: RealFloat a => T.Tensor (Complex a) -- ^ Gate tensor
  -> Int -- ^ Index
  -> T.Tensor (Complex a) -- ^ State vector
  -> Either String (T.Tensor (Complex a))
applyGate x i y
    | not (T.isSquare x) = Left "Invalid Gate Tensor"
    | not (T.isSquare y) = Left "Invalid Statevector"
    | i < 0 || i >= T.rank y = Left "Index out of bounds"
    | otherwise = Right $ T.norm $ build (T.rank y) [(i, x)] pauliId * y


applyGateAll :: RealFloat a => T.Tensor (Complex a) -> T.Tensor (Complex a) -> Either String (T.Tensor (Complex a))
applyGateAll x y
    | not (T.isSquare x) = Left "Invalid Gate Tensor"
    | not (T.isSquare y) = Left "Invalid Statevector"
    | otherwise = Right $ T.norm $ build (T.rank y) [] x * y


applyControl :: RealFloat a => T.Tensor (Complex a) -- ^ Gate tensor
  -> Int -- ^ Control qubit index
  -> Int -- ^ Apply qubit index
  -> T.Tensor (Complex a) -- ^ State vector
  -> Either String (T.Tensor (Complex a))
applyControl x ctl i y
    | not (T.isSquare x) = Left "Invalid gate tensor"
    | not (T.isSquare y) = Left "Invalid state vector"
    | i < 0 || i >= T.rank y = Left "Application qubit index out of bounds"
    | ctl < 0 || ctl >= T.rank y = Left "Control qubit index out of bounds"
    | ctl == i = Left "Control and application qubit cannot be the same"
    | otherwise = Right $ T.norm $ (build (T.rank y) [(ctl, mask0)] pauliId
        + build (T.rank y) (sortBy (compare `on` fst) [(ctl, mask1), (i, x)]) pauliId) * y

getProb :: RealFloat a => Int -> T.Tensor (Complex a) -> Either String (a, a)
getProb i v
    | i < 0 || i >= T.rank v = Left "Index out of bounds"
    | not (T.isSquare v) = Left "Invalid state vector"
    | otherwise = Right $ bimap (getSum.foldMap (Sum . (**2) . magnitude))
        (getSum.foldMap (Sum . (**2) . magnitude)) (go i v)
    where
        go 0 v = (T.head v `T.con` T.Tensor [], T.last v `T.con` T.Tensor [])
        go i v = bimap
          (T.append (fst (go (i - 1) (T.head v))))
          (T.append (snd (go (i - 1) (T.head v)))) (go (i - 1) (T.last v))

collapse :: RealFloat a => Int -> Bool -> T.Tensor (Complex a) -> Either String (T.Tensor (Complex a))
collapse i state v
    | i < 0 || i >= T.rank v = Left "Index out of bounds"
    | not (T.isSquare v) = Left "Invalid state vector"
    | otherwise = Right . T.norm $ go i v
    where
        go 0 v = if state then T.Tensor [fmap (const 0) (T.head v), T.last v]
            else T.Tensor [T.head v, fmap (const 0) (T.last v)]
        go i v = T.Tensor [go (i - 1) (T.head v), go (i - 1) (T.last v)]

measure :: (RealFloat a, Random a, MonadIO m) => Int -> Either String (T.Tensor (Complex a)) -> m (Either String (Bool, T.Tensor (Complex a)))
measure i v = do
        a <- randomRIO (0.0, 1.0)
        if a < fst p then
            return $ fmap (\x -> (False, x)) (collapse i False =<< v)
        else
            return $ fmap (\x -> (True, x)) (collapse i True =<< v)
    where
        prob = getProb i =<< v
        p = fromRight (0.0, 0.0) prob