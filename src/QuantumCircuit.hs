module QuantumCircuit where

import Data.Complex
import Data.Either
import Data.List
import Data.Function
import System.Random
import Control.Monad.IO.Class

import Helper

import Tensor



applyGate :: (QuantumRegister q, RealFloat a) => q (Complex a) -- ^ Gate tensor
  -> Int -- ^ Index
  -> q (Complex a) -- ^ State vector
  -> Either String (q (Complex a))
applyGate x i y
    | not (isSquare x) = Left "Invalid Gate Tensor"
    | not (isSquare y) = Left "Invalid Statevector"
    | i < 0 || i >= rank y = Left "Index out of bounds"
    | otherwise = Right $ norm $ build (rank y) [(i, x)] pauliId * y


applyGateAll :: (QuantumRegister q, RealFloat a) => q (Complex a) -> q (Complex a) -> Either String (q (Complex a))
applyGateAll x y
    | not (isSquare x) = Left "Invalid Gate Tensor"
    | not (isSquare y) = Left "Invalid Statevector"
    | otherwise = Right $ norm $ build (rank y) [] x * y


applyControl :: (QuantumRegister q, RealFloat a) => q (Complex a) -- ^ Gate tensor
  -> Int -- ^ Control qubit index
  -> Int -- ^ Apply qubit index
  -> q (Complex a) -- ^ State vector
  -> Either String (q (Complex a))
applyControl x ctl i y
    | not (isSquare x) = Left "Invalid gate tensor"
    | not (isSquare y) = Left "Invalid state vector"
    | i < 0 || i >= rank y = Left "Application qubit index out of bounds"
    | ctl < 0 || ctl >= rank y = Left "Control qubit index out of bounds"
    | ctl == i = Left "Control and application qubit cannot be the same"
    | otherwise = Right $ norm $ (build (rank y) [(ctl, mask0)] pauliId
        + build (rank y) (sortBy (compare `on` fst) [(ctl, mask1), (i, x)]) pauliId) * y

collapse :: (RealFloat a, QuantumRegister q) => Int -- ^ Index of the qubit
        -> Bool -- ^ Collapsed State
        -> q (Complex a) -- ^ Qubits to collapse
        -> Either String (q (Complex a)) -- ^ Resulting Tensor
collapse i state q
    | i < 0 || i >= rank q = Left "Index out of bounds"
    | not (isSquare q) = Left "Invalid state vector"
    | state = norm <$> applyGate mask1 i q
    | otherwise = norm <$> applyGate mask0 i q

-- measure :: (RealFloat a, Monad m) => Int -- ^ Index of the qubit
--     -> q (Complex a) -- ^ Qubits
--     -> Either String ( m (Bool, q (Complex a))) -- ^ Resulting measurement and the collapsed qubits
-- measure i v = do
--     a <- randomRIO (0.0, 1.0)
--     if a < fst p then
--         return $ fmap (\x -> (False, x)) (collapse i False =<< v)
--     else
--         return $ fmap (\x -> (True, x)) (collapse i True =<< v)
--     where
--         prob = getProb i =<< v
--         p = fromRight (0.0, 0.0) prob