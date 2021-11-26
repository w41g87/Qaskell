{-# LANGUAGE QuantifiedConstraints, 
             TupleSections,
             DefaultSignatures,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             ConstraintKinds,
             MultiParamTypeClasses,
             TypeFamilies #-}

module Tensor where
import Data.Complex
import Helper
import EitherTrans
import Control.Monad.Trans.State
import Data.Monoid
import Data.List
import Data.Function
import System.Random
import System.Random.Stateful

-- orphan instance
-- sort complex numbers by their magnitudes
instance RealFloat a => Ord (Complex a) where
    compare a b = compare (magnitude a) (magnitude b)

instance (UniformRange a) => UniformRange (Complex a) where
    uniformRM (lr :+ li, hr :+ hi) g = do
        mr <- uniformRM (lr, hr) g 
        mi <- uniformRM (li, hi) g
        return $ mr :+ mi

instance (Uniform a) => Uniform (Complex a) where
    uniformM g = do
        r <- uniformM g
        i <- uniformM g
        return $ r :+ i
    

{-
    The definition of tensor product should follow conventional tensor product properties.
    Tensor product should form a monoid upto isomorphism
-}

-- higer-kinded definition
-- Polymorphic constraints

class Cmplx c where
    conj :: c -> c
    conj = error "please define conjugate function"

instance (Num a) => Cmplx (Complex a) where
    conj = conjugate

class (Monoid (t a), Cmplx a) => Tensor t a where
    (|*|) :: t a -> t a -> t a
    (|*|) = (<>)
    tconcat :: [t a] -> t a
    tconcat = mconcat
    norm :: Floating a => t a -> t a
    fromList :: Integral b => [a] -> b -> t a
    rank :: Integral b => t a -> b
    isSquare :: t a -> Bool
    conjTr :: t a -> t a
    inner :: t a -> t a -> t a

class (Tensor q a, Num (q a)) => QuantumRegister q a where

    densityOp :: q a
        -> q a

    densityOp x = x * conjTr x

    -- Builds a transformation tensor to apply input gate to selected qubit
    build :: (Integral b, Integral d) => b -- ^ Number of qubits this tensor applies to
        -> [(d, q a)]-- ^ Index / Gate pair (Pre-sorted)
        -> q a -- ^ Default Gate
        -> q a
    build i xs d = tconcat $ go i (sortOnFst xs) d 0
        where
            go i2 [] d2 n = replicate (fromIntegral i2 - n) d2
            go i2 (x:xs) d2 n = replicate (fromIntegral (fst x) - n) d2 ++ (snd x : go i2 xs d2 (fromIntegral $ fst x + 1))

    getProb :: Int -- ^ Index of the qubit
        -> q a -- ^ Qubits
        -> Either String (a, a) -- ^ Resulting Probability
    default getProb :: (Foldable q, Floating a) => Int -> q a -> Either String (a, a) -- ^ Resulting Probability
    getProb i q
        | i < 0 || i >= rank q = Left "Index out of bounds"
        | not (isSquare q) = Left "Invalid Quantum Register"
        | otherwise = do
            a <- applyGate mask0 i q
            b <- applyGate mask1 i q
            return (f a, f b)
        where
            f = getSum . foldMap (Sum . (\x -> conj x * x))

    initQubit :: a -- ^ Constant for |0>
        -> a -- ^ Constant for |1>
        -> Either String (q a) -- ^ Resulting Qubit

    initQubit0 :: q a -- ^ Initialize |0> qubit at z-direction

    initNumQubit0 :: (Integral b) => b -- ^ Number of qubits to initialize
        -> Either String (q a) -- ^ Initialized quantum register

    toQuantumRegister :: [a] -- ^ State vector array to convert
        -> Either String (q a) -- ^ Converted Quantum Register

    collapse :: Int -- ^ Index of the qubit
        -> Bool -- ^ Collapsed State
        -> q a -- ^ Qubits to collapse
        -> Either String (q a) -- ^ Resulting Tensor

    default collapse :: (Floating a) => Int -> Bool -> q a -> Either String (q a)
    collapse i s q
        | i < 0 || i >= rank q = Left "Index out of bounds"
        | not (isSquare q) = Left "Invalid state vector"
        | s = norm <$> applyGate mask1 i q
        | otherwise = norm <$> applyGate mask0 i q

    measure :: (Monad m, StatefulGen g m, UniformRange a) => Int -- ^ Index of the qubit
        -> g -- ^ generator
        -> StateT (q a) (EitherT String m) Bool -- ^ Resulting State Transformer

    default measure :: (Monad m, Num a, Ord a, StatefulGen g m, UniformRange a) => Int -- ^ Index of the qubit
        -> g -- ^ generator
        -> StateT (q a) (EitherT String m) Bool -- ^ Resulting State Transformer
    measure i g = StateT go
        where
            go q
                | i < 0 || i >= rank q = EitherT $ return $ Left "Index out of bounds"
                | not (isSquare q) = EitherT $ return $ Left "Invalid state vector"
                | otherwise = EitherT $ do
                    p <- uniformRM (0, 1) g
                    return $ do
                        t <- getProb i q
                        if p < fst t
                        then (False, ) <$> collapse i False q
                        else (True, ) <$> collapse i True q


    applyGate :: q a -- ^ Gate tensor
        -> Int -- ^ Index
        -> q a -- ^ Quantum Registers
        -> Either String (q a)
    applyGate a i b
        | not (isSquare a) = Left "Invalid Gate Tensor"
        | not (isSquare b) = Left "Invalid Statevector"
        | i < 0 || i >= rank b = Left "Index out of bounds"
        | otherwise = Right $ build (rank b) [(i, a)] pauliId * b


    applyGateAll :: q a -- ^ Gate tensor
        -> q a -- ^ Quantum Registers
        -> Either String (q a)

    applyGateAll a b
        | not (isSquare a) = Left "Invalid Gate Tensor"
        | not (isSquare b) = Left "Invalid Statevector"
        | otherwise = Right $ build (rank b) [] a * b

    applyControl :: q a -- ^ Gate tensor
        -> Int -- ^ Control qubit index
        -> Int -- ^ Apply qubit index
        -> q a -- ^ Quantum Registers
        -> Either String (q a)

    applyControl a ctl i b
        | not (isSquare a) = Left "Invalid gate tensor"
        | not (isSquare b) = Left "Invalid state vector"
        | i < 0 || i >= rank b = Left "Application qubit index out of bounds"
        | ctl < 0 || ctl >= rank b = Left "Control qubit index out of bounds"
        | ctl == i = Left "Control and application qubit cannot be the same"
        | otherwise = Right $ (build (rank b) [(ctl, mask0)] pauliId
            + build (rank b) (sortBy (compare `on` fst) [(ctl, mask1), (i, a)]) pauliId) * b

    illegalPeek :: q a
        -> String

    subSystem :: [Int] -> q a -> Either String (q a)

    isEntangled :: [Int] -> q a -> Either String Bool

    mask0 :: q a
    mask1 :: q a
    pauliId :: q a

class (QuantumRegister q a) => Gates q a where
    pauliX :: q a
    pauliY :: q a
    pauliZ :: q a
    hadamard :: q a



-- Num-only declaration

-- class (forall a. Num a => Monoid (t a)) => Tensor t where
--     (|*|) :: (Num a) => t a -> t a -> t a
--     (|*|) = (<>)
--     norm :: Floating a => t a -> t a
--     fromList :: Integral b => [a] -> b -> t a
--     rank :: Integral b => t a -> b
--     isSquare :: t a -> Bool

-- class (Tensor q, forall a. Num a => Num (q a)) => QuantumRegister q where
--     -- Builds a transformation tensor to apply input gate to selected qubit
--     build :: (RealFloat a, Integral b, Integral c) => b -- ^ Number of qubits this tensor applies to
--         -> [(c, q (Complex a))]-- ^ Index / Gate pair (Pre-sorted)
--         -> q (Complex a) -- ^ Default Gate
--         -> q (Complex a)
--     build i xs d = mconcat $ go i (sortOnFst xs) d 0
--         where
--             go i2 [] d2 n = replicate (fromIntegral i2 - n) d2
--             go i2 (x:xs) d2 n = replicate (fromIntegral (fst x) - n) d2 ++ (snd x : go i2 xs d2 (fromIntegral $ fst x + 1))

--     getProb :: (RealFloat a) => Int -- ^ Index of the qubit
--         -> q (Complex a) -- ^ Qubits
--         -> Either String (a, a) -- ^ Resulting Probability

--     initQubit :: (RealFloat a) => Complex a -- ^ Constant for |0>
--         -> Complex a -- ^ Constant for |1>
--         -> Either String (q (Complex a)) -- ^ Resulting Qubit

--     initQubit0 :: (RealFloat a) => q (Complex a) -- ^ Initialize |0> qubit at z-direction

--     initNumQubit0 :: (RealFloat a, Integral b) => b -- ^ Number of qubits to initialize
--         -> Either String (q (Complex a)) -- ^ Initialized quantum register

--     toQuantumRegister :: (RealFloat a) => [Complex a] -- ^ State vector array to convert
--         -> Either String (q (Complex a)) -- ^ Converted Quantum Register

--     collapse :: (RealFloat a) => Int -- ^ Index of the qubit
--         -> Complex a -- ^ Collapsed State
--         -> q (Complex a) -- ^ Qubits to collapse
--         -> EitherT String IO (q (Complex a)) -- ^ Resulting Tensor

--     measure :: (RealFloat a, Monad m) => Int -- ^ Index of the qubit
--         -> StateT (q (Complex a)) (EitherT String m) (Complex a)
--     -- -> q (Complex a) -- ^ Qubits
--     -- -> Either String ( m (Complex a, q (Complex a))) -- ^ Resulting measurement and the collapsed qubits


--     pauliX :: (RealFloat a) => q (Complex a)
--     pauliY :: (RealFloat a) => q (Complex a)
--     pauliZ :: (RealFloat a) => q (Complex a)
--     pauliId :: (RealFloat a) => q (Complex a)
--     hadamard :: (RealFloat a) => q (Complex a)
--     mask0 :: (RealFloat a) => q (Complex a)
--     mask1 :: (RealFloat a) => q (Complex a)