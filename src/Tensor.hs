{-# LANGUAGE QuantifiedConstraints, 
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

{-
    The definition of tensor product should follow conventional tensor product properties.
-}

-- higer-kinded definition
-- Polymorphic constraints

class Cmplx c where
    conj :: c -> c
    conj = error "please define conjugate function"

instance {-# OVERLAPPABLE #-} (Num a) => Cmplx a where
    conj = id

instance {-# OVERLAPPING #-} (Num a) => Cmplx (Complex a) where
    conj = conjugate

class (Monoid (t a), Cmplx (t a)) => Tensor t a where
    (|*|) :: t a -> t a -> t a
    (|*|) = (<>)
    tconcat :: [t a] -> t a
    tconcat = mconcat
    norm :: Floating a => t a -> t a
    fromList :: Integral b => [a] -> b -> t a
    rank :: Integral b => t a -> b
    isSquare :: t a -> Bool
    conjTr :: t a -> t a
    conjTr = conj

class (Tensor q a, Num (q a)) => QuantumRegister q a where

    densityOp :: q a
        -> q a

    densityOp x = x * conj x

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

    initQubit :: a -- ^ Constant for |0>
        -> a -- ^ Constant for |1>
        -> Either String (q a) -- ^ Resulting Qubit

    initQubit0 :: q a -- ^ Initialize |0> qubit at z-direction

    initNumQubit0 :: (Integral b) => b -- ^ Number of qubits to initialize
        -> Either String (q a) -- ^ Initialized quantum register

    toQuantumRegister :: [a] -- ^ State vector array to convert
        -> Either String (q a) -- ^ Converted Quantum Register

    collapse :: Int -- ^ Index of the qubit
        -> a -- ^ Collapsed State
        -> q a -- ^ Qubits to collapse
        -> EitherT String IO (q a) -- ^ Resulting Tensor

    measure :: (Monad m) => Int -- ^ Index of the qubit
        -> StateT (q a) (EitherT String m) a -- ^ Resulting State Transformer

    applyGate :: q a -- ^ Gate tensor
        -> Int -- ^ Index
        -> q a -- ^ Quantum Registers
        -> Either String (q a)

    applyGateAll :: q a -- ^ Gate tensor
        -> q a -- ^ Quantum Registers
        -> Either String (q a)

    applyControl :: q a -- ^ Gate tensor
        -> Int -- ^ Control qubit index
        -> Int -- ^ Apply qubit index
        -> q a -- ^ Quantum Registers
        -> Either String (q a)

    illegalPeek :: q a
        -> String

class (QuantumRegister q a) => Gates q a where
    pauliX :: q a
    pauliY :: q a
    pauliZ :: q a
    pauliId :: q a
    hadamard :: q a
    mask0 :: q a
    mask1 :: q a


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