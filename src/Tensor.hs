{-# LANGUAGE QuantifiedConstraints, 
             TypeFamilies #-}

module Tensor where
import Data.Maybe (fromMaybe)
import GHC.Float (RealFloat)
import Data.Complex
import Helper
import EitherTrans
import GHC.Exts (Constraint)

{-
    The definition of tensor product should follow conventional tensor product properties.
-}

-- class Monoid t => Tensor t where
--     (|*|) :: t -> t -> t
--     x |*| y = x <> y
--     norm :: t -> t
--     fromList :: Integral b => [a] -> b -> t
--     rank :: Integral b => t -> b
--     isSquare :: t -> Bool

-- class Tensor q => QuantumRegister q where
--     -- Builds a transformation tensor to apply input gate to selected qubit
--     build :: (Integral a, Integral b) => a -- ^ Number of qubits this tensor applies to
--         -> [(b, q)]-- ^ Index / Gate pair (Pre-sorted)
--         -> q -- ^ Default Gate
--         -> q
--     build i xs d = mconcat $ go i (sortOnFst xs) d 0
--         where
--             go i2 [] d2 n = replicate (fromIntegral i2 - n) d2
--             go i2 (x:xs) d2 n = replicate (fromIntegral (fst x) - n) d2 ++ (snd x : go i2 xs d2 (fromIntegral $ fst x + 1))

--     getProb :: Int -- ^ Index of the qubit
--         -> q -- ^ Qubits
--         -> Either String (Double, Double) -- ^ Resulting Probability

--     initQubit :: (RealFloat a) => a -- ^ Constant for |0>
--         -> a -- ^ Constant for |1>
--         -> Either String q -- ^ Resulting Qubit

--     initQubit0 :: q -- ^ Initialize |0> qubit at z-direction

--     initNumQubit0 :: (Integral a) => a -- ^ Number of qubits to initialize
--         -> Either String q -- ^ Initialized quantum register

--     toQuantumRegister :: (RealFloat a) => [Complex a] -- ^ State vector array to convert
--         -> Either String q -- ^ Converted Quantum Register

--     collapse :: Int -- ^ Index of the qubit
--         -> Bool -- ^ Collapsed State
--         -> q -- ^ Qubits to collapse
--         -> EitherT String IO q -- ^ Resulting Tensor

--     pauliX :: q
--     pauliY :: q
--     pauliZ :: q
--     pauliId :: q
--     hadamard :: q
--     mask0 ::q
--     mask1 :: q


-- higer-kinded definition

class (forall a. Num a => Monoid (t a)) => Tensor t where
    (|*|) :: (Num a) => t a -> t a -> t a
    x |*| y = x <> y
    norm :: Floating a => t a -> t a
    fromList :: Integral b => [a] -> b -> t a
    rank :: Integral b => t a -> b
    isSquare :: t a -> Bool

class (Tensor q, forall a. Num a => Num (q a)) => QuantumRegister q where
    -- Builds a transformation tensor to apply input gate to selected qubit
    build :: (RealFloat a, Integral b, Integral c) => b -- ^ Number of qubits this tensor applies to
        -> [(c, q (Complex a))]-- ^ Index / Gate pair (Pre-sorted)
        -> q (Complex a)-- ^ Default Gate
        -> q (Complex a)
    build i xs d = mconcat $ go i (sortOnFst xs) d 0
        where
            go i2 [] d2 n = replicate (fromIntegral i2 - n) d2
            go i2 (x:xs) d2 n = replicate (fromIntegral (fst x) - n) d2 ++ (snd x : go i2 xs d2 (fromIntegral $ fst x + 1))

    getProb :: (RealFloat a) => Int -- ^ Index of the qubit
        -> q (Complex a) -- ^ Qubits
        -> Either String (a, a) -- ^ Resulting Probability

    initQubit :: (RealFloat a) => Complex a -- ^ Constant for |0>
        -> Complex a -- ^ Constant for |1>
        -> Either String (q (Complex a)) -- ^ Resulting Qubit

    initQubit0 :: (RealFloat a) => q (Complex a) -- ^ Initialize |0> qubit at z-direction

    initNumQubit0 :: (RealFloat a, Integral b) => b -- ^ Number of qubits to initialize
        -> Either String (q (Complex a)) -- ^ Initialized quantum register

    toQuantumRegister :: (RealFloat a) => [Complex a] -- ^ State vector array to convert
        -> Either String (q (Complex a)) -- ^ Converted Quantum Register

    collapse :: (RealFloat a) => Int -- ^ Index of the qubit
        -> Bool -- ^ Collapsed State
        -> q (Complex a) -- ^ Qubits to collapse
        -> EitherT String IO (q (Complex a)) -- ^ Resulting Tensor

    pauliX :: (RealFloat a) => q (Complex a)
    pauliY :: (RealFloat a) => q (Complex a)
    pauliZ :: (RealFloat a) => q (Complex a)
    pauliId :: (RealFloat a) => q (Complex a)
    hadamard :: (RealFloat a) => q (Complex a)
    mask0 :: (RealFloat a) => q (Complex a)
    mask1 :: (RealFloat a) => q (Complex a)