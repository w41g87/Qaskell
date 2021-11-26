module Main where

import Vector
import Tensor
import Data.Complex
import System.Random.Stateful
import EitherTrans
import Control.Monad.Trans.State

func i n c = do
        s <- newIOGenM $ mkStdGen 42
        let sT = measure i s
        result <- runEither $ (EitherT . return) x1 >>= execStateT sT
        let result2 = x1 >>= isEntangled [c]
        let result3 = x2 >>= isEntangled [c]
        --either putStrLn (putStrLn . illegalPeek) result3
        either putStrLn print result2
        either putStrLn print result3
    where
        x = initNumQubit0 n :: Either String (Vector (Complex Double))
        x0 = x >>= applyGate hadamard 0
        x1 = x0 >>= applyControl pauliX 0 1
        x2 = x >>= applyGateAll hadamard

main = func 1 2 0
