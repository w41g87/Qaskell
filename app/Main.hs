module Main where

import Vector
import Tensor
import Legacy.Base

x = Column [Scalar 0.5, Scalar 0.5] :: Vector Double

func = norm x

main :: IO ()
main = putStrLn "The main function does nothing for now"
    
