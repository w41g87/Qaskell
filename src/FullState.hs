module FullState where

import Data.Complex
import qualified Tensor as T
import qualified Vector as V

pauliX :: (RealFloat a) => V.Vector (Complex a)
pauliX = V.Row [
            V.Column [V.Scalar (0 :+ 0), V.Scalar (1 :+ 0)],
            V.Column [V.Scalar (1 :+ 0), V.Scalar (0 :+ 0)]
        ]

pauliY :: (RealFloat a) => V.Vector (Complex a)
pauliY = V.Row [
            V.Column [V.Scalar (0 :+ 0), V.Scalar (0 :+ 1)],
            V.Column [V.Scalar (0 :+ (-1)), V.Scalar (0 :+ 0)]
        ]

pauliZ :: (RealFloat a) => V.Vector (Complex a)
pauliZ = V.Row [
            V.Column [V.Scalar (1 :+ 0), V.Scalar (0 :+ 0)],
            V.Column [V.Scalar (0 :+ 0), V.Scalar ((-1) :+ 0)]
        ]

pauliId :: (RealFloat a) => V.Vector (Complex a)
pauliId = V.Row [
            V.Column [V.Scalar (1 :+ 0), V.Scalar (0 :+ 0)],
            V.Column [V.Scalar (0 :+ 0), V.Scalar (1 :+ 0)]
        ]

hadamard :: (RealFloat a) => V.Vector (Complex a)
hadamard = V.Row [
            V.Column [V.Scalar (1 / sqrt 2 :+ 0), V.Scalar (1 / sqrt 2 :+ 0)],
            V.Column [V.Scalar (1 / sqrt 2 :+ 0), V.Scalar ((-1) / sqrt 2 :+ 0)]
        ]


        