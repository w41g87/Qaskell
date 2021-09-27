import Test.QuickCheck
import Test.Hspec
import qualified Tensor as T

main :: IO ()
main = prop_mult

prop_mult = hspec $ do
    describe "Tensor Multiply" $ do
        it "t2 multiply by t3 is t4" $ do
            t2 * t3 `shouldBe` t4

t1 :: T.Tensor Int
t1 = T.Tensor[
       T.Tensor[
           T.Tensor[T.Scalar 1,T.Scalar 2,T.Scalar 3],
           T.Tensor[T.Scalar 4,T.Scalar 5,T.Scalar 6],
           T.Tensor[T.Scalar 7,T.Scalar 8,T.Scalar 9]
        ],T.Tensor[
           T.Tensor[T.Scalar 10,T.Scalar 11,T.Scalar 12],
           T.Tensor[T.Scalar 13,T.Scalar 14,T.Scalar 15],
           T.Tensor[T.Scalar 16,T.Scalar 17,T.Scalar 18]
        ],T.Tensor[
           T.Tensor[T.Scalar 19,T.Scalar 20,T.Scalar 21],
           T.Tensor[T.Scalar 22,T.Scalar 23,T.Scalar 24],
           T.Tensor[T.Scalar 25,T.Scalar 26,T.Scalar 27]
        ]
    ]

t2 :: T.Tensor Int
t2 = T.Tensor [
        T.Tensor [T.Scalar 12, T.Scalar 3, T.Scalar 9], 
        T.Tensor [T.Scalar 8, T.Scalar 17, T.Scalar 8], 
        T.Tensor [T.Scalar 4, T.Scalar 14, T.Scalar 10]
    ]

t3 :: T.Tensor Int
t3 = T.Tensor [
        T.Tensor [T.Scalar 5, T.Scalar 6, T.Scalar 7], 
        T.Tensor [T.Scalar 19, T.Scalar 15, T.Scalar 8], 
        T.Tensor [T.Scalar 3, T.Scalar 9, T.Scalar 16]
    ]

t4 :: T.Tensor Int
t4 = T.Tensor [
        T.Tensor [T.Scalar 136, T.Scalar 215, T.Scalar 163], 
        T.Tensor [T.Scalar 380, T.Scalar 424, T.Scalar 371], 
        T.Tensor [T.Scalar 172, T.Scalar 386, T.Scalar 259]
    ]

r4 :: T.Tensor Double
r4 = T.Tensor[
        T.Tensor[
            T.Tensor[
                T.Tensor[T.Scalar 1, T.Scalar 2, T.Scalar 3], 
                T.Tensor[T.Scalar 4, T.Scalar 5, T.Scalar 6], 
                T.Tensor[T.Scalar 7, T.Scalar 8, T.Scalar 9]
            ], T.Tensor[
                T.Tensor[T.Scalar 10, T.Scalar 11, T.Scalar 12], 
                T.Tensor[T.Scalar 13, T.Scalar 14, T.Scalar 15], 
                T.Tensor[T.Scalar 16, T.Scalar 17, T.Scalar 18]
            ], T.Tensor[
                T.Tensor[T.Scalar 19, T.Scalar 20, T.Scalar 21], 
                T.Tensor[T.Scalar 22, T.Scalar 23, T.Scalar 24], 
                T.Tensor[T.Scalar 25, T.Scalar 26, T.Scalar 27]
            ]
        ], T.Tensor[
            T.Tensor[
                T.Tensor[T.Scalar 28, T.Scalar 29, T.Scalar 30], 
                T.Tensor[T.Scalar 31, T.Scalar 32, T.Scalar 33], 
                T.Tensor[T.Scalar 34, T.Scalar 35, T.Scalar 36]
            ], T.Tensor[
                T.Tensor[T.Scalar 37, T.Scalar 38, T.Scalar 39], 
                T.Tensor[T.Scalar 40, T.Scalar 41, T.Scalar 42], 
                T.Tensor[T.Scalar 43, T.Scalar 44, T.Scalar 45]
            ], T.Tensor[
                T.Tensor[T.Scalar 46, T.Scalar 47, T.Scalar 48], 
                T.Tensor[T.Scalar 49, T.Scalar 50, T.Scalar 51], 
                T.Tensor[T.Scalar 52, T.Scalar 53, T.Scalar 54]
            ]
        ], T.Tensor[
            T.Tensor[
                T.Tensor[T.Scalar 55, T.Scalar 56, T.Scalar 57], 
                T.Tensor[T.Scalar 58, T.Scalar 59, T.Scalar 60], 
                T.Tensor[T.Scalar 61, T.Scalar 62, T.Scalar 63]
            ], T.Tensor[
                T.Tensor[T.Scalar 64, T.Scalar 65, T.Scalar 66], 
                T.Tensor[T.Scalar 67, T.Scalar 68, T.Scalar 69], 
                T.Tensor[T.Scalar 70, T.Scalar 71, T.Scalar 72]
            ], T.Tensor[
                T.Tensor[T.Scalar 73, T.Scalar 74, T.Scalar 75], 
                T.Tensor[T.Scalar 76, T.Scalar 77, T.Scalar 78], 
                T.Tensor[T.Scalar 79, T.Scalar 80, T.Scalar 81]
            ]
        ]
    ]