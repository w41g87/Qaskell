module Braket where

data Braket a = Bra Bool | Ket Bool | Operator Char | Apply (Braket a) (Braket a)