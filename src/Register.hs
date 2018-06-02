--Hasquil registers
--Copyright Laurence Emms 2018

module Register (Quantum(..),
                 Classical(..)) where

import Data.Complex

data Quantum = QubitRegister Int |
               MetaQubitRegister String

instance Show Quantum where
    show (QubitRegister i) = show i
    show (MetaQubitRegister s) = s

data Classical a = Register Int |
                   Range Int Int |
                   RealConstant a |
                   ComplexConstant (Complex a) |
                   MetaRegister String

instance (Floating a, Show a, Ord a) => Show (Classical a) where
    show (Register i) = "[" ++ (show i) ++ "]"
    show (Range i j) = "[" ++ (show i) ++ "-" ++ (show j) ++ "]"
    show (RealConstant r) = show r
    show (ComplexConstant (p :+ q))
        | q >= 0 = (show p) ++ "+" ++ (show q) ++ "i"
        | otherwise = (show p) ++ (show q) ++ "i"
    show (MetaRegister s) = s
