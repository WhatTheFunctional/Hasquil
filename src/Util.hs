--Utility functions
--Copyright Laurence Emms 2018

module Util (loadIntToRegisters,
             addRegisters) where

import Data.Complex
import Data.Bits
import Data.List
import Register
import Instruction
import ClassicalCircuit
import QuantumCircuit

loadIntToRegisters :: (Floating a, Show a, Ord a) => Int -> Int -> [Instruction a]
loadIntToRegisters x r = loadIntBit x r 0 []

loadIntBit :: (Floating a, Show a, Ord a) => Int -> Int -> Int -> [Instruction a] -> [Instruction a]
loadIntBit x r b commands
    | b < 32 = if (testBit x b)
               then (loadIntBit x r (b + 1) (commands ++ [(ITrue (Register (r + b)))]))
               else (loadIntBit x r (b + 1) (commands ++ [(IFalse (Register (r + b)))]))
    | otherwise = commands

addRegisters :: (Floating a, Show a, Ord a) => Int -> Int -> Int -> Int -> Int -> [Instruction a]
addRegisters carry temp a b r
    = (IFalse (Register carry)) :
      (IFalse (Register temp)) :
      (fmap (\(x, y, z) -> (CallCircuit adder
                                       [Right (Register carry),
                                        Right (Register temp),
                                        Right (Register x),
                                        Right (Register y),
                                        Right (Register z)]))
                                       (zip3 [a..(a + 31)] [b..(b + 31)] [r..(r + 31)]))

ifC :: (Floating a, Show a, Ord a) => String -> String -> Classical a -> [Instruction a] -> [Instruction a] -> [Instruction a]
ifC thenLabel endLabel c x y = (JumpWhen thenLabel c) :
                               y ++
                               [(Jump endLabel),
                                (Label thenLabel)] ++
                               x ++
                               [(Label endLabel)]

whileC :: (Floating a, Show a, Ord a) => String -> String -> Classical a -> [Instruction a] -> [Instruction a]
whileC conditionLabel loopLabel c x = [(Jump conditionLabel),
                                       (Label loopLabel)] ++
                                      x ++
                                      [(Label conditionLabel),
                                       (JumpWhen loopLabel c)]

doWhileC :: (Floating a, Show a, Ord a) => String -> Classical a -> [Instruction a] -> [Instruction a]
doWhileC whileLabel c x = (Label whileLabel) :
                          x ++
                          [(JumpWhen whileLabel c)]
