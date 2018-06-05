--Haskell Quil compiler
--Copyright Laurence Emms 2018

module Lib (compile) where

import Data.Complex
import Register
import Instruction
import ClassicalCircuit
import QuantumCircuit

testRXCircuit :: (Floating a, Show a, Ord a) => Circuit a
testRXCircuit = let parameters@[Left a] = [Left (MetaQubitRegister "a")]
                    instructions = [(RX (ComplexConstant (5.0 :+ 10.0)) a)]
                in Circuit "TESTRX" parameters instructions

compile :: IO ()
compile = putStrLn "Compiling quantum executable" >>
          putStrLn (show $ QubitRegister 10) >>
          putStrLn (show $ Register 5) >>
          putStrLn (show $ ComplexConstant (6.0 :+ 7.0)) >>
          putStrLn (show $ ComplexConstant ((-8.0) :+ 1.0)) >>
          putStrLn (show $ ComplexConstant (2.0 :+ (-3.0))) >>
          putStrLn (show $ ComplexConstant ((-4.0) :+ (-6.0))) >>
          putStrLn (show $ CNot (QubitRegister 0) (QubitRegister 1)) >>
          putStrLn (show $ PSwap (ComplexConstant (5.0 :+ (-3.2))) (QubitRegister 0) (QubitRegister 1)) >>
          putStrLn (show $ Measure (QubitRegister 4)) >>
          putStrLn (show $ MeasureOut (QubitRegister 4) (Register 5)) >>
          putStrLn (show $ DefCircuit bell) >>
          putStrLn (show $ CallCircuit bell [Left (QubitRegister 5), Left (QubitRegister 3)]) >>
          putStrLn (show $ DefCircuit testRXCircuit) >>
          putStrLn (show $ DefCircuit (xor "xor0")) >>
          putStrLn (show $ CallCircuit (xor "xor0") [Right (Register 0), Right (Register 1), Right (Register 2)])
