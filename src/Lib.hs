--Haskell Quil compiler
--Copyright Laurence Emms 2018

module Lib (compile) where

import Data.Complex
import Register
import Instruction

testCircuit :: (Floating a, Show a, Ord a) => Circuit a
testCircuit = let parameters@[Left a, Left b] = [Left (MetaQubitRegister "a"), Left (MetaQubitRegister "b")]
                  instructions = [(Hadamard a),
                                  (CNot a b)]
              in Circuit "BELL" parameters instructions

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
          putStrLn (show $ DefCircuit testCircuit) >>
          putStrLn (show $ CallCircuit testCircuit [Left (QubitRegister 5), Left (QubitRegister 3)])
