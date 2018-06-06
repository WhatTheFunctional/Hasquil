--Haskell Quil compiler
--Copyright Laurence Emms 2018

module Lib (compile) where

import Data.Complex
import Data.List
import Register
import Instruction
import ClassicalCircuit
import QuantumCircuit
import Util

testRXCircuit :: (Floating a, Show a, Ord a) => Circuit a
testRXCircuit = let parameters@[Left a] = [Left (MetaQubitRegister "a")]
                    instructions = [(RX (ComplexConstant (5.0 :+ 10.0)) a)]
                in Circuit "TESTRX" parameters instructions

hadamardRotateXYZ :: (Floating a, Show a, Ord a) => Complex a -> Circuit a
hadamardRotateXYZ rotation
    = let parameters@[Left a, Left b, Right r, Left z] = [Left (MetaQubitRegister "a"), Left (MetaQubitRegister "b"), Right (MetaRegister "r"), Left (MetaQubitRegister "z")]
          instructions = [(Hadamard a),
                          (MeasureOut a r)] ++
                          (ifC "HROTXYZTHEN0" "HROTXYZEND0"
                               r
                               [(RX (ComplexConstant rotation) z)]
                               ([(Hadamard b),
                                 (MeasureOut b r)] ++
                                (ifC "HROTXYZTHEN1" "HROTXYZEND1"
                                     r
                                     [(RY (ComplexConstant rotation) z)]
                                     [(RZ (ComplexConstant rotation) z)])))
      in Circuit "HROTXYZ" parameters instructions

compile :: IO ()
compile = putStrLn "Compiling quantum executable" >>
          --Test registers
          putStrLn (show $ QubitRegister 10) >>
          putStrLn (show $ Register 5) >>
          putStrLn (show $ ComplexConstant (6.0 :+ 7.0)) >>
          putStrLn (show $ ComplexConstant ((-8.0) :+ 1.0)) >>
          putStrLn (show $ ComplexConstant (2.0 :+ (-3.0))) >>
          putStrLn (show $ ComplexConstant ((-4.0) :+ (-6.0))) >>
          --Test instructions
          putStrLn (show $ CNot (QubitRegister 0) (QubitRegister 1)) >>
          putStrLn (show $ PSwap (ComplexConstant (5.0 :+ (-3.2))) (QubitRegister 0) (QubitRegister 1)) >>
          putStrLn (show $ Measure (QubitRegister 4)) >>
          putStrLn (show $ MeasureOut (QubitRegister 4) (Register 5)) >>
          --Test circuits
          putStrLn (show $ DefCircuit bell) >>
          putStrLn (show $ CallCircuit bell [Left (QubitRegister 5), Left (QubitRegister 3)]) >>
          putStrLn (show $ DefCircuit testRXCircuit) >>
          putStrLn (show $ CallCircuit testRXCircuit [Left (QubitRegister 1)]) >>
          putStrLn (show $ DefCircuit xor) >>
          putStrLn (show $ CallCircuit xor [Right (Register 0), Right (Register 1), Right (Register 2)]) >>
          putStrLn (show $ DefCircuit halfAdder) >>
          putStrLn (show $ CallCircuit halfAdder [Right (Register 0), Right (Register 1), Right (Register 2), Right (Register 3)]) >>
          putStrLn (show $ DefCircuit adder) >>
          putStrLn (show $ CallCircuit adder [Right (Register 0), Right (Register 1), Right (Register 2), Right (Register 3), Right (Register 4)]) >>
          --Load the integer 53 into registers [0-31]
          putStrLn (intercalate "\n" (fmap show (loadIntToRegisters 53 0))) >>
          --Load the integer 18 into registers [32-63]
          putStrLn (intercalate "\n" (fmap show (loadIntToRegisters 18 32))) >>
          --Add [0-31] + [32-63] into [64-95]
          let carry = 128
              temp = 129
          in putStrLn (intercalate "\n" (fmap show (addRegisters carry temp 0 32 64))) >>
          putStrLn (show $ DefCircuit (hadamardRotateXYZ (0.70710678118 :+ 0.70710678118))) >>
          putStrLn (show $ CallCircuit (hadamardRotateXYZ (0.70710678118 :+ 0.70710678118)) [Left (QubitRegister 0), Left (QubitRegister 1), Right (Register 0), Left (QubitRegister 2)])
