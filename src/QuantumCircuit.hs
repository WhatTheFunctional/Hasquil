--Quantum circuits
--Copyright Laurence Emms 2018

module QuantumCircuit (bell) where

import Data.Complex
import Register
import Instruction

bell :: (Floating a, Show a, Ord a) => Circuit a
bell = let parameters@[Left a, Left b] = [Left (MetaQubitRegister "a"), Left (MetaQubitRegister "b")]
           instructions = [(Hadamard a),
                           (CNot a b)]
       in Circuit "BELL" parameters instructions

clear :: (Floating a, Show a, Ord a) => Circuit a
clear = let parameters@[Left q, Right scratch] = [Left (MetaQubitRegister "q"), Right (MetaRegister "scratch")]
            instructions = [(MeasureOut q scratch),
                            (JumpUnless "CLEAREND" scratch),
                            (PauliX q),
                            (Label "CLEAREND")]
        in Circuit "CLEAR" parameters instructions

euler :: (Floating a, Show a, Ord a) => Complex a -> Complex a -> Complex a -> Circuit a
euler alpha beta gamma = let parameters@[Left q] = [Left (MetaQubitRegister "q")]
                             instructions = [(RX (ComplexConstant alpha) q),
                                             (RY (ComplexConstant alpha) q),
                                             (RZ (ComplexConstant alpha) q)]
                         in Circuit "EULER" parameters instructions
