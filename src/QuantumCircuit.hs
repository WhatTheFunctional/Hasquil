--Quantum circuits
--Copyright Laurence Emms 2018

module QuantumCircuit (bell) where

import Register
import Instruction

bell :: (Floating a, Show a, Ord a) => Circuit a
bell = let parameters@[Left a, Left b] = [Left (MetaQubitRegister "a"), Left (MetaQubitRegister "b")]
           instructions = [(Hadamard a),
                           (CNot a b)]
       in Circuit "BELL" parameters instructions
