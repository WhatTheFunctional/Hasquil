--Classical circuits
--Copyright Laurence Emms 2018

module ClassicalCircuit (xor,
                         halfAdder) where

import Register
import Instruction

--XOR circuit
xor :: (Floating a, Show a, Ord a) => String -> Circuit a
xor jumpLabel = let parameters@[Right a, Right b, Right r] = [Right (MetaRegister "a"),
                                                              Right (MetaRegister "b"),
                                                              Right (MetaRegister "r")]
                    instructions = [(Move b r),
                                    (IOr a r),
                                    (JumpUnless jumpLabel r),
                                    (Move b r),
                                    (INot a),
                                    (INot r),
                                    (IOr a r),
                                    (INot a),
                                    (Label jumpLabel)]
                in Circuit "XOR" parameters instructions

--Half Adder
--Requires an existing XOR circuit
halfAdder :: (Floating a, Show a, Ord a) => Circuit a -> Circuit a
halfAdder xorCircuit = let parameters@[Right a, Right b, Right s, Right c] = [Right (MetaRegister "a"),
                                                                              Right (MetaRegister "b"),
                                                                              Right (MetaRegister "s"),
                                                                              Right (MetaRegister "c")]
                           instructions = [(CallCircuit xorCircuit [Right a, Right b, Right s]),
                                           (Move b c),
                                           (IAnd a c)]
                      in Circuit "HALFADDER" parameters instructions

