--Classical circuits
--Copyright Laurence Emms 2018

module ClassicalCircuit (xor,
                         halfAdder,
                         adder) where

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

--Half adder
--Requires an existing XOR circuit
halfAdder :: (Floating a, Show a, Ord a) => Circuit a -> Circuit a
halfAdder xorCircuit = let parameters@[Right c, Right a, Right b, Right s] = [Right (MetaRegister "c"),
                                                                              Right (MetaRegister "a"),
                                                                              Right (MetaRegister "b"),
                                                                              Right (MetaRegister "s")]
                           instructions = [(CallCircuit xorCircuit [Right a, Right b, Right s]),
                                           (Move b c),
                                           (IAnd a c)]
                      in Circuit "HALFADDER" parameters instructions

--Full adder
--Requires an existing XOR circuit
adder :: (Floating a, Show a, Ord a) => Circuit a -> Circuit a
adder xorCircuit = let parameters@[Right c, Right temp, Right a, Right b, Right s] = [Right (MetaRegister "c"),
                                                                                      Right (MetaRegister "temp"),
                                                                                      Right (MetaRegister "a"),
                                                                                      Right (MetaRegister "b"),
                                                                                      Right (MetaRegister "s")]
                       instructions = [(CallCircuit xorCircuit [Right a, Right b, Right s]),
                                       (Move s temp),
                                       (CallCircuit xorCircuit [Right temp, Right c, Right s]),
                                       (IAnd c temp),
                                       (Move b c),
                                       (IAnd a c),
                                       (IOr temp c)]
                   in Circuit "ADDER" parameters instructions
