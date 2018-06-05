--Classical circuits
--Copyright Laurence Emms 2018

module ClassicalCircuit (xor) where

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
