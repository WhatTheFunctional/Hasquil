--Classical circuits
--Copyright Laurence Emms 2018

module ClassicalCircuit (xor,
                         halfAdder,
                         adder) where

import Register
import Instruction

--XOR circuit
xor :: (Floating a, Show a, Ord a) => Circuit a
xor = let parameters@[Right a, Right b, Right r] = [Right (MetaRegister "a"),
                                                    Right (MetaRegister "b"),
                                                    Right (MetaRegister "r")]
          instructions = [(Move b r),
                          (IOr a r),
                          (JumpUnless "xor0" r),
                          (Move b r),
                          (INot a),
                          (INot r),
                          (IOr a r),
                          (INot a),
                          (Label "xor0")]
      in Circuit "XOR" parameters instructions

--Half adder
halfAdder :: (Floating a, Show a, Ord a) => Circuit a
halfAdder = let parameters@[Right c, Right a, Right b, Right s] = [Right (MetaRegister "c"),
                                                                   Right (MetaRegister "a"),
                                                                   Right (MetaRegister "b"),
                                                                   Right (MetaRegister "s")]
                instructions = [(CallCircuit xor[Right a, Right b, Right s]),
                                (Move b c),
                                (IAnd a c)]
            in Circuit "HALFADDER" parameters instructions

--Full adder
adder :: (Floating a, Show a, Ord a) => Circuit a
adder = let parameters@[Right c, Right temp, Right a, Right b, Right s] = [Right (MetaRegister "c"),
                                                                           Right (MetaRegister "temp"),
                                                                           Right (MetaRegister "a"),
                                                                           Right (MetaRegister "b"),
                                                                           Right (MetaRegister "s")]
            instructions = [(CallCircuit xor [Right a, Right b, Right s]),
                            (Move s temp),
                            (CallCircuit xor [Right temp, Right c, Right s]),
                            (IAnd c temp),
                            (Move b c),
                            (IAnd a c),
                            (IOr temp c)]
        in Circuit "ADDER" parameters instructions
