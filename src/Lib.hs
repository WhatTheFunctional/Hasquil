--Haskell Quil compiler
--Copyright Laurence Emms 2018

module Lib (compile) where

import Data.Complex
import Register
import Instruction

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
          putStrLn (show $ Measure (QubitRegister 4) Nothing) >>
          putStrLn (show $ Measure (QubitRegister 4) (Just (Register 5)))
