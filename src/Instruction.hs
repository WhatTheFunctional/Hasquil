--Hasquil instructions
--Copyright Laurence Emms 2018

module Instruction (Instruction(..),
                    Circuit(..)) where

import Data.Char
import Register

data Instruction a
    = PauliI Quantum | --Quantum instructions
      PauliX Quantum |                    
      PauliY Quantum |                    
      PauliZ Quantum |
      Hadamard Quantum |
      Phase (Classical a) Quantum |
      PhaseS Quantum |
      PhaseT Quantum |
      CPhase00 (Classical a) Quantum Quantum |
      CPhase01 (Classical a) Quantum Quantum |
      CPhase10 (Classical a) Quantum Quantum |
      CPhase (Classical a) Quantum Quantum |
      RX (Classical a) Quantum |
      RY (Classical a) Quantum |
      RZ (Classical a) Quantum |
      CNot Quantum Quantum |
      CCNot Quantum Quantum Quantum Quantum |
      PSwap (Classical a) Quantum Quantum |
      Swap Quantum Quantum |
      ISwap Quantum Quantum |
      CSwap Quantum Quantum Quantum Quantum |
      Measure Quantum |
      MeasureOut Quantum (Classical a) |
      Reset |
      Halt | --Classical instructions
      Jump String |
      JumpWhen String (Classical a) |
      JumpUnless String (Classical a) |
      Label String |
      Nop |
      IFalse (Classical a) |
      ITrue (Classical a) |
      INot (Classical a) |
      IAnd (Classical a) (Classical a) |
      IOr (Classical a) (Classical a) |
      Move (Classical a) (Classical a) |
      Exchange (Classical a) (Classical a) |
      Pragma String |
      DefCircuit (Circuit a) |
      CallCircuit (Circuit a) [Either Quantum (Classical a)]

instance (Floating a, Show a, Ord a) => Show (Instruction a) where
    show (PauliI q) = "I " ++ (show q)
    show (PauliX q) = "X " ++ (show q)
    show (PauliY q) = "Y " ++ (show q)
    show (PauliZ q) = "Z " ++ (show q)
    show (Hadamard q) = "H " ++ (show q)
    show (Phase c q) = "PHASE(" ++ (show c) ++ ") " ++ (show q)
    show (PhaseS q) = "S " ++ (show q)
    show (PhaseT q) = "T " ++ (show q)
    show (CPhase00 c q0 q1) = "CPHASE00(" ++ (show c) ++ ") " ++ (show q0) ++ " " ++ (show q1)
    show (CPhase01 c q0 q1) = "CPHASE01(" ++ (show c) ++ ") " ++ (show q0) ++ " " ++ (show q1)
    show (CPhase10 c q0 q1) = "CPHASE10(" ++ (show c) ++ ") " ++ (show q0) ++ " " ++ (show q1)
    show (CPhase c q0 q1) = "CPHASE(" ++ (show c) ++ ") " ++ (show q0) ++ " " ++ (show q1)
    show (RX c q) = "RX(" ++ (show c) ++ ") " ++ (show q)
    show (RY c q) = "RY(" ++ (show c) ++ ") " ++ (show q)
    show (RZ c q) = "RZ(" ++ (show c) ++ ") " ++ (show q)
    show (CNot q0 q1) = "CNOT " ++ (show q0) ++ " " ++ (show q1)
    show (CCNot q0 q1 q2 q3) = "CCNOT " ++ (show q0) ++ " " ++ (show q1) ++ (show q2) ++ " " ++ (show q3)
    show (PSwap c q0 q1) = "PSWAP(" ++ (show c) ++ ") " ++ (show q0) ++ " " ++ (show q1)
    show (Swap q0 q1) = "SWAP " ++ (show q0) ++ " " ++ (show q1)
    show (ISwap q0 q1) = "ISWAP " ++ (show q0) ++ " " ++ (show q1)
    show (CSwap q0 q1 q2 q3) = "CSWAP " ++ (show q0) ++ " " ++ (show q1) ++ (show q2) ++ " " ++ (show q3)
    show (Measure q) = "MEASURE " ++ (show q)
    show (MeasureOut q r) = "MEASURE " ++ (show q) ++ " " ++ (show r)
    show (Reset) = "RESET"
    show Halt = "HALT"
    show (Jump s) = "JUMP @" ++ (fmap toUpper s)
    show (JumpWhen s r) = "JUMP-WHEN @" ++ (fmap toUpper s) ++ " " ++ (show r)
    show (JumpUnless s r) = "JUMP-UNLESS @" ++ (fmap toUpper s) ++ " " ++ (show r)
    show (Label s) = "LABEL @" ++ (fmap toUpper s)
    show Nop = "NOP"
    show (IFalse r) = "FALSE " ++ (show r)
    show (ITrue r) = "TRUE " ++ (show r)
    show (INot r) = "NOT " ++ (show r)
    show (IAnd r0 r1) = "AND " ++ (show r0) ++ " " ++ (show r1)
    show (IOr r0 r1) = "OR " ++ (show r0) ++ " " ++ (show r1)
    show (Move r0 r1) = "MOVE " ++ (show r0) ++ " " ++ (show r1)
    show (Exchange r0 r1) = "EXCHANGE " ++ (show r0) ++ " " ++ (show r1)
    show (Pragma s) = "PRAGMA " ++ s
    show (DefCircuit c) = case showDefCircuit c of
                               Left e -> e
                               Right c -> c
    show (CallCircuit c arguments) = case showCallCircuit c arguments of
                                     Left e -> e
                                     Right c -> c

--Circuit type
data Circuit a = Circuit String [Either Quantum (Classical a)] [Instruction a]

type CircuitText = String

--Circuit definition
showDefCircuit :: (Floating a, Show a, Ord a) => Circuit a -> Either String CircuitText
showDefCircuit (Circuit name _ []) = Left ("Error (showDefCircuit): No instructions in circuit " ++ name)
showDefCircuit (Circuit name parameters instructions) = (Right ("DEFCIRCUIT " ++ (fmap toUpper name))) >>= (defCircuitParameters parameters instructions name)

defCircuitParameters :: (Floating a, Show a, Ord a) => [Either Quantum (Classical a)] -> [Instruction a] -> String -> CircuitText -> Either String CircuitText
defCircuitParameters [] instructions name circuitText = (Right (circuitText ++ ":")) >>= (defCircuitInstructions instructions name)
defCircuitParameters (Left r@(MetaQubitRegister _) : parameters) instructions name circuitText = (Right (circuitText ++ " " ++ (show  r))) >>= (defCircuitParameters parameters instructions name)
defCircuitParameters (Right r@(MetaRegister _) : parameters) instructions name circuitText = (Right (circuitText ++ " " ++ (show r))) >>= (defCircuitParameters parameters instructions name)
defCircuitParameters p _ name _ = Left ("Error (defCircuitParameters): Type mismatch for parameter " ++ (show p) ++ " in circuit " ++ name)

circuitInstruction :: (Floating a, Show a, Ord a) => [Instruction a] -> String -> CircuitText -> Either String CircuitText
circuitInstruction (instruction : instructions) name circuitText = (Right (circuitText ++ "\n    " ++ (show instruction))) >>= (defCircuitInstructions instructions name)

defCircuitInstructions :: (Floating a, Show a, Ord a) => [Instruction a] -> String-> CircuitText -> Either String CircuitText
defCircuitInstructions [] name circuitText = Right circuitText
defCircuitInstructions (instruction@(PauliI (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(PauliX (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(PauliY (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(PauliZ (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(Hadamard (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(Phase (RealConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Phase (ComplexConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Phase (MetaRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(PhaseS (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(PhaseT (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(CPhase00 (RealConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase00 (ComplexConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase00 (MetaRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(CPhase01 (RealConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase01 (ComplexConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase01 (MetaRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(CPhase10 (RealConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase10 (ComplexConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase10 (MetaRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(CPhase (RealConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase (ComplexConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CPhase (MetaRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(RX (RealConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(RX (ComplexConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(RX (MetaRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(RY (RealConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(RY (ComplexConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(RY (MetaRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(RZ (RealConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(RZ (ComplexConstant _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(RZ (MetaRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(CNot (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CCNot (MetaQubitRegister _) (MetaQubitRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(PSwap (RealConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(PSwap (ComplexConstant _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(PSwap (MetaRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText

defCircuitInstructions (instruction@(Swap (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(ISwap (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CSwap (MetaQubitRegister _) (MetaQubitRegister _) (MetaQubitRegister _) (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Measure (MetaQubitRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(MeasureOut (MetaQubitRegister _) (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Reset) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Halt) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Jump _) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(JumpWhen _ (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(JumpUnless _ (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Label _) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Nop) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(IFalse (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(ITrue (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(INot (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(IAnd (MetaRegister _) (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(IOr (MetaRegister _) (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Move (MetaRegister _) (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Exchange (MetaRegister _) (MetaRegister _)) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(Pragma _) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
defCircuitInstructions (instruction@(CallCircuit _ _) : instructions) name circuitText = circuitInstruction (instruction : instructions) name circuitText
--Since circuits are macros, defining recursive circuits inside circuits is forbidden
defCircuitInstructions (instruction : instructions) name _ = Left ("Error (defCircuitInstructions): Type mismatch for instruction " ++ (show instruction) ++ " in circuit " ++ name)

--Circuit call
showCallCircuit :: (Floating a, Show a, Ord a) => Circuit a -> [Either Quantum (Classical a)] -> Either String CircuitText
showCallCircuit (Circuit name _ _) [] = Right name
showCallCircuit (Circuit name parameters _) arguments = (Right name) >>= callCircuitArguments parameters arguments name

callCircuitArguments :: (Floating a, Show a, Ord a) => [Either Quantum (Classical a)] -> [Either Quantum (Classical a)] -> String -> CircuitText -> Either String String
callCircuitArguments [] [] _ circuitText = Right circuitText
callCircuitArguments (Left (MetaQubitRegister _) : parameters) (Left q@(QubitRegister _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show q))) >>= callCircuitArguments parameters arguments name
callCircuitArguments (Left (MetaQubitRegister _) : parameters) (Left q@(MetaQubitRegister _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show q))) >>= callCircuitArguments parameters arguments name
callCircuitArguments (Right (RealConstant _) : parameters) (Right c@(RealConstant _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show c))) >>= callCircuitArguments parameters arguments name
callCircuitArguments (Right (ComplexConstant _) : parameters) (Right c@(ComplexConstant _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show c))) >>= callCircuitArguments parameters arguments name
callCircuitArguments (Right (MetaRegister _) : parameters) (Right r@(Register _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show r))) >>= callCircuitArguments parameters arguments name
callCircuitArguments (Right (MetaRegister _) : parameters) (Right r@(Range _ _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show r))) >>= callCircuitArguments parameters arguments name
callCircuitArguments (Right (MetaRegister _) : parameters) (Right r@(MetaRegister _) : arguments) name circuitText = (Right (circuitText ++ " " ++ (show r))) >>= callCircuitArguments parameters arguments name
callCircuitArguments _ [] name _ = Left ("Error (callCircuitArguments): Incorrect number of arguments passed to circuit " ++ name)
callCircuitArguments _ (a : arguments) name _ = Left ("Error (callCircuitArguments): Type mismatch for argument " ++ (show a) ++ " in circuit " ++ name)
