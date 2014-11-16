-- Instruction Tree --
module InstructionTree(
Program(..),
Stmt(..), 
Alu8Stmt(..), 
Alu16Stmt(..), 
Ld8Stmt(..), 
Ld16Stmt(..), 
JmpStmt(..), 
MiscellaneousStmt(..),
RotateStmt(..),
Condition(..),
Register(..),
StackRegister(..),
CombinedRegister(..),
Alu8Op(..))
where

import Data.Word

data Program = Program [Stmt] deriving (Show)

data Stmt = Alu8 Alu8Stmt 
          | Alu16 Alu16Stmt 
          | Ld8 Ld8Stmt 
          | Ld16 Ld16Stmt 
          | Jmp JmpStmt
          | Miscellaneous MiscellaneousStmt
            deriving (Show, Eq, Ord)

data Alu8Stmt = AluReg Alu8Op Register 
              | AluImm Alu8Op Word8
              | AluMemHl Alu8Op 
               deriving (Show,Eq, Ord)

data Alu16Stmt = Inc16 CombinedRegister
               | Dec16 CombinedRegister
               | AddSpIm Word8
               | AddHL CombinedRegister
               deriving (Show,Eq, Ord)

data Ld8Stmt = LdRegReg Register Register -- Load register contents into register
            | LdRegIm  Register Word8 -- Load immediate 8 bit unsigned val into Reg
            | LdRegMemHl Register  -- Load value at address HL into register
            | LdMemHlReg Register  -- Load register into memory at address HL
            | LdAMemCombinedReg CombinedRegister --Load value at address Combined reg into A
            | LdMemCombinedRegA CombinedRegister --Load contents of register A into mem address of reg
            | LdAMemIm Word16 --Load value at immediate 16 bit address into register A
            | LdiAmemHL
            | LdimemHLA
            | LddAmemHL
            | LddmemHLA
            | LDIOAC   
            | LDIOCA    
            | LdhIOA Word8
            | LdhAIO Word8 
              deriving (Show,Eq, Ord)

data Ld16Stmt = LdCombinedRegIm CombinedRegister Word16
              |  LdMemSp Word16 -- Load SP contents into given 16 bit address 
              |  LdSpHl -- Load contents of HL into SP
              |  LdHLSpPlusIm Word16 -- Load contents of memory 
              |  Push StackRegister -- Push contents of combined reg onto stack
              |  Pop StackRegister -- Pop contents of combined reg from stack
                 deriving (Show, Eq, Ord)

data JmpStmt = JmpIm Word16 -- Jump to 16 bit immediate address
             | JmpCond Condition Word16 -- Jump if Condition is met
             | JmpHL -- Jump to address contained in register HL
             | JmpRel Word8 -- Jump to 8 bit address relative to PC
             | JmpRelCond Condition Word8 -- Relative Jump if Condition is met
               deriving (Show, Eq, Ord) 

data MiscellaneousStmt = 
    SwapReg Register
  | SwapMemHL 
  | DAA
  | CPL
  | CCF
  | SCF
  | NOP
  | HALT
  | STOP
  | DI
  | EI
    deriving (Show, Eq, Ord)

data RotateStmt = RLCA
                | RLA
                | RRCA
                | RRA
                | RLCReg Register
                | RLCMemHL 
                | RLReg Register
                | RLMemHL 
                | RRCReg Register
                | RRCMemHL
                | RRReg Register
                | RRMemHL 
                | SLAReg Register
                | SLAMemHL
                | SRAReg Register
                | SRAMemHL 
                | SRLReg Register
                | SRLMemHL
                deriving (Show, Eq, Ord)

data Condition = NotZero | Zero | NoCarry | Carry deriving (Show,Eq, Ord)

data Register = A | B | C | D | E | H | L  deriving (Show,Eq, Ord)

data CombinedRegister = BC | DE | HL | SP  deriving (Show,Eq, Ord) 

data StackRegister = StackRegAF | StackRegBC | StackRegDE | StackRegHL deriving (Show,Eq, Ord)

data Alu8Op = Add | Adc | Sub | Sbc | And | Or | Xor | Cmp | Inc | Dec deriving (Show,Eq, Ord)

