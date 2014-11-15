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

data Program = Program [Stmt] deriving (Show)

data Stmt = Alu8 Alu8Stmt 
          | Alu16 Alu16Stmt 
          | Ld8 Ld8Stmt 
          | Ld16 Ld16Stmt 
          | Jmp JmpStmt
          | Miscellaneous MiscellaneousStmt
            deriving (Show)

data Alu8Stmt = AluReg Alu8Op Register 
              | AluImm Alu8Op Integer
              | AluMemHl Alu8Op 
               deriving (Show)

data Alu16Stmt = Inc16 CombinedRegister
               | Dec16 CombinedRegister
               | AddSpIm Integer
               | AddHL CombinedRegister
               deriving (Show)

data Ld8Stmt = LdRegReg Register Register -- Load register contents into register
            | LdRegIm  Register Integer -- Load immediate 8 bit unsigned val into Reg
            | LdRegMemHl Register  -- Load value at address HL into register
            | LdMemHlReg Register  -- Load register into memory at address HL
            | LdAMemCombinedReg CombinedRegister --Load value at address Combined reg into A
            | LdMemCombinedRegA CombinedRegister --Load contents of register A into mem address of reg
            | LdAMemIm Integer --Load value at immediate 16 bit address into register A
            | LdiAmemHL
            | LdimemHLA
            | LddAmemHL
            | LddmemHLA
            | LDIOAC   
            | LDIOCA    
            | LdhIOA Integer
            | LdhAIO Integer 
              deriving (Show)

data Ld16Stmt = LdCombinedRegIm CombinedRegister Integer
              |  LdMemSp Integer -- Load SP contents into given 16 bit address 
              |  LdSpHl -- Load contents of HL into SP
              |  LdHLSpPlusIm Integer -- Load contents of memory 
              |  Push StackRegister -- Push contents of combined reg onto stack
              |  Pop StackRegister -- Pop contents of combined reg from stack
                 deriving (Show)

data JmpStmt = JmpIm Integer -- Jump to 16 bit immediate address
             | JmpCond Condition Integer -- Jump if Condition is met
             | JmpHL -- Jump to address contained in register HL
             | JmpRel Integer -- Jump to 8 bit address relative to PC
             | JmpRelCond Condition Integer -- Relative Jump if Condition is met
               deriving (Show) 

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
    deriving (Show)

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


data Condition = NotZero | Zero | NoCarry | Carry deriving (Show)

data Register = A | B | C | D | E | H | L  deriving (Show)

data CombinedRegister = BC | DE | HL | SP  deriving (Show) 

data StackRegister = StackRegAF | StackRegBC | StackRegDE | StackRegHL deriving (Show)

data Alu8Op = Add | Adc | Sub | Sbc | And | Or | Xor | Cmp | Inc | Dec deriving (Show)

