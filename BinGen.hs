-- Generates Gameboy Binary --
module BinGen where

import InstructionTree

import Data.Char
import qualified Data.ByteString as B
import Data.List
import qualified Data.Map as M
import Data.Word


regs = [B, C, D, E, H, L, A]
regOffsets = [0, 1, 2, 3, 4, 5, 7]

getRegAluOps :: Word8 -> Alu8Op -> [((Alu8Op, Register), Word8)]
getRegAluOps n op = zip (zip (repeat op) regs) (map (+ n) regOffsets)

getRegIncs :: [((Alu8Op, Register), Word8)]
getRegIncs = zip (zip (repeat Inc) regs) [0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x3C] 

getRegDecs :: [((Alu8Op, Register), Word8)]
getRegDecs = zip (zip (repeat Inc) regs) [0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x3D] 

aluRegTable :: M.Map (Alu8Op, Register) Word8
aluRegTable = M.fromList $
    getRegAluOps 0x80 Add ++
    getRegAluOps 0x88 Adc ++
    getRegAluOps 0x90 Sub ++
    getRegAluOps 0x98 Sbc ++
    getRegAluOps 0xA0 And ++
    getRegAluOps 0xA8 Xor ++
    getRegAluOps 0xB0 Or  ++
    getRegAluOps 0xB8 Cmp ++
    getRegIncs            ++
    getRegDecs
    
            


-- |Generate Gameboy Binary code from Assembly
--genCode :: Program -> B.ByteString
--genCode (Program xs) = concatMap genStmt xs



--genStmt :: Stmt -> B.ByteString
--genStmt (Alu8 a) = genAlu8 a


genAlu8 :: Alu8Stmt -> B.ByteString
genAlu8 (AluReg op reg) = B.singleton $ M.findWithDefault 0x0 (op, reg) aluRegTable
