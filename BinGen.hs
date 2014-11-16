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
getRegDecs = zip (zip (repeat Dec) regs) [0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x3D] 

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
    
            

aluWordTable :: M.Map Alu8Op Word8
aluWordTable = M.fromList $ 
    zip [Add, Adc, Sub, Sbc, And, Xor, Or, Cmp]
        [0xC6, 0xCE, 0xD6, 0xDE, 0xE6, 0xEE, 0xF6, 0xFE]


aluHLTable :: M.Map Alu8Op Word8
aluHLTable = M.fromList $
    zip [Add, Adc, Sub, Sbc, And, Xor, Or, Cmp, Inc, Dec]
        [0x86, 0x8E, 0x96, 0x9E, 0xA6, 0xAE, 0xB6, 0xBE, 0x34, 0x35]

-- |Generate Gameboy Binary code from Assembly
--genCode :: Program -> B.ByteString
--genCode (Program xs) = concatMap genStmt xs


genStmt :: Stmt -> B.ByteString
genStmt (Alu8 a) = genAlu8 a
genStmt (Alu16 a) = genAlu16 a

genAlu8 :: Alu8Stmt -> B.ByteString
genAlu8 (AluReg op reg) = B.singleton $ M.findWithDefault 0xD3 (op, reg) aluRegTable
genAlu8 (AluImm op word) = B.pack [M.findWithDefault 0xD3 op aluWordTable ,word] 
genAlu8 (AluMemHl op) = B.singleton $ M.findWithDefault 0xD3 op aluHLTable  

genAlu16 :: Alu16Stmt -> B.ByteString
genAlu16 (Inc16 reg) = B.singleton $ case reg of
    BC -> 0x03
    DE -> 0x13
    HL -> 0x23
    SP -> 0x33
genAlu16 (Dec16 reg) = B.singleton $ case reg of
    BC -> 0x0B
    DE -> 0x1B
    HL -> 0x2B
    SP -> 0x3B
genAlu16 (AddSpIm w) = B.pack [0xE8, w]
genAlu16 (AddHL reg) = B.singleton $ case reg of
    BC -> 0x09
    DE -> 0x19
    HL -> 0x29
    SP -> 0x39


