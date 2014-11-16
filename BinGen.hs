-- Generates Gameboy Binary --
module BinGen where

import InstructionTree

import Data.Char
import qualified Data.ByteString as B
import Data.List
import qualified Data.Map as M
import Data.Word
import Data.Bits

splitW16 :: Word16 -> [Word8]
splitW16 w16 = [fromIntegral (w16 .&. 0xFF),  fromIntegral (w16 `shiftR` 8)]

regs = [B, C, D, E, H, L, A]
regOffsets :: [Word8]
regOffsets = [0, 1, 2, 3, 4, 5, 7] 
regOffsetMap = M.fromList $ zip regs regOffsets

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

-- Gives  offset of first register in load instruction
reg1MapLd :: M.Map Register Word8
reg1MapLd = M.fromList [(B,0), (C, 8), (D, 16), (E, 24), (H, 32), (L, 40), (A, 52)]


-- |Generate Gameboy Binary code from Assembly
--genCode :: Program -> B.ByteString
--genCode (Program xs) = concatMap genStmt xs


genStmt :: Stmt -> B.ByteString
genStmt (Alu8 a) = genAlu8 a
genStmt (Alu16 a) = genAlu16 a
genStmt (Ld8 a) = genLd8 a 
genStmt (Ld16 a) = genLd16 a

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


genLd8 :: Ld8Stmt -> B.ByteString
genLd8 (LdRegReg reg1 reg2) = B.singleton $ 0x40 + 
    (M.findWithDefault 0x0 reg1 reg1MapLd) + (M.findWithDefault 0x0 reg2 regOffsetMap)
genLd8 (LdRegIm reg word) = B.pack [ins, word] 
 where ins = case reg of 
        B -> 0x06
        C -> 0x0E
        D -> 0x16
        E -> 0x1E
        H -> 0x26
        L -> 0x2E
        A -> 0x3E
genLd8 (LdRegMemHl reg) = B.singleton $ case reg of
    B -> 0x46
    C -> 0x4E
    D -> 0x56
    E -> 0x5E
    H -> 0x66
    L -> 0x6E
    A -> 0x7E
genLd8 (LdMemHlReg reg) = B.singleton $ case reg of
    B -> 70
    C -> 71
    D -> 72
    E -> 73
    H -> 74
    L -> 75
    A -> 77
genLd8 (LdAMemCombinedReg reg) = B.singleton $ case reg of
    BC -> 0x0A
    DE -> 0x1A
    otherwise -> error $ "Cannot load memory at  register " ++ (show reg) ++ "into register A"
genLd8 (LdMemCombinedRegA reg) = B.singleton $ case reg of
    BC -> 0x3
    DE -> 0x13
    otherwise -> error $ "Cannot load content of register A into memory at register " ++ (show reg) 
genLd8 (LdAMemIm w16) = B.pack $ [0xFA] ++ (splitW16 w16)
genLd8 (LdMemImA w16) = B.pack $ [0xEA] ++ (splitW16 w16)
genLd8 LdiAmemHL = B.singleton 0x2A
genLd8 LdimemHLA = B.singleton 0x22
genLd8 LddAmemHL = B.singleton 0x3A
genLd8 LddmemHLA = B.singleton 0x33
genLd8 LDIOAC = B.singleton 0xF2 
genLd8 LDIOCA = B.singleton 0xE2
genLd8 (LdhIOA w8) = B.pack [0xE0, w8]
genLd8 (LdhAIO w8) = B.pack [0xF0, w8]


genLd16 :: Ld16Stmt -> B.ByteString
genLd16 (LdCombinedRegIm reg16 w16) = B.pack $ [ins] ++ (splitW16 w16) 
 where ins = case reg16 of
        BC -> 0x01
        DE -> 0x11
        HL -> 0x21
        SP -> 0x31
genLd16 (LdMemSp w16) = B.singleton 0x08
genLd16 LdSpHl = B.singleton 0xF9
genLd16 (LdHLSpPlusIm w16) = B.singleton 0xF8
genLd16 (Push stackReg) = B.singleton $ case stackReg of
    StackRegBC -> 0xC5
    StackRegDE -> 0xD5
    StackRegHL -> 0xE5 
    StackRegAF -> 0xF5
genLd16 (Pop stackReg) = B.singleton $ case stackReg of
    StackRegBC -> 0xC1
    StackRegDE -> 0xD1
    StackRegHL -> 0xE1
    StackRegAF -> 0xF1




