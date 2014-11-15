{- Parser and Lexer -}

module GBParser (parseSource) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Error;
import Control.Applicative hiding ((<|>), many, optional, empty)
import InstructionTree
        
languageDef =
    emptyDef { Token.commentStart  = ""
             , Token.commentEnd    = ""
             , Token.commentLine   = ";"
             , Token.identStart    = letter
             , Token.identLetter   = alphaNum
             , Token.reservedNames = ["add", "adc", "sub", "sbc", "cmp", "and"
                                     ,"or", "xor", "mul", "div", "a", "b", "c"
                                     , "d", "e", "h", "l", "memHL", "memBC"
                                     , "memDE", "memSP"
                                     ]
             , Token.reservedOpNames = []
             }
             
lexer = Token.makeTokenParser languageDef             
             
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer 
parens = Token.parens lexer
integer = Token.integer lexer
semiColon = Token.semi lexer 
whiteSpace = Token.whiteSpace lexer            
comma = Token.comma lexer
a = reserved "a"
c = reserved "c"
hl = reserved "hl"
sp = reserved "sp"

parseSource :: String -> Program
parseSource = parser asmParser 

parser :: Show a => Parser a -> String -> a
parser p input
        = case parse p "" input of
            Left err -> error $ "parse error at " ++ (show err)
            Right x  -> x


asmParser :: Parser Program
asmParser = whiteSpace >> program

program :: Parser Program
program = Program <$> statements

statements :: Parser [Stmt] 
statements =     try ((:) <$> statement <*> statements)
             <|> (eof >> return [])


statement :: Parser Stmt
statement =      (Alu8 <$> try alu8Stmt)
             <|> (Alu16 <$> try alu16Stmt)
             <|> (Ld8  <$> try ld8Stmt)
             <|> (Ld16 <$> try ld16Stmt)
             <|> (Jmp <$> try jmpStmt)
             <|> (Miscellaneous <$> try miscellaneousStmt)


--Parse 8 bit ALU instructions  
alu8Stmt :: Parser Alu8Stmt
alu8Stmt =    try (parserAluOp >>= alu8Stmt')

alu8Stmt' :: Alu8Op -> Parser Alu8Stmt
alu8Stmt' op = 
           try (AluReg op <$> parser8Reg)
       <|> try (AluImm op <$> parserSigned8Int)
       <|> try (parens hl >> return (AluMemHl op))

       <?> "register or 8 bit signed integer"  
       

parserAluOp :: Parser Alu8Op
parserAluOp =   try (reserved "add" >> a >> comma >> return Add)
            <|> try (reserved "adc" >> a >> comma >> return Adc)
            <|> try (reserved "sub" >> return Sub)
            <|> try (reserved "sbc" >> a >> comma >> return Sbc)
            <|> try (reserved "cp"  >> return Cmp)
            <|> try (reserved "and" >> return And)
            <|> try (reserved "or"  >> return Or)
            <|> try (reserved "xor" >> return Xor)
            <|> try (reserved "inc" >> return Inc)
            <|> try (reserved "dec" >> return Dec)

-- Parse 16 bit ALU instructions
alu16Stmt :: Parser Alu16Stmt
alu16Stmt =     try (Inc16 <$> (reserved "inc" *> parseCombinedReg))
            <|> try (Dec16 <$> (reserved "dec" *> parseCombinedReg)) 
            <|> try (AddSpIm <$> (reserved "add" *> sp *> comma *> parserSigned8Int))
            <|> try (AddHL <$> (reserved "add" *> hl *> comma *> parseCombinedReg))

-- Parse 8 bit LD, LDI and LDH instructions       
ld8Stmt :: Parser Ld8Stmt 
ld8Stmt =    try (reserved "ld"  *> ldStmt')
         <|> try (reserved "ldi" *> ldiStmt)
         <|> try (reserved "ldd" *> lddStmt)
         <|> try (reserved "ldh" *> ldhStmt)

ldStmt' :: Parser Ld8Stmt
ldStmt' =
            try (LdRegReg <$> parser8Reg  <* comma <*> parser8Reg)
        <|> try (LdRegIm  <$> parser8Reg  <* comma <*> parserUnsigned8Int)
        <|> try (LdRegMemHl <$> parser8Reg <* comma <* parens hl)
        <|> try (LdAMemCombinedReg <$> (a *> comma *> parens parseCombinedReg))
        <|> try (LdMemCombinedRegA <$> parens parseCombinedReg <* comma <* a)
        <|> try (LdAMemIm <$> (a *> comma *> parens parserUnsigned16Int))

        <|> try (a >> comma >> parens c >> return LDIOAC) 
        <|> try (parens c >> comma >> a >> return LDIOCA)

ldiStmt :: Parser Ld8Stmt
ldiStmt = 
          try (a >> comma >> parens hl >> return LdiAmemHL) 
      <|> try (parens hl >> comma >> a >> return LdimemHLA)

lddStmt :: Parser Ld8Stmt
lddStmt =
          try (a >> comma >> parens hl >> return LddAmemHL) 
      <|> try (parens hl >> comma >> a >> return LddmemHLA)

ldhStmt :: Parser Ld8Stmt
ldhStmt = 
           try (LdhAIO <$> (a *> comma *> parens parserSigned8Int))
      <|>  try (LdhIOA <$> parens parserSigned8Int <* comma <* a)


-- Parse 16 bit LD instructions
ld16Stmt :: Parser Ld16Stmt
ld16Stmt = try (reserved "ld" *> ld16Stmt')

ld16Stmt' :: Parser Ld16Stmt
ld16Stmt' =    try (LdCombinedRegIm <$> (parseCombinedReg <* comma) <*> parserUnsigned16Int)
           <|> try (LdMemSp <$> parserUnsigned16Int <* comma <* sp)
           <|> try (sp >> comma >> hl >> return LdSpHl)
           <|> try (LdHLSpPlusIm <$> (hl >> comma >> parens (sp >> (reserved "+") >> parserSigned8Int)))
           <|> try (Push <$> parseStackReg)
           <|> try (Pop  <$> parseStackReg)

-- Parser JP and JR instructions
jmpStmt :: Parser JmpStmt
jmpStmt = try (reserved "jp" *> jpStmt)
      <|> try (reserved "jr" *> jrStmt)

jpStmt :: Parser JmpStmt
jpStmt =    try (JmpCond <$> parserCondition <* comma <*> parserUnsigned16Int) 
        <|> try (JmpIm <$> parserUnsigned16Int)

        <|> try (parens hl >> return JmpHL)


jrStmt :: Parser JmpStmt
jrStmt =   try (JmpRelCond <$> parserCondition <* comma <*> parserSigned8Int)
       <|> try (JmpRel     <$> parserSigned8Int)

--Miscellaneous Instructions
miscellaneousStmt :: Parser MiscellaneousStmt
miscellaneousStmt =    try (reserved "nop"  >> return NOP)
                   <|> try (reserved "di"   >> return DI)
                   <|> try (reserved "ei"   >> return EI)
                   <|> try (reserved "swap" >> swap)
                   <|> try (reserved "daa"  >> return DAA)
                   <|> try (reserved "cpl"  >> return CPL)
                   <|> try (reserved "ccf"  >> return CCF)
                   <|> try (reserved "scf"  >> return SCF)
                   <|> try (reserved "halt" >> return HALT)
                   <|> try (reserved "stop" >> return STOP)
                   
                    

swap :: Parser MiscellaneousStmt
swap = try (SwapReg <$> parser8Reg)
     <|> try (parens hl >> return SwapMemHL)
       
parserCondition :: Parser Condition
parserCondition = try (reserved "nz" >> return NotZero)
              <|> try (reserved "z"  >> return Zero) 
              <|> try (reserved "nc" >> return NoCarry)
              <|> try (reserved "c"  >> return Carry)


parser8Reg :: Parser Register
parser8Reg = (reserved "a" >> return A) 
         <|> (reserved "b" >> return B)
         <|> (reserved "c" >> return C)
         <|> (reserved "d" >> return D)
         <|> (reserved "e" >> return E)     
         <|> (reserved "h" >> return H)
         <|> (reserved "l" >> return L)


parseCombinedReg :: Parser CombinedRegister
parseCombinedReg = (reserved "bc" >> return BC)
               <|> (reserved "de" >> return DE)
               <|> (reserved "hl" >> return HL)
               <|> (reserved "sp" >> return SP)

parseStackReg :: Parser StackRegister
parseStackReg =    (reserved "af" >> return StackRegAF)
               <|> (reserved "bc" >> return StackRegBC)
               <|> (reserved "de" >> return StackRegDE)
               <|> (reserved "hl" >> return StackRegHL)
          



parserInteger :: Parser Integer
parserInteger = whiteSpace *> Token.lexeme lexer integer

-- Parse 8 bit signed integer -128 - 127
parserSigned8Int :: Parser Integer
parserSigned8Int = parserInteger >>= \num ->
                   if num >= -128 && num <= 127
                   then return num 
                   else mzero

-- Parse 8 bit unsigned integer 0 - 255
parserUnsigned8Int :: Parser Integer
parserUnsigned8Int = parserInteger >>= \num ->
                     if num <= 255 && num >= 0
                     then return num
                     else mzero


-- parser 16 bit unsigned integer 0 - 65---
parserUnsigned16Int :: Parser Integer
parserUnsigned16Int = parserInteger >>= \num ->
                      if num >= 0 && num <= 0xFFFF
                      then return num
                      else mzero  
