{- Parser and Lexer -}
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Error;
import Control.Applicative hiding ((<|>), many, optional, empty)

data Program = Program [Stmt] deriving (Show)

data Stmt =  Alu8 Alu8Stmt | Alu16 Alu16Stmt |  Ld LdStmt | Jmp JmpStmt
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

data LdStmt = LdRegReg Register Register 
            | LdRegIm  Register Integer
            | LdRegMem Register MemRegister
            | LdMemReg MemRegister Register
            | LdMemSP Integer
            | LdCombinedRegIm CombinedRegister Integer
            | LdiAmemHL
            | LdimemHLA
            | LddAmemHL
            | LddmemHLA
            | LDIOAC   
            | LDIOCA    
            | LdhIOA Integer
            | LdhAIO Integer 
              deriving (Show)

data JmpStmt = JmpIm Integer -- Jump to 16 bit immediate address
             | JmpCond Condition Integer -- Jump if Condition is met
             | JmpHL -- Jump to address contained in register HL
             | JmpRel Integer -- Jump to 8 bit address relative to PC
             | JmpRelCond Condition Integer -- Relative Jump if Condition is met
               deriving (Show) 

data Condition = NotZero | Zero | NoCarry | Carry deriving (Show)

data Register = A | B | C | D | E | H | L  deriving (Show)

data CombinedRegister = AF | BC | DE | HL | SP  deriving (Show) 

data MemRegister = MemAF | MemBC | MemDE | MemHL deriving (Show)

data Alu8Op = Add | Adc | Sub | Sbc | And | Or | Xor | Cmp | Inc | Dec deriving (Show)


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

run :: Show a => Parser a -> String -> IO ()
run p input
        = case parse p "" input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x


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
             <|> (Ld  <$> try ldStmt)
             <|> (Jmp <$> try jmpStmt)


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
parserAluOp =   try (reserved "add" >> a >> return Add)
            <|> try (reserved "adc" >> a >> return Adc)
            <|> try (reserved "sub" >> return Sub)
            <|> try (reserved "sbc" >> a >> return Sbc)
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

-- Parse LD X,X instructions       
ldStmt :: Parser LdStmt 
ldStmt =     try (reserved "ld" *> ldStmt')
         <|> try (reserved "ldi" *> ldiStmt)
         <|> try (reserved "ldd" *> lddStmt)
         <|> try (reserved "ldh" *> ldhStmt)
ldStmt' :: Parser LdStmt
ldStmt' =
            try (LdRegReg <$> parser8Reg  <* comma <*> parser8Reg)
        <|> try (LdRegIm  <$> parser8Reg  <* comma <*> parserUnsigned8Int)
        <|> try (LdMemReg <$> parseMemReg <* comma <*> parser8Reg)
        <|> try (LdRegMem <$> parser8Reg  <* comma <*> parseMemReg)
        <|> try (LdCombinedRegIm <$> parseCombinedReg <* comma <*> parserUnsigned16Int)
        <|> try (LdMemSP <$> parens parserUnsigned16Int <* comma <* reserved "sp")

        <|> try (a >> comma >> parens c >> return LDIOAC) 
        <|> try (parens c >> comma >> a >> return LDIOCA)

ldiStmt :: Parser LdStmt
ldiStmt = 
          try (a >> comma >> parens hl >> return LdiAmemHL) 
      <|> try (parens hl >> comma >> a >> return LdimemHLA)

lddStmt :: Parser LdStmt
lddStmt =
          try (a >> comma >> parens hl >> return LddAmemHL) 
      <|> try (parens hl >> comma >> a >> return LddmemHLA)
        

ldhStmt :: Parser LdStmt
ldhStmt = 
           try (LdhAIO <$> (a *> comma *> parens parserSigned8Int))
      <|>  try (LdhIOA <$> parens parserSigned8Int <* comma <* a)

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
parseCombinedReg = (reserved "af" >> return AF)
               <|> (reserved "bc" >> return BC)
               <|> (reserved "de" >> return DE)
               <|> (reserved "hl" >> return HL)
               <|> (reserved "sp" >> return SP)

parseMemReg :: Parser MemRegister
parseMemReg =   parens $ 
                try (reserved "af" >> return MemAF)
            <|> try (reserved "bc" >> return MemBC)
            <|> try (reserved "de" >> return MemDE)
            <|> try (reserved "hl" >> return MemHL)
            

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





