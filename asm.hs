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

data Stmt =  Alu8 Alu8Stmt | Alu16 Alu16Stmt |  Ld8 Ld8Stmt | Ld16 Ld16Stmt | Jmp JmpStmt
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

data Condition = NotZero | Zero | NoCarry | Carry deriving (Show)

data Register = A | B | C | D | E | H | L  deriving (Show)

data CombinedRegister = BC | DE | HL | SP  deriving (Show) 

data StackRegister = StackRegAF | StackRegBC | StackRegDE | StackRegHL deriving (Show)

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
             <|> (Ld8  <$> try ld8Stmt)
             <|> (Ld16 <$> try ld16Stmt)
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





