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

data Stmt =  Stmt AluStmt
             deriving (Show)

data AluStmt = AluReg AluOp Register 
             | AluImm AluOp Integer
             | AluMem MemRegister 
               deriving (Show)

data LdStmt = LdRegReg Register Register 
            | LdRegIm  Register Integer
            | LdRegMem Register MemRegister
            | LdMemReg MemRegister Register

data Register = A | B | C | D | E | H | L  deriving (Show)

data CombinedRegister = AF | BC | DE | HL | SP | PC deriving (Show) 

data MemRegister = MemAF | MemBC | MemDE | MemHL | MemHLPlus | MemHLMinus deriving (Show)

data AluOp = Add 
           | Adc 
           | Sub 
           | Sbc 
           | Mul 
           | Div
           | And 
           | Or 
           | Xor 
           | Cmp 
             deriving (Show)

--data AluStmt = AluStmt AluOp Register deriving (Show)         


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
statement = Stmt <$> alu8Stmt

alu8Stmt :: Parser AluStmt
alu8Stmt = try parserAluOp >>= alu8Stmt'

alu8Stmt' :: AluOp -> Parser AluStmt
alu8Stmt' op = 
        (try ((AluReg op) <$> parser8Reg))
       <|> (try ((AluImm op) <$> parserSigned8Int))
       <?> "register or 8 bit signed integer"  
       
        
parserAluOp :: Parser AluOp
parserAluOp =   try (reserved "add" >> return Add)
            <|> try (reserved "adc" >> return Adc)
            <|> try (reserved "sub" >> return Sub)
            <|> try (reserved "sbc" >> return Sbc)
            <|> try (reserved "cmp" >> return Cmp)
            <|> try (reserved "mul" >> return Mul)
            <|> try (reserved "div" >> return Div)
            <|> try (reserved "and" >> return And)
            <|> try (reserved "or"  >> return Or)
            <|> try (reserved "xor" >> return Xor)

parser8Reg :: Parser Register
parser8Reg = (reserved "a" >> return A) 
         <|> (reserved "b" >> return B)
         <|> (reserved "c" >> return C)
         <|> (reserved "d" >> return D)
         <|> (reserved "e" >> return E)     
         <|> (reserved "h" >> return H)
         <|> (reserved "l" >> return L)
         <|> (reserved "(hl)" >> return MemHL)


--parseMemReg :: Parser MemRegister
--parseMemReg 

parserInteger :: Parser Integer
parserInteger = whiteSpace *> Token.lexeme lexer integer

parserSigned8Int :: Parser Integer
parserSigned8Int = parserInteger >>= \num ->
                   if (num >= -128 && num <= 127)
                   then return num 
                   else mzero

