-- PFL 2023/24 - Haskell practical Assign quickstart - Luis Relvas, Valter Ochoa - 30/12/2023

-- Part 1
import Debug.Trace
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.List (intersperse, intercalate, sort)
import Data.Char (toLower)
import Data.List (isPrefixOf, find)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.Function (on)
import Data.List (intercalate, sortBy) -- Import the intercalate function from Data.List module
import Data.Ord (comparing) -- Import the comparing function from Data.Ord module
import Data.Char (isDigit, isSpace, digitToInt, isAlpha, isAlphaNum, isLower)
import qualified Data.Text as T
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

-- Define the type for the stack
type Stack = [Either Integer Bool]

-- Define the type for the state
type State = [(String, Either Integer Bool)]

-- Function to create an empty stack
createEmptyStack :: Stack
createEmptyStack = [] 

-- Function to convert a stack to a string
stack2Str :: Stack -> String
stack2Str stack = concat $ intersperse "," $ map showEither stack
  where showEither (Left i) = show i
        showEither (Right b) = show b

-- Function to create an empty state
createEmptyState :: State
createEmptyState = [] 

-- Function to convert a state to a string
state2Str :: State -> String
state2Str state = intercalate "," $ sort $ map showPair state
  where showPair (var, val) = var ++ "=" ++ showEither val
        showEither (Left i) = show i
        showEither (Right b) = show b

--- The run function takes a tuple of Code, Stack, and State and returns a tuple of Code, Stack, and State
run :: (Code, Stack, State) -> (Code, Stack, State)

-- If there are no instructions left, return the current stack and state
run ([], stack, state) = ([], stack, state)

-- Push an integer onto the stack
run ((Push i):code, stack, state) = run (code, (Left i):stack, state)

-- Add the top two integers on the stack
run ((Add):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 + i2)):stack, state)

-- If the Add instruction is not followed by two integers, throw a run-time error
run ((Add):code, _, _) = error "Run-time error"

-- MultA the top two integers on the stack
run ((Mult):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 * i2)):stack, state)

-- If the Mult instruction is not followed by two integers, throw a run-time error
run ((Mult):code, _, _) = error "Run-time error"

-- SubA the second integer on the stack from the top integer
run ((Sub):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 - i2)):stack, state)

-- If the Sub instruction is not followed by two integers, throw a run-time error
run ((Sub):code, _, _) = error "Run-time error"

-- Push True onto the stack
run ((Tru):code, stack, state) = run (code, (Right True):stack, state)

-- Push False onto the stack
run ((Fals):code, stack, state) = run (code, (Right False):stack, state)

-- Compare the top two booleans on the stack for equality
run ((Equ):code, (Right b1):(Right b2):stack, state) = run (code, (Right (b1 == b2)):stack, state)

-- Compare the top two integers on the stack for equality
run ((Equ):code, (Left i1):(Left i2):stack, state) = run (code, (Right (i1 == i2)):stack, state)

-- If the Equ instruction is not followed by two of the same type, throw a run-time error
run ((Equ):code, _, _) = error "Run-time error"

-- Check if the second integer on the stack is less than or Equ2 to the top integer
run ((Le):code, (Left i1):(Left i2):stack, state) = run (code, (Right (i1 <= i2)):stack, state)

-- If the Le instruction is not followed by two integers, throw a run-time error
run ((Le):code, _, _) = error "Run-time error"

-- Perform a logical and on the top two booleans on the stack
run ((And):code, (Right b1):(Right b2):stack, state) = run (code, (Right (b1 && b2)):stack, state)

-- If the And instruction is not followed by two booleans, throw a run-time error
run ((And):code, _, _) = error "Run-time error"

-- Negate the top boolean on the stack
run ((Neg):code, (Right b):stack, state) = run (code, (Right (not b)):stack, state)

-- If the Neg instruction is not followed by a boolean, throw a run-time error
run ((Neg):code, _, _) = error "Run-time error"

-- Fetch the value of a variable and push it onto the stack
run ((Fetch var):code, stack, state) = case lookup var state of
  Just val -> run (code, val:stack, state)
  Nothing -> error "Run-time error"

-- Store the top value on the stack in a variable
run ((Store var):code, (val:stack), state) = run (code, stack, (var, val):(filter ((/= var) . fst) state))

-- If the Store instruction is not followed by a value, throw a run-time error
run ((Store var):code, _, _) = error "Run-time error"

-- Branch based on the top boolean on the stack
run ((Branch code1 code2):code, (Right b):stack, state) = if b then run (code1 ++ code, stack, state) else run (code2 ++ code, stack, state)

-- If the Branch instruction is not followed by a boolean, throw a run-time error
run ((Branch code1 code2):code, _, _) = error "Run-time error"

-- Loop while the condition code evaluates to True
run ((Loop cond body):code, stack, state) = 
  case run (cond, stack, state) of
    (_, (Right b):stack', state') -> 
      if b 
      then case run (body, stack', state') of
        (_, stack'', state'') -> run ((Loop cond body):code, stack'', state'')
      else run (code, stack', state')
    _ -> error "Run-time error" --  error handling for Loop


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


data Aexp
  = IntLit Integer
  | Var String
  | AddA Aexp Aexp
  | SubA Aexp Aexp
  | MultA Aexp Aexp
  deriving (Show, Eq)

data Bexp
  = BoolLit Bool
  | Aexp Aexp
  | Equ2 Bexp Bexp
  | EquB Bexp Bexp
  | LeB Bexp Bexp
  | NegB Bexp
  | AndB Bexp Bexp
  deriving (Show, Eq)

data Stm
  = Assign String Aexp
  | LoopW Bexp [Stm]
  | BranchIf Bexp [Stm] [Stm]
  deriving (Show)

type Program = [Stm]

compile :: [Stm] -> Code
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm stm = case stm of
  Assign var aexp -> compA aexp ++ [Store var]                                  
  BranchIf bexp stm1 stm2  -> compB bexp ++ [Branch (compile stm1) (compile stm2)]  
  LoopW bexp stm -> [Loop (compB bexp) (compile stm)]                        

compA :: Aexp -> Code
compA (IntLit n) = [Push n]
compA (Var var) = [Fetch var] 
compA (AddA e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (SubA e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (MultA e1 e2) = compA e2 ++ compA e1 ++ [Mult]

compB :: Bexp -> Code
compB (BoolLit True) = [Tru]
compB (BoolLit False) = [Fals]
compB (Equ2 e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (EquB e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (LeB e1 e2) = compB e2 ++ compB e1 ++ [Le]
compB (NegB b) = compB b ++ [Neg]
compB (AndB b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Aexp e) = compA e

--Functions taken from the slides with some changes 

-- parseIntOrParenExpr tries to parse an integer literal, a variable, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLit n, restTokens)
parseIntOrParenExpr (OpenP : restTokens1)
  = case parseSumOrProdOrSubOrIntOrPar restTokens1 of
    Just (expr, (CloseP : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing 
    Nothing -> Nothing
parseIntOrParenExpr (VarTok var : restTokens)
  = Just (Var var, restTokens)
parseIntOrParenExpr tokens = Nothing

-- parseProdOrIntOrPar tries to parse a product of expressions, an integer literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens
  = case parseIntOrParenExpr tokens of
    Just (expr1, (TimesTok : restTokens1)) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MultA expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) -> Just (expr, SemicolonTok : restTokens)
    result -> result

-- parseSumOrProdOrSubOrIntOrPar tries to parse a sum, a difference, a product, an integer literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseSumOrProdOrSubOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrSubOrIntOrPar tokens
  = case parseProdOrIntOrPar tokens of
    Just (expr1, (PlusTok : restTokens1)) ->
      case parseSumOrProdOrSubOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AddA expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (MinusTok : restTokens1)) ->
      case parseSumOrProdOrSubOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (SubA expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) ->
      Just (expr, [SemicolonTok] ++ restTokens)  
    result -> result

-- parseArithmeticExp tries to parse an arithmetic expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseArithmeticExp :: [Token] -> Maybe (Aexp, [Token])
parseArithmeticExp = parseSumOrProdOrSubOrIntOrPar

-- parseBoolOrParenExpr tries to parse a boolean literal or a parenthesized boolean expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseBoolOrParenExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParenExpr (BoolTok n : restTokens) = Just (BoolLit n, restTokens)
parseBoolOrParenExpr (OpenP : restTokens) = parseBexp restTokens >>= checkCloseP
parseBoolOrParenExpr _ = Nothing

-- checkCloseP checks if the next token is a closing parenthesis.
-- If it is, it returns 'Just' the parsed expression and the remaining tokens, otherwise it returns 'Nothing'.
checkCloseP :: (Bexp, [Token]) -> Maybe (Bexp, [Token])
checkCloseP (expr, CloseP : restTokens) = Just (expr, restTokens)
checkCloseP _ = Nothing

-- parseIneorBoolorParExpr tries to parse a less than or equal to expression, a boolean literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseArithorBoolorParExpr :: [Token] -> Maybe (Bexp, [Token])
parseArithorBoolorParExpr tokens = 
  case parseBoolOrParenExpr tokens of
    Nothing -> parseArithmeticExp tokens >>= toAexp
    justVal -> justVal
  where
    toAexp (expr, restTokens) = Just (Aexp expr, restTokens)

-- parseIneorBoolorParExpr tries to parse a less than or equal to expression, a boolean literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseIneorBoolorParExpr :: [Token] -> Maybe (Bexp, [Token])
parseIneorBoolorParExpr = parseBinaryExpr LeTok LeB parseArithorBoolorParExpr

-- parseArithBoolExpr tries to parse an equality expression, a less than or equal to expression, a boolean literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseArithBoolExpr :: [Token] -> Maybe (Bexp, [Token])
parseArithBoolExpr = parseBinaryExpr EqTok Equ2 parseIneorBoolorParExpr

-- parseBexpByHalf tries to parse a negated expression, an equality expression, a less than or equal to expression, a boolean literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseBexpByHalf :: [Token] -> Maybe (Bexp, [Token])
parseBexpByHalf (NegBok: restTokens) = parseArithBoolExpr restTokens >>= toNegB
  where
    toNegB (expr, restTokens) = Just (NegB expr, restTokens)
parseBexpByHalf tokens = parseArithBoolExpr tokens

-- parseBoolExpr tries to parse a boolean equality expression, a negated expression, an equality expression, a less than or equal to expression, a boolean literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseBoolExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolExpr = parseBinaryExpr EquBTok EquB parseBexpByHalf

-- parseBexp tries to parse a boolean and expression, a boolean equality expression, a negated expression, an equality expression, a less than or equal to expression, a boolean literal, or a parenthesized expression.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp = parseBinaryExpr AndTok AndB parseBoolExpr

-- parseBinaryExpr tries to parse a binary expression.
-- It takes a token, a constructor function, a parsing function, and a list of tokens.
-- It returns 'Just' the parsed expression and the remaining tokens, or 'Nothing' if parsing fails.
parseBinaryExpr :: Token -> (Bexp -> Bexp -> Bexp) -> ([Token] -> Maybe (Bexp, [Token])) -> [Token] -> Maybe (Bexp, [Token])
parseBinaryExpr tok constructor parseFn tokens = case parseFn tokens of
  Just (expr1, (tok' : restTokens1)) | tok' == tok ->
    parseFn restTokens1 >>= toBinaryExpr expr1
  Just (expr, (SemicolonTok : restTokens)) -> Just (expr, SemicolonTok : restTokens)
  result -> result
  where
    toBinaryExpr expr1 (expr2, restTokens2) = Just (constructor expr1 expr2, restTokens2)

-- Token is a data type representing the different types of tokens that can be parsed.
data Token
  = PlusTok
  | MinusTok
  | TimesTok
  | OpenP
  | CloseP
  | IntTok Integer
  | VarTok String
  | BoolTok Bool
  | AssignTok
  | SemicolonTok
  | IfTok
  | ThenTok
  | ElseTok
  | WhileTok
  | DoTok
  | NegBok
  | AndTok
  | EqTok
  | EquBTok
  | LeTok
  deriving (Show, Eq)

-- lexer takes a string and returns a list of tokens.
lexer :: String -> [Token]
lexer [] = []
lexer str = case findToken str tokenMap of
  Just (token, restStr) -> token : lexer restStr
  Nothing -> handleDefaultCase str

-- handleDefaultCase handles the case when a token is not found in the token map.
handleDefaultCase :: String -> [Token]
handleDefaultCase (chr : restStr)
  | isSpace chr = lexer restStr
  | isAlpha chr && isLower chr = lexVar chr restStr
  | isDigit chr = lexInt (chr : restStr)
  | otherwise = error ("unexpected character: '" ++ show chr ++ "'")

-- tokenMap is a list of pairs mapping strings to their corresponding tokens.
tokenMap :: [(String, Token)]
tokenMap =
  [ ("+", PlusTok)
  , ("*", TimesTok)
  , ("-", MinusTok)
  , ("(", OpenP)
  , (")", CloseP)
  , (";", SemicolonTok)
  , (":=", AssignTok)
  , ("if", IfTok)
  , ("then", ThenTok)
  , ("else", ElseTok)
  , ("while", WhileTok)
  , ("do", DoTok)
  , ("not", NegBok)
  , ("and", AndTok)
  , ("==", EqTok)
  , ("=", EquBTok)
  , ("<=", LeTok)
  , ("True", BoolTok True)
  , ("False", BoolTok False)
  ]

-- findToken tries to find a token in the token map that matches the beginning of a string.
findToken :: String -> [(String, Token)] -> Maybe (Token, String)
findToken str [] = Nothing
findToken str ((tokenStr, token):rest) =
  if tokenStr `isPrefixOf` str
    then Just (token, drop (length tokenStr) str)
    else findToken str rest

-- lexVar lexes a variable token.
lexVar :: Char -> String -> [Token]
lexVar chr restStr = VarTok varName : lexer restStr'
  where
    varName = chr : takeWhile (`notElem` reservedSymbols) restStr
    restStr' = dropWhile (`notElem` reservedSymbols) restStr
    reservedSymbols = "+-*();:=<> "

-- lexInt lexes an integer token.
lexInt :: String -> [Token]
lexInt str = IntTok (stringToInt digitStr) : lexer restStr
  where
    (digitStr, restStr) = break (not . isDigit) str
    stringToInt = foldl (\acc chr -> 10 * acc + toInteger (digitToInt chr)) 0

-- parseStms tries to parse a list of statements.
-- It returns 'Just' the parsed program and the remaining tokens, or 'Nothing' if parsing fails.
parseStms :: [Token] -> Maybe (Program, [Token])
parseStms [] = Just ([], [])
parseStms (OpenP : restTokens1) = parseOpenPCase restTokens1
parseStms tokens = parseDefaultCase tokens

-- Parses a sequence of statements enclosed in parentheses
parseOpenPCase :: [Token] -> Maybe (Program, [Token])
parseOpenPCase tokens = 
  case parseStms tokens of
    Just (stmts, (CloseP : SemicolonTok : restTokens2)) -> parseRemainingStms stmts restTokens2
    _ -> Nothing 

-- Parses a single statement
parseDefaultCase :: [Token] -> Maybe (Program, [Token])
parseDefaultCase tokens = 
  case parseStm tokens of
    Just (stmt, restTokens) -> parseRemainingStms [stmt] restTokens
    Nothing -> Nothing

-- Parses remaining statements
parseRemainingStms :: Program -> [Token] -> Maybe (Program, [Token])
parseRemainingStms stmts tokens = 
  case parseStms tokens of
    Just (stmts2, finalRestTokens) -> Just (stmts ++ stmts2, finalRestTokens)
    _ -> Just (stmts, tokens)

-- Parses a statement based on the first token
parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm (VarTok var : AssignTok : restTokens1) = parseAssign var restTokens1
parseStm (IfTok : restTokens1) = parseIf restTokens1
parseStm (WhileTok : restTokens1) = parseWhile restTokens1
parseStm _ = Nothing

-- Parses an assignment statement
parseAssign :: String -> [Token] -> Maybe (Stm, [Token])
parseAssign var tokens =
  case parseSumOrProdOrSubOrIntOrPar tokens of
    Just (expr, SemicolonTok : restTokens) -> Just (Assign var expr, restTokens)
    _ -> Nothing

-- Parses an if statement
parseIf :: [Token] -> Maybe (Stm, [Token])
parseIf tokens =
  case parseBexp tokens of
    Just (expr, ThenTok : restTokens1) ->
      case parseStmsThen restTokens1 of
        Just (stmts1, ElseTok : restTokens2) ->
          case parseStmsElse restTokens2 of
            Just (stmts2, finalRestTokens) -> Just (BranchIf expr stmts1 stmts2, finalRestTokens)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- Parses a while statement
parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile tokens =
  case parseBexp tokens of
    Just (expr, DoTok : OpenP: restTokens1) ->
      case parseStms restTokens1 of
        Just (stmts, CloseP: SemicolonTok: finalRestTokens) -> Just (LoopW expr stmts, finalRestTokens)
        _ -> Nothing
    _ -> Nothing

-- Parses statements in the then branch of an if statement
parseStmsThen :: [Token] -> Maybe (Program, [Token])
parseStmsThen (OpenP : restTokens1) = parseOpenPCaseThen restTokens1
parseStmsThen tokens = parseDefaultCaseThen tokens

-- Parses statements in the else branch of an if statement
parseStmsElse :: [Token] -> Maybe (Program, [Token])
parseStmsElse (OpenP : restTokens1) = parseOpenPCaseElse restTokens1
parseStmsElse tokens = parseDefaultCaseElse tokens

-- Parses a sequence of statements enclosed in parentheses in the then branch
parseOpenPCaseThen :: [Token] -> Maybe (Program, [Token])
parseOpenPCaseThen tokens =
  case parseStms tokens of
    Just (stmts, CloseP : restTokens) -> Just (stmts, restTokens)
    _ -> Nothing

-- Parses a single statement in the then branch
parseDefaultCaseThen :: [Token] -> Maybe (Program, [Token])
parseDefaultCaseThen tokens =
  case parseStm tokens of
    Just (stmt, restTokens) -> Just ([stmt], restTokens)
    _ -> Nothing

-- Parses a sequence of statements enclosed in parentheses in the else branch
parseOpenPCaseElse :: [Token] -> Maybe (Program, [Token])
parseOpenPCaseElse tokens =
  case parseStms tokens of
    Just (stmts, CloseP : SemicolonTok : restTokens) -> Just (stmts, restTokens)
    _ -> Nothing

-- Parses a single statement in the else branch
parseDefaultCaseElse :: [Token] -> Maybe (Program, [Token])
parseDefaultCaseElse tokens =
  case parseStm tokens of
    Just (stmt, restTokens) -> Just ([stmt], restTokens)
    _ -> Nothing

-- Main parser function
parser :: [Token] -> Program
parser tokens = parseOrThrowError tokens

-- Parses a sequence of statements or throws an error
parseOrThrowError :: [Token] -> Program
parseOrThrowError tokens =
  case parseStms tokens of
    Just (expr, []) -> expr
    _ -> error "Parser error"

-- parse is the main function to parse a string into a program.
-- It first lexes the string into tokens using 'lexer', then parses the tokens into a program using 'parser'.
parse :: String -> Program
parse expr = parser (lexer expr)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)


-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")  
-- testParser "x := 0 - 2;" == ("","x=-2") 
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2") 
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")