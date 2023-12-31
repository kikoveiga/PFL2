import Data.List (intercalate, sortOn, isInfixOf)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [Either Integer Bool]
type State = [(String, Either Integer Bool)]


createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map (either show show) stack)

state2Str :: State -> String
state2Str state = intercalate "," (map (\(x,y) -> x ++ "=" ++ either show show y) (sortOn fst state))

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Push x:code, stack, state) = run (code, Left x:stack, state)

run (Add:code, Left x:Left y:stack, state) = run (code, Left (x+y):stack, state)
run (Add:code, _, _) = error "Run-time error"

run (Mult:code, Left x:Left y:stack, state) = run (code, Left (x*y):stack, state)
run (Mult:code, _, _) = error "Run-time error"

run (Sub:code, Left x:Left y:stack, state) = run (code, Left (x-y):stack, state)
run (Sub:code, _, _) = error "Run-time error"

run (Tru:code, stack, state) = run (code, Right True:stack, state)
run (Fals:code, stack, state) = run (code, Right False:stack, state)

run (Equ:code, Left x:Left y:stack, state) = run (code, Right (x==y):stack, state)
run (Equ:code, Right x:Right y:stack, state) = run (code, Right (x==y):stack, state)
run (Equ:code, _, _) = error "Run-time error"

run (Le:code, Left x:Left y:stack, state) = run (code, Right (x<=y):stack, state)
run (Le:code, _, _) = error "Run-time error"

run (And:code, Right x:Right y:stack, state) = run (code, Right (x && y):stack, state)
run (And:code, _, _) = error "Run-time error"

run (Neg:code, Right x:stack, state) = run (code, Right (not x):stack, state)
run (Neg:code, _, _) = error "Run-time error"

run (Fetch x:code, stack, state) = run (code, maybe (error "Run-time error") (:stack) (lookup x state), state)
run (Store x:code, y:stack, state) = run (code, stack, (x, y) : filter ((/= x) . fst) state)
run (Noop:code, stack, state) = run (code, stack, state)

run (Branch code1 code2:code, Right True:stack, state) = run (code1 ++ code, stack, state)
run (Branch code1 code2:code, Right False:stack, state) = run (code2 ++ code, stack, state)
run (Branch code1 code2:code, _, _) = error "Run-time error"

run (Loop code1 code2:code, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)


--To help you test your assembler
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

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving Show

data Bexp = Bool Bool | EqA Aexp Aexp | LeA Aexp Aexp | NegB Bexp | AndB Bexp Bexp deriving Show

data Stm = Assign String Aexp | Skip | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (Num x) = [Push x]
compA (Var x) = [Fetch x]
compA (AddA x y) = compA x ++ compA y ++ [Add]
compA (SubA x y) = compA x ++ compA y ++ [Sub]
compA (MultA x y) = compA x ++ compA y ++ [Mult]

compB :: Bexp -> Code
compB (Bool x) = if x then [Tru] else [Fals]
compB (EqA x y) = compA x ++ compA y ++ [Equ]
compB (LeA x y) = compA x ++ compA y ++ [Le]
compB (NegB x) = compB x ++ [Neg]
compB (AndB x y) = compB x ++ compB y ++ [And]

compile :: Program -> Code
compile [] = []
compile (Assign x y:xs) = compA y ++ [Store x] ++ compile xs
compile (Skip:xs) = Noop:compile xs
compile (Comp x y:xs) = compile (x:y:xs)
compile (If x y z:xs) = compB x ++ [Branch (compile (y:xs)) (compile (z:xs))]
compile (While x y:xs) = Loop (compB x ++ compile (y:xs)) [Noop] : compile xs


parse :: String -> Program
parse = buildData . lexer . filter (/= ' ') . filter (/= '\n')

buildData :: [String] -> Program
buildData [] = []
buildData (x:xs)
  | x == "if" = let (ifStm, rest) = buildIf xs in ifStm : buildData rest
  | x == "while" = let (whileStm, rest) = buildWhile xs in whileStm : buildData rest
  | otherwise = let (assignStm, rest) = buildAssign (x:xs) in assignStm : buildData rest

buildIf :: [String] -> (Stm, [String])
buildIf [] = error "Parse error 1"
buildIf (x:xs)
  | x == "(" = let (bexp, rest) = buildBexp xs in
    if head rest == ")" then
      let (stm1, rest1) = buildStm (tail rest) in
        if head rest1 == "else" then
          let (stm2, rest2) = buildStm (tail rest1) in
            (If bexp stm1 stm2, rest2)
        else error "Parse error 2"
    else error "Parse error 3"
  | otherwise = error "Parse error 4"

buildWhile :: [String] -> (Stm, [String])
buildWhile [] = error "Parse error 5"
buildWhile (x:xs)
  | x == "(" = let (bexp, rest) = buildBexp xs in
    if head rest == ")" then
      let (stm, rest1) = buildStm (tail rest) in
        (While bexp stm, rest1)
    else error "Parse error 6"
  | otherwise = error "Parse error 7"

buildAssign :: [String] -> (Stm, [String])
buildAssign [] = error "Parse error 8"
buildAssign (variable:":=":aexp)
  | head variable `elem` ['a'..'z'] = let (result, rest) = buildAexp aexp in (Assign variable result, rest)
  | head variable `elem` ['0'..'9'] = error "Parse error 10"
  | otherwise = error "Parse error 11"

buildStm :: [String] -> (Stm, [String])
buildStm [] = error "Parse error 12"
buildStm (x:xs)
  | x == "if" = buildIf xs
  | x == "while" = buildWhile xs
  | head x `elem` ['a'..'z'] = buildAssign (x:xs)
  | otherwise = error "Parse error 13"

buildBexp :: [String] -> (Bexp, [String])
buildBexp [] = error "Parse error 14"
buildBexp (x:xs)
  | x == "not" = let (bexp, rest) = buildBexp xs in (NegB bexp, rest)
  | x == "True" = (Bool True, xs)
  | x == "False" = (Bool False, xs)
  | x == "(" = let (aexp1, rest) = buildAexp xs in
    if head rest == "==" then
      let (aexp2, rest1) = buildAexp (tail rest) in
        if head rest1 == ")" then
          (EqA aexp1 aexp2, tail rest1)
        else error "Parse error 15"
    else if head rest == "<=" then
      let (aexp2, rest1) = buildAexp (tail rest) in
        if head rest1 == ")" then
          (LeA aexp1 aexp2, tail rest1)
        else error "Parse error 16"
    else error "Parse error 17"
  | otherwise = error "Parse error 18"

buildAexp :: [String] -> (Aexp, [String])
buildAexp [] = error "Parse error 19"


testRead :: IO ()
testRead = do
  print $ lexerhelper "x := 5; x := x - 1;"
  print $ lexerhelper "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
  print $ lexerhelper "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
  print $ lexerhelper "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
  print $ lexerhelper "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
  print $ lexerhelper "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
  print $ lexerhelper "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"


lexerhelper :: String -> [String]
lexerhelper = lexer . filter (/= ' ') . filter (/= '\n')

lexer :: String -> [String]
lexer [] = []
lexer (':':'=':xs) = ":=" : lexer xs
lexer ('=':'=':xs) = "==" : lexer xs
lexer ('<':'=':xs) = "<=" : lexer xs
lexer ('>':'=':xs) = ">=" : lexer xs
lexer ('!':'=':xs) = "!=" : lexer xs
lexer ('=':xs) = "=" : lexer xs
lexer ('(':xs) = "(" : lexer xs
lexer (')':xs) = ")" : lexer xs
lexer ('+':xs) = "+" : lexer xs
lexer ('-':xs) = "-" : lexer xs
lexer ('*':xs) = "*" : lexer xs
lexer (';':xs) = ";" : lexer xs
lexer ('i':'f':xs) = "if" : lexer xs
lexer ('t':'h':'e':'n':xs) = "then" : lexer xs
lexer ('e':'l':'s':'e':xs) = "else" : lexer xs
lexer ('w':'h':'i':'l':'e':xs) = "while" : lexer xs
lexer ('n':'o':'t':xs) = "not" : lexer xs
lexer ('a':'n':'d':xs) = "and" : lexer xs
lexer ('T':'r':'u':'e':xs) = "True" : lexer xs
lexer ('F':'a':'l':'s':'e':xs) = "False" : lexer xs
lexer ('d':'o':xs) = "do" : lexer xs
lexer ('f':'a':'c':'t':xs) = "fact" : lexer xs
lexer (x : xs) | x `elem` ['0'..'9'] = let (num, rest) = span (`elem` ['0'..'9']) (x:xs) in num : lexer rest
lexer (x : xs)
  | x `elem` ['a'..'z'] = 
    let (var, rest) = span (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])) (x:xs) in
      if isValidVar var
        then var : lexer rest 
        else error "Lexical error"
  | otherwise = error "Lexical error"
    
isValidVar :: String -> Bool
isValidVar x = not (any (`isInfixOf` x) ["if", "then", "else", "while", "not", "and", "True", "False", "do"])

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")