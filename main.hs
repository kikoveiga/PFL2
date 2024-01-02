import Data.List (intercalate, sortOn, isInfixOf)
import Debug.Trace (trace)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Stack and State definitions
type Stack = [Either Integer Bool]
type State = [(String, Either Integer Bool)]

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Create an empty state
createEmptyState :: State
createEmptyState = []

-- Convert a stack to a string and print it
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map (either show show) stack)

-- Convert a state to a string and print it
state2Str :: State -> String
state2Str state = intercalate "," (map (\(x,y) -> x ++ "=" ++ either show show y) (sortOn fst state))

-- The run function
run :: (Code, Stack, State) -> (Code, Stack, State)

-- Base case, when the code is empty
run ([], stack, state) = ([], stack, state)

-- Push an integer value onto the stack
run (Push x:code, stack, state) = run (code, Left x:stack, state)

-- Pop two integer values from the stack, add them, and push the result onto the stack. Print an error if the stack does not contain two integer values.
run (Add:code, Left x:Left y:stack, state) = run (code, Left (x+y):stack, state)
run (Add:code, _, _) = error "Run-time error"

-- Pop two integer values from the stack, multiply them, and push the result onto the stack. Print an error if the stack does not contain two integer values.
run (Mult:code, Left x:Left y:stack, state) = run (code, Left (x*y):stack, state)
run (Mult:code, _, _) = error "Run-time error"

-- Pop two integer values from the stack, subtract the first by the second, and push the result onto the stack. Print an error if the stack does not contain two integer values.
run (Sub:code, Left x:Left y:stack, state) = run (code, Left (x-y):stack, state)
run (Sub:code, _, _) = error "Run-time error"

-- Push the boolean values True and False onto the stack
run (Tru:code, stack, state) = run (code, Right True:stack, state)
run (Fals:code, stack, state) = run (code, Right False:stack, state)

-- Pop two boolean values or two integer values from the stack, compare them for equality, and push the result onto the stack. Print an error if the stack does not contain two boolean values or two integer values.
run (Equ:code, Left x:Left y:stack, state) = run (code, Right (x==y):stack, state)
run (Equ:code, Right x:Right y:stack, state) = run (code, Right (x==y):stack, state)
run (Equ:code, _, _) = error "Run-time error"

-- Pop two integer values from the stack, compare them for inequality, and push the result onto the stack. Print an error if the stack does not contain two integer values.
run (Le:code, Left x:Left y:stack, state) = run (code, Right (x<=y):stack, state)
run (Le:code, _, _) = error "Run-time error"

-- Pop two boolean values from the stack, compare them for logical and, and push the result onto the stack. Print an error if the stack does not contain two boolean values.
run (And:code, Right x:Right y:stack, state) = run (code, Right (x && y):stack, state)
run (And:code, _, _) = error "Run-time error"

-- Pop a boolean value from the stack, negate it, and push the result onto the stack. Print an error if the stack does not contain a boolean value.
run (Neg:code, Right x:stack, state) = run (code, Right (not x):stack, state)
run (Neg:code, _, _) = error "Run-time error"

-- Push the value of a variable onto the stack. Print an error if the variable is not in the state.
run (Fetch x:code, stack, state) = run (code, maybe (error "Run-time error") (:stack) (lookup x state), state)

-- Pop a value from the stack and store it in a variable.
run (Store x:code, y:stack, state) = run (code, stack, (x, y) : filter ((/= x) . fst) state)

-- Do nothing.
run (Noop:code, stack, state) = run (code, stack, state)

-- Pop a boolean value from the stack, and execute the first code if the value is True, or the second code if the value is False. Print an error if the stack does not contain a boolean value.
run (Branch code1 code2:code, Right True:stack, state) = run (code1 ++ code, stack, state)
run (Branch code1 code2:code, Right False:stack, state) = run (code2 ++ code, stack, state)
run (Branch code1 code2:code, _, _) = error "Run-time error"

-- Compute a loop.
run (Loop code1 code2:code, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)

-- Part 2

-- Aexp and Bexp definitions
data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving Show
data Bexp = Bool Bool | EqA Aexp Aexp | EqB Bexp Bexp | LeA Aexp Aexp | NegB Bexp | AndB Bexp Bexp deriving Show

-- Stm definition,; Program is defined as a list of Stm.
data Stm = Assign String Aexp | Skip | Seq Stm Stm | If Bexp Stm Stm | While Bexp Stm deriving Show
type Program = [Stm]

-- Compile function for arithmetic expressions
compA :: Aexp -> Code
compA (Num x) = [Push x]
compA (Var x) = [Fetch x]
compA (AddA x y) = compA x ++ compA y ++ [Add]
compA (SubA x y) = compA x ++ compA y ++ [Sub]
compA (MultA x y) = compA x ++ compA y ++ [Mult]

-- Compile function for boolean expressions
compB :: Bexp -> Code
compB (Bool x) = if x then [Tru] else [Fals]
compB (EqA x y) = compA x ++ compA y ++ [Equ]
compB (EqB x y) = compB x ++ compB y ++ [Equ]
compB (LeA x y) = compA y ++ compA x ++ [Le]
compB (NegB x) = compB x ++ [Neg]
compB (AndB x y) = compB x ++ compB y ++ [And]

-- Compile function for statements
compile :: Program -> Code
compile [] = []
compile (Assign x aexp:xs) = compA aexp ++ [Store x] ++ compile xs
compile (Skip:xs) = Noop:compile xs
compile (Seq instr1 instr2:xs) = compile (instr1:instr2:xs)
compile (If bexp thenStm elseStm:xs) = compB bexp ++ [Branch (compile (thenStm:xs)) (compile (elseStm:xs))]
compile (While bexp doStm:xs) = compB bexp ++ [Branch (compile (doStm:While bexp doStm:xs)) [Noop]] ++ compile xs

-- Parse function that after many calls outputs the final program
parse :: String -> Program
parse = buildData . lexer . filter (/= ' ') . filter (/= '\n')

-- Main parse auxiliary function. It builds the program recursively and checks for invalid variable names and other syntax errors.
buildData :: [String] -> Program
buildData [] = []
buildData (x:xs)
  | null xs || notElem ";" xs || length (filter (=="(") (x:xs)) /= length (filter (==")") (x:xs)) = error "Run-time error"
  | isValidVar x && head xs == ":=" = let (assignStm, rest) = buildAssign (x:xs) in assignStm : buildData rest
  | x == "if" = let (ifStm, rest) = buildIf xs in ifStm : buildData rest
  | x == "while" = let (whileStm, rest) = buildWhile xs in whileStm : buildData rest
  | x == "(" = let (seqStm, rest) = buildSeq (x:xs) in seqStm : buildData rest
  | otherwise = error "Run-time error"

-- Parse auxiliary function for the assign statements x := aexp; . It checks for invalid variable names and other syntax errors.
buildAssign :: [String] -> (Stm, [String])
buildAssign [] = error "Run-time error"
buildAssign (x:":=":xs)
  | isValidVar x = (Assign x (fst (buildAexp (takeWhile (/= ";") xs))), tail (dropWhile (/= ";") xs))
  | otherwise = error "Run-time error"
buildAssign _ = error "Run-time error"

-- Parse auxiliary function for the if statements if bexp thenStm elseStm . It checks for invalid variable names and other syntax errors.
buildIf :: [String] -> (Stm, [String])
buildIf [] = error "Run-time error"
buildIf xs = 
  let (bexp, rest) = (fst (buildBexp (takeWhile (/= "then") xs)), tail (dropWhile (/= "then") xs))

      (thenStm, rest')
        | head rest == "(" = buildSeq rest
        | head rest == "if" = buildIf (tail rest)
        | head rest == "while" = buildWhile (tail rest)
        | isValidVar (head rest) = (fst (buildAssign (takeWhile (/= "else") rest)), tail (dropWhile (/= "else") rest))
        | otherwise = error "Run-time error"

      (elseStm, rest'')
        | head rest' == "(" = buildSeq rest'
        | head rest' == "if" = buildIf (tail rest')
        | head rest' == "while" = buildWhile (tail rest')
        | isValidVar (head rest') = buildAssign rest'
        | otherwise = error "Run-time error"

  in (If bexp thenStm elseStm, rest'')

-- Parse auxiliary function for the while statements while bexp doStm . It checks for invalid variable names and other syntax errors.
buildWhile :: [String] -> (Stm, [String])
buildWhile [] = error "Run-time error"
buildWhile xs = 
  let (bexp, rest) = (fst (buildBexp (takeWhile (/= "do") xs)), tail (dropWhile (/= "do") xs))

      (doStm, rest')
        | head rest == "(" = buildSeq rest
        | head rest == "if" = buildIf (tail rest)
        | head rest == "while" = buildWhile (tail rest)
        | isValidVar (head rest) = (fst (buildAssign (takeWhile (/= ";") rest ++ [";"])), tail (dropWhile (/= ";") rest))
  in (While bexp doStm, rest')

-- Parse auxiliary function for the sequence statements (stm1; stm2) . It checks for invalid variable names and other syntax errors.
buildSeq :: [String] -> (Stm, [String])
buildSeq [] = error "Run-time error"
buildSeq (x:xs)
  |isValidVar x && head xs == ":=" = buildAssign (x:xs)
  | x == "if" = buildIf xs
  | x == "while" = buildWhile xs
  | x == "(" = let 
                (instr1, rest) = buildSeq xs
                (instr2, rest') = buildSeq rest
                in (Seq instr1 instr2, if length rest' > 1 && (head (tail rest') == "else" || head (tail rest') == ";") then drop 2 rest' else tail rest') 
  | otherwise = error "Run-time error"

-- Function that builds the arithmetic expressions. It takes into account the order of operations. Builds a parse tree for the boolean expression.
buildBexp :: [String] -> (Bexp, [String])
buildBexp = buildAnd

-- Function that builds an and expression.
buildAnd :: [String] -> (Bexp, [String])
buildAnd xs = 
  let (left, rest) = buildEqB xs
  in buildAnd' left rest

-- Auxiliary function that builds an and expression.
buildAnd' :: Bexp -> [String] -> (Bexp, [String])
buildAnd' left (op:rest) | op == "and" =
  let (right, rest') = buildEqB rest
  in buildAnd' (AndB left right) rest'
buildAnd' left xs = (left, xs)

-- Function that builds an equality expression for boolean expressions.
buildEqB :: [String] -> (Bexp, [String])
buildEqB xs = 
  let (left, rest) = buildNot xs
  in buildEqB' left rest

-- Auxiliary function that builds an equality expression for boolean expressions.
buildEqB' :: Bexp -> [String] -> (Bexp, [String])
buildEqB' left (op:rest) | op == "=" =
  let (right, rest') = buildNot rest
  in buildEqB' (EqB left right) rest'
buildEqB' left xs = (left, xs)

-- Function that builds a not expression.
buildNot :: [String] -> (Bexp, [String])
buildNot ("not":xs) = 
  let (exp, rest) = buildEqA xs
  in (NegB exp, rest)
buildNot xs = buildEqA xs

-- Function that builds an equality expression for arithmetic expressions.
buildEqA :: [String] -> (Bexp, [String])
buildEqA xs = 
  let (left, rest) = buildComparison xs
  in buildEqA' left rest

-- Auxiliary function that builds an equality expression for arithmetic expressions.
buildEqA' :: Bexp -> [String] -> (Bexp, [String])
buildEqA' left (op:rest) | op == "==" =
  let (right, rest') = buildComparison rest
  in buildEqA' (EqB left right) rest'
buildEqA' left xs = (left, xs)

-- Final function that will build the tree all the way up to the root node.
buildComparison :: [String] -> (Bexp, [String])
buildComparison ("True":xs) = (Bool True, xs)
buildComparison ("False":xs) = (Bool False, xs)
buildComparison("(":xs) = 
  let (exp, _:rest) = buildBexp xs -- ignore the closing parenthesis
  in (exp, rest)

buildComparison xs = 
  let (left, rest) = buildAexp xs
      (op:rest') = rest
      (right, rest'') = buildAexp rest'
  in case op of
    "<=" -> (LeA left right, rest'')
    ">=" -> (LeA right left, rest'')
    "==" -> (EqA left right, rest'')
    "not" -> (NegB (EqA left right), rest'')
    "=" -> (EqA left right, rest'')
    _ -> error "Run-time error"


-- Function that builds the arithmetic expressions. It takes into account the order of operations. Builds a parse tree for the arithmetic expression.
buildAexp :: [String] -> (Aexp, [String])
buildAexp  = buildAddSub 

-- Function that builds an addition or subtraction expression.
buildAddSub :: [String] -> (Aexp, [String])
buildAddSub xs = 
  let (left, rest) = buildMult xs
  in buildAddSub' left rest

-- Auxiliary function that builds an addition or subtraction expression.
buildAddSub' :: Aexp -> [String] -> (Aexp, [String])
buildAddSub' left (op:rest) | op `elem` ["+", "-"] =
  let (right, rest') = buildMult rest
  in buildAddSub' (case op of "+" -> AddA left right
                              "-" -> SubA right left) rest'
buildAddSub' left xs = (left, xs)

-- Function that builds a multiplication expression.
buildMult :: [String] -> (Aexp, [String])
buildMult xs = 
  let (left, rest) = buildFactor xs
  in buildMult' left rest

-- Auxiliary function that builds a multiplication expression.
buildMult' :: Aexp -> [String] -> (Aexp, [String])
buildMult' left (op:rest) | op == "*" =
  let (right, rest') = buildFactor rest
  in buildMult' (MultA left right) rest'
buildMult' left xs = (left, xs)

-- Final function that will build the tree all the way up to the root node.
buildFactor :: [String] -> (Aexp, [String])
buildFactor [] = error "Run-time error"
buildFactor (x:xs) 
  | all (`elem` ['0'..'9']) x = (Num (read x), xs)
  | x == "(" = 
      let (exp, _:rest) = buildAexp xs -- ignore the closing parenthesis
      in (exp, rest)
  | otherwise = (Var x, xs)

-- Lexer function that receives a string and divides it into a list of token strings, the atoms of our language.
lexer :: String -> [String]
lexer [] = []
lexer (':':'=':xs) = ":=" : lexer xs
lexer ('=':'=':xs) = "==" : lexer xs
lexer ('<':'=':xs) = "<=" : lexer xs
lexer ('>':'=':xs) = ">=" : lexer xs
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
lexer (x : xs) | x `elem` ['0'..'9'] = let (num, rest) = span (`elem` ['0'..'9']) (x:xs) in num : lexer rest
lexer (x : xs)
  | x `elem` ['a'..'z'] = 
    let (var, rest) = span (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])) (x:xs) in
      if isValidVar var
        then var : lexer rest 
        else error "Run-time error"
  | otherwise = error "Run-time error"
    
-- Auxiliary function that checks if a variable name doesn't include reserved keywords.
isValidVar :: String -> Bool
isValidVar x = not (any (`isInfixOf` x) ["if", "then", "else", "while", "not", "and", "True", "False", "do"])


----------------------------------------------------------------------------------------
-------------------------------------TEST FUNCTIONS-------------------------------------
----------------------------------------------------------------------------------------

--To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- IO function to test the assembler.
testAssembler' :: IO ()
testAssembler' = do
  print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
  print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
  -- print $ testAssembler [Push 1,Push 2,And] Run-time error
  -- print $ testAssembler [Tru,Tru,Store "y", Fetch "x",Tru] Run-time error


-- To help you test your parser.
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- IO function to test the parser.
testParser' :: IO ()
testParser' = do
  print $ testParser "x := 5; x := x - 1;" == ("","x=4")
  print $ testParser "x := 0 - 2;" == ("","x=-2")
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
  print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
  print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
  print $ testParser "i := 1; sum := 0; while (i <= 100) do (sum := sum + i; i := i + 1;);"
  print $ testParser "x := 1; a := 10; if (x <= a) then (x := 2; if x == 2 then a:= 100; else a := 10;) else a := 100;"
  print $ testParser "x := 10; y := 20; while not (x == 0) do (x := x - 1; while not (y == 0) do y := y - 1;);"

-- IO function to test the parser output (code).
testCode :: IO ()
testCode = do
  print $ parse "x := 5; x := x - 1;"
  print $ parse "x := 0 - 2;"
  print $ parse "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
  print $ parse "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
  print $ parse "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
  print $ parse "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
  print $ parse "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
  print $ parse "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
  print $ parse "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
  print $ parse "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
  print $ parse "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
  print $ parse "i := 1; sum := 0; while (i <= 100) do (sum := sum + i; i := i + 1;);"
  print $ parse "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"


-- Auxiliary function to the lexer.
lexerhelper :: String -> [String]
lexerhelper = lexer . filter (/= ' ') . filter (/= '\n')

-- IO function to test the lexer.
testRead :: IO ()
testRead = do
  print $ lexerhelper "x := 5; x := x - 1;"
  print $ lexerhelper "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
  print $ lexerhelper "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
  print $ lexerhelper "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
  print $ lexerhelper "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
  print $ lexerhelper "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
  print $ lexerhelper "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"

-- IO function to test the arithmetic expressions parser.
testBuildAexp :: IO ()
testBuildAexp = do
  print $ buildAexp $ lexerhelper "1"
  print $ buildAexp $ lexerhelper "x"
  print $ buildAexp $ lexerhelper "1+1"
  print $ buildAexp $ lexerhelper "1+2*3"
  print $ buildAexp $ lexerhelper "1+2*3+4"
  print $ buildAexp $ lexerhelper "1+2*(3+4)*5"
  print $ buildAexp $ lexerhelper "((1))"

-- IO function to test the boolean expressions parser.
testBuildBexp :: IO ()
testBuildBexp = do
  print $ buildBexp $ lexerhelper "True"
  print $ buildBexp $ lexerhelper "x == 1"
  print $ buildBexp $ lexerhelper "x == 1 and y == 2"
  print $ buildBexp $ lexerhelper "not True and 2 <= 5 = 3 == 4"
  print $ buildBexp $ lexerhelper "((True) and (3*(1+2) >= ((4))))"