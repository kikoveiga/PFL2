import Data.List (intercalate, sortOn)
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
run (Sub:code, Left x:Left y:stack, state) = run (code, Left (x-y):stack, state)
run (Tru:code, stack, state) = run (code, Right True:stack, state)
run (Fals:code, stack, state) = run (code, Right False:stack, state)
run (Equ:code, x:y:stack, state) = run (code, Right (x==y):stack, state)
run (Le:code, Left x:Left y:stack, state) = run (code, Right (x<=y):stack, state)
run (And:code, Right x:Right y:stack, state) = run (code, Right (x && y):stack, state)
run (And:code, _, _) = error "Run-time error"
run (Neg:code, Right x:stack, state) = run (code, Right (not x):stack, state)
run (Fetch x:code, stack, state) = run (code, maybe (error "Run-time error") (:stack) (lookup x state), state)
run (Store x:code, y:stack, state) = run (code, stack, (x, y) : filter ((/= x) . fst) state)
run (Noop:code, stack, state) = run (code, stack, state)
run (Branch code1 code2:code, Right True:stack, state) = run (code1 ++ code, stack, state)
run (Branch code1 code2:code, Right False:stack, state) = run (code2 ++ code, stack, state)
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

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, state2Str store)
  --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")