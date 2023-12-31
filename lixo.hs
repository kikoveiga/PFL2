buildData :: [String] -> Program
buildData [] = []
buildData (x:xs)
  | x == "if" = let (ifStm, rest) = buildIf xs in ifStm : buildData rest
  | x == "while" = let (whileStm, rest) = buildWhile xs in whileStm : buildData rest
  | otherwise = let (assignStm, rest) = buildAssign (x:xs) in assignStm : buildData rest

buildIf :: [String] -> (Stm, [String])
buildIf [] = error "Parse error"
buildIf (x:xs)
  | x == "(" = let (bexp, rest) = buildBexp xs in
    if head rest == ")" then
      let (stm1, rest1) = buildStm (tail rest) in
        if head rest1 == "else" then
          let (stm2, rest2) = buildStm (tail rest1) in
            (If bexp stm1 stm2, rest2)
        else error "Parse error"
    else error "Parse error"
  | otherwise = error "Parse error"

buildWhile :: [String] -> (Stm, [String])
buildWhile [] = error "Parse error"
buildWhile (x:xs)
  | x == "(" = let (bexp, rest) = buildBexp xs in
    if head rest == ")" then
      let (stm, rest1) = buildStm (tail rest) in
        (While bexp stm, rest1)
    else error "Parse error"
  | otherwise = error "Parse error"

buildAssign :: [String] -> (Stm, [String])
buildAssign [] = error "Parse error"
buildAssign (x:xs)
  | head x `elem` ['a'..'z'] = let (aexp, rest) = buildAexp xs in
    if head rest == ";" then
      (Assign x aexp, tail rest)
    else error "Parse error"
  | head x `elem` ['0'..'9'] = error "Parse error"
  | otherwise = error "Parse error"

buildStm :: [String] -> (Stm, [String])
buildStm [] = error "Parse error"
buildStm (x:xs)
  | x == "if" = buildIf xs
  | x == "while" = buildWhile xs
  | head x `elem` ['a'..'z'] = buildAssign (x:xs)
  | otherwise = error "Parse error"

buildBexp :: [String] -> (Bexp, [String])
buildBexp [] = error "Parse error"
buildBexp (x:xs)
  | x == "not" = let (bexp, rest) = buildBexp xs in (NegB bexp, rest)
  | x == "True" = (Bool True, xs)
  | x == "False" = (Bool False, xs)
  | x == "(" = let (aexp1, rest) = buildAexp xs in
    if head rest == "==" then
      let (aexp2, rest1) = buildAexp (tail rest) in
        if head rest1 == ")" then
          (EqA aexp1 aexp2, tail rest1)
        else error "Parse error"
    else if head rest == "<=" then
      let (aexp2, rest1) = buildAexp (tail rest) in
        if head rest1 == ")" then
          (LeA aexp1 aexp2, tail rest1)
        else error "Parse error"
    else error "Parse error"
  | otherwise = error "Parse error"

buildAexp :: [String] -> (Aexp, [String])
buildAexp [] = error "Parse error 19"
buildAexp (x:symbol:xs)
  | x == "(" =
    let (aexp1, rest) = buildAexp xs in
      if head rest == ")" then
        let (aexp2, rest1) = buildAexp (tail rest) in
          if head x `elem` ['0'..'9'] then
            (MultA (Num (read x)) aexp2, rest1)
          else
            (MultA (Var x) aexp2, rest1)
      else error "Parse error 20"
  
  | symbol == ";" = if head x `elem` ['0'..'9'] then (Num (read x), xs) else (Var x, xs)
  | symbol == "*" = 
    if head xs == "(" then
      let (aexp, rest) = buildAexp (tail xs) in
        if head rest == ")" then
          (MultA (Num (read x)) aexp, tail rest)
        else error "Parse error 20"
    else let (aexp, rest) = buildAexp xs in (MultA (Num (read x)) aexp, rest)
  | symbol == "+" = let (aexp, rest) = buildAexp xs in if head x `elem` ['0'..'9'] then (AddA (Num (read x)) aexp, rest) else (AddA (Var x) aexp, rest)
  | symbol == "-" = let (aexp, rest) = buildAexp xs in if head x `elem` ['0'..'9'] then (SubA (Num (read x)) aexp, rest) else (SubA aexp (Var x), rest)
  | otherwise = error "Parse error"

