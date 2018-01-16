module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Expression
    = Var String                   -- Variable
    | Val Int                      -- Integer literal
    | Op Expression Bop Expression -- Operation
    deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Part 01 -----------------------------------------

extend :: State -> String -> Int -> State
extend state key value = (\k -> if k == key then value else state k)

empty :: State
empty _ = 0

-- Part 02 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

evalBoolFun :: (Int -> Int -> Bool) -> Int -> Int -> Int
evalBoolFun f x y = boolToInt $ f x y

evalBop :: Bop -> Int -> Int -> Int
evalBop Plus    = (+)
evalBop Minus   = (-)
evalBop Times   = (*)
evalBop Divide  = quot
evalBop Gt      = evalBoolFun (>)
evalBop Ge      = evalBoolFun (>=)
evalBop Lt      = evalBoolFun (<)
evalBop Le      = evalBoolFun (<=)
evalBop Eql     = evalBoolFun (==)

evalE :: State -> Expression -> Int
evalE state (Var key)           = state key
evalE _     (Val value)         = value
evalE state (Op  exp1 bop exp2) = evalBop bop (evalE state exp1) (evalE state exp2)

-- Part 03 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign   strVar exp)
        = DAssign strVar exp
desugar (Incr     strVar)
        = DAssign strVar (Op (Var strVar) Plus (Val 1))
desugar (If       expCond stmtTrue stmtFalse)
        = DIf expCond (desugar stmtTrue) (desugar stmtFalse)
desugar (While    expCond stmtLoop)
        = DWhile expCond (desugar stmtLoop)
desugar (For      stmtInit expCond stmtUpdate stmtLoop)
        = DSequence
            (desugar stmtInit)
            (DWhile expCond (DSequence (desugar stmtLoop) (desugar stmtUpdate)))
desugar (Sequence stmt1 stmt2)
        = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip
        = DSkip

-- Part 04 -----------------------------------------

isTrue :: Int -> Bool
isTrue = (/=0)

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign strVar exp)
    = extend state strVar $ evalE state exp
evalSimple state (DIf expCond stmtTrue stmtFalse)
    = evalSimple state $ if isTrue $ evalE state expCond then stmtTrue else stmtFalse
evalSimple state (DWhile expCond stmtLoop)
    = if isTrue $ evalE state expCond
        then let newState = evalSimple state stmtLoop 
            in evalSimple newState (DWhile expCond stmtLoop)
        else state
evalSimple state (DSequence stmt1 stmt2)
    = evalSimple (evalSimple state stmt1) stmt2
evalSimple state DSkip 
    = state
    
run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Part 05 -----------------------------------------

parse :: String -> Maybe Statement
parse = undefined

-- Programs ----------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
  for (Out := 1; In > 0; In := In - 1) {
      Out := In * Out
  }
-}

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{-
  Calculate the floor of the square root of the input
  B := 0;
  while (A >= B * B) {
      B++
  };
  B := B - 1
-}

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{-
  Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;

  if (In == 0) {
      Out := F0
  } else {
      if (In == 1) {
          Out := F1
      } else {
          for (C := 2; C <= In; C++) {
              T  := F0 + F1;
              F0 := F1;
              F1 := T;
              Out := T
          }
      }
  }

-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]