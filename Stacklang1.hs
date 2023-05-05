module Stacklang1 where


type Prog = [Cmd]
data Cmd = LD Int 
  | ADD 
  | MULT 
  | DUP 
  deriving Show 

type Stack = [Int] 

semCmd :: Cmd -> Stack -> Maybe Stack

semCmd (LD x) y = Just(x:y)

semCmd ADD [] = Nothing
semCmd ADD ((x):[]) = Nothing
semCmd ADD ((x):(y):s) = Just (((x+y)):s)
semCmd ADD _ = Nothing

semCmd MULT [] = Nothing
semCmd MULT ((x):[]) = Nothing
semCmd MULT ((x):(y):s) = Just (((x*y)):s)
semCmd MULT _ = Nothing

semCmd DUP [] = Nothing
semCmd DUP ((x):s) = Just (x:x:s)
semCmd DUP _ = Nothing

run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
