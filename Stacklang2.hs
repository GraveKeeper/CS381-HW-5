module Stacklang2 where

type Prog = [Cmd]

type Stack = [Either Bool Int]
data Cmd = LDI Int 
  | LDB Bool 
  | LEQ 
  | ADD 
  | MULT 
  | DUP 
  | IFELSE Prog Prog 
  deriving Show   


semCmd :: Cmd ->Stack -> Maybe Stack

semCmd ADD [] = Nothing
semCmd ADD ((Right x):[]) = Nothing
semCmd ADD ((Right x):(Right y):s) = Just ((Right(x+y)):s)
semCmd ADD _ = Nothing

semCmd DUP [] = Nothing
semCmd DUP (Right (x):s) = Just (Right(x):Right(x):s)
semCmd DUP (Left (x):s) = Just (Left(x):Left(x):s)
semCmd DUP _ = Nothing

semCmd MULT [] = Nothing
semCmd MULT ((Right _):[]) = Nothing
semCmd MULT ((Right x):(Right y):s) = Just ((Right(x*y)):s)
semCmd MULT _ = Nothing

semCmd LEQ [] = Nothing
semCmd LEQ ((Right x):[]) = Nothing
semCmd LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s)
semCmd LEQ _ = Nothing

semCmd (LDI  x) y = Just (Right(x):y)

semCmd (LDB x) y = Just(Left(x):y)

semCmd (IFELSE p1 p2) (x:s) = case [x] of 
                              [Right x] -> Nothing
                              [Left x] -> 
                                  if x
                                      then run p1 s
                                  else run p2 s

run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
