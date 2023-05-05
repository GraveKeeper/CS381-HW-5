module HW5sol where
import HW5types
import Data.Maybe

--Assignment 5 Group 28
--Sasha Rosenthal and Sean Harrington
--Date: 2/26/2023

--for each function pair the first number is what is pushed off 
--The second number is what is put back on

rankC :: Cmd -> CmdRank
rankC (LDI _) = (0,1)
rankC (ADD)   = (2,1)
rankC (MULT)  = (2,1)
rankC (DUP)   = (1,2)
rankC (DEC)   = (1,1)
rankC (SWAP)  = (2,2)
rankC (POP c) = (c,0)
rankC (LDB _) = (0,1)
rankC (LEQ)   = (2,1)

--Assigns if 1 or if 2 for both halfs
--if neither of the checks return nothing takes min of 2 as new rank
--if checking rank fails the whole rank check fails
--returns min rank only if it does not fail
--if else too small returns nothing 
--if goes though all commands returns the current rank

rankP :: Prog -> Rank -> Maybe Rank
rankP (IFELSE p1 p2:prog) currRank
  | rankp1 /= Nothing && rankp2 /= Nothing = rankP prog (min (fromMaybe 0 rankp1) (fromMaybe 0 rankp2))
  | otherwise = Nothing
  where rankp1 = rankP p1 (currRank - 1)
        rankp2 = rankP p2 (currRank - 1)
rankP (cmd:prog) currRank = if (currRank >= fst (rankC cmd)) then (rankP prog (currRank - fst (rankC cmd) + snd (rankC cmd))) else Nothing
rankP _ currRank = Just(currRank)

-- Checks rank of prog if rank nothing returns ~ rank error
-- if rank not nothing runs prog
-- if prog returns nothing ~ type error 
-- otherwise valid stack

run :: Prog -> Stack -> Result
run p s | (rankP p (length s)) /= Nothing = case runProg p s of
            Nothing -> TypeError
            Just s' -> A(s')
        | otherwise = RankError

-- Deals with maybe stack as helper function
-- if no commands returns stack
-- if ever returns nothing passes
-- otherwise passes though new commands with the new stack

runProg :: Prog -> Stack -> Maybe Stack
runProg [] s = Just s
runProg (c:cs) s = case semCmd c s of
  Nothing -> Nothing
  Just s' -> runProg cs s'

-- Runs/Implementation

semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LDI input) xs                 = Just(I(input):xs)
semCmd ADD (I x:I y:xs)               = Just(I(x+y):xs)
semCmd MULT (I x:I y:xs)              = Just(I(x*y):xs)
semCmd DUP (x:xs)                     = Just(x:x:xs)
semCmd DEC (I x:xs)                   = Just(I(x-1):xs)
semCmd (POP input) xs                 = Just(drop input xs)
semCmd SWAP (x:y:xs)                  = Just(y:x:xs)
semCmd (LDB input) xs                 = Just(B(input):xs)
semCmd LEQ (I x:I y:xs)               = Just(B(x<=y):xs)
semCmd (IFELSE prog1 prog2) (B x:xs)  = runProg (if x then prog1 else prog2) xs
semCmd _ _ = Nothing