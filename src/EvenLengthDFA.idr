module EvenLengthDFA

import Data.Vect

%default total

-- Define the states of the DFA
data State = Even | Odd

-- Manually implement Eq for State
implementation Eq State where
  (==) Even Even = True
  (==) Odd Odd   = True
  (==) _ _       = False

-- Define the transition function
transition : State -> Char -> State
transition Even _ = Odd
transition Odd  _ = Even

-- Define the DFA processing function
process : State -> List Char -> State
process state []        = state
process state (x :: xs) = process (transition state x) xs

-- Define the acceptance condition (final state must be Even)
accepts : List Char -> Bool
accepts input = process Even input == Even

-- Example test cases
main : IO ()
main = do
  printLn (accepts ['0', '1'])      -- True, length = 2
  printLn (accepts ['0', '1', '1']) -- False, length = 3
  printLn (accepts ['1', '0', '1', '0']) -- True, length = 4
