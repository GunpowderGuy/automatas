module EvenLengthDFA

%default total

-- Define the states of the DFA
data State = Even | Odd

data Symbol = Zero | One


-- Manually implement Eq for State
implementation Eq State where
  (==) Even Even = True
  (==) Odd Odd   = True
  (==) _ _       = False

-- Define the transition function
transition : State -> Symbol -> State
transition Even _ = Odd
transition Odd  _ = Even

-- Define the DFA processing function
process : State -> List Symbol -> State
process state []        = state
process state (x :: xs) = process (transition state x) xs

-- Define the acceptance condition (final state must be Even)
accepts : List Symbol -> Bool
accepts input = process Even input == Even

isEvenLength : List a -> Bool
isEvenLength [] = True  -- Empty list is even-length
isEvenLength [_] = False  -- Single element is odd-length
isEvenLength (_ :: _ :: xs) = isEvenLength xs  -- Remove two elements at a time

squareIdentity : (x : Integer) -> (x * x =  x * x)
squareIdentity x = Refl  -- Direct proof using Idris2's evaluation

--appendChangesList : (xs : List Symbol) -> ( ( isEvenLength xs )  = ( accepts xs )  )
-- appendChangesList xs = Refl


-- Example test cases
main : IO ()
main = do
  printLn (accepts [One, Zero])      -- True, length = 2
  printLn (accepts [ One, One, One]) -- False, length = 3
  printLn (accepts [Zero, Zero, Zero, One]) -- True, length = 4
