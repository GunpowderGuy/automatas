module DynamicTypes

import Data.Vect

-- Define a dynamic "Any" type
data Any : Type where
  MkAny : (a : Type) -> a -> Any

-- Define a dynamic function wrapper
data DynFunc : Type where
  MkDynFunc : (a, b : Type) -> (a -> b) -> DynFunc

-- Check if two types are the same
sameType : (x, y : Type) -> Maybe (x = y)
sameType Int Int = Just Refl
sameType String String = Just Refl
sameType _ _ = Nothing

-- Apply a dynamic function to a dynamic value
applyDyn : DynFunc -> Any -> Maybe Any
applyDyn (MkDynFunc a b f) (MkAny a' x) =
  case sameType a a' of
    Just Refl => Just (MkAny b (f x))  -- Type matches, apply function
    Nothing   => Nothing               -- Type mismatch, return Nothing

-- Helper function to extract an `Int` from `Any`
extractInt : Any -> Maybe Int
extractInt (MkAny Int x) = Just x
extractInt _ = Nothing

-- Helper function to extract a `String` from `Any`
extractString : Any -> Maybe String
extractString (MkAny String x) = Just x
extractString _ = Nothing

-- Example: Typed Functions
inc : Int -> Int
inc x = x + 1

intToStr : Int -> String
intToStr x = show x

-- Create dynamic function wrappers
dynInc : DynFunc
dynInc = MkDynFunc Int Int inc

dynToString : DynFunc
dynToString = MkDynFunc Int String intToStr

-- Create dynamic values
valInt : Any
valInt = MkAny Int 5

valString : Any
valString = MkAny String "Hello"

-- **Structured Test Cases (Pure)**
testApplyIncToInt : Bool
testApplyIncToInt =
  case applyDyn dynInc valInt of
    Just res => extractInt res == Just 6
    Nothing  => False

testApplyIncToString : Bool
testApplyIncToString =
  case applyDyn dynInc valString of
    Nothing  => True  -- Should fail as expected
    Just _   => False -- Unexpected success

testApplyIntToStr : Bool
testApplyIntToStr =
  case applyDyn dynToString valInt of
    Just res => extractString res == Just "5"
    Nothing  => False

-- **Run all tests and return True if all pass**
runTests : Bool
runTests = testApplyIncToInt && testApplyIncToString && testApplyIntToStr

-- **Main function that prints test results**
main : IO ()
main = if runTests
       then putStrLn "All tests passed!"
       else putStrLn "Some tests failed!"
