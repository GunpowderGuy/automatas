module Factor

-- Factorial function using recursion
factorial : Nat -> Nat
factorial Z     = 1
factorial (S k) = (S k) * factorial k

-- Fibonacci function using recursion
fibonacci : Nat -> Natrk
fibonacci Z     = 0
fibonacci (S Z) = 1
fibonacci (S (S k)) = fibonacci (S k) + fibonacci k

-- Example usage:
-- factorial 5 => 120
-- fibonacci 6 => 8

main : IO ()
main = putStrLn "Hello, World!"