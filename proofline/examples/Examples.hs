module Examples where

import MainTest (main')

-- Basic identity function
test1 :: IO ()
test1 = main' "elab" "λ x . x"

-- Identity with explicit type annotation
test2 :: IO ()
test2 = main' "elab" $ unlines [
  "let id : U → U = λ x. x;",
  "id U"
  ] --passed

-- Pair types and functions
test3 :: IO ()
test3 = main' "elab" $ unlines [
  "let Pair : U → U → U = λ A B. (x : A) × B;", 
  "let pair : (A : U) → (B : U) → A → B → Pair A B = λ A B x y. (x, y);",
  "pair U U U U"
  ]
-- passed
test4 :: IO ()
test4 = main' "elab" $ unlines [
  "let comp : (B → U) → (A → B) → A → U = λ f g x. f (g x);",
  "comp (λ _. U) (λ x. x) U"
  ]
-- Error at line 1, column 13:
-- Unbound variable: B

-- Testing projection
test5 :: IO ()
test5 = main' "elab" $ unlines [
  "let fst : (A : U) → (B : U) → A × B → A = λ A B p. p.1;",
  "let snd : (A : U) → (B : U) → A × B → B = λ A B p. p.2;",
  "fst U U (U, U → U)"
  ] -- passed

-- Testing precedence of operators
test6 :: IO ()
test6 = main' "elab" $ unlines [
  "-- Arrow binds looser than Times",
  "let Prod : U → U → U = λ A B. A × B;",
  "let Func : U → U → U = λ A B. A → B;",
  "let test = λ A B C. A × B → C;", -- should parse as (A × B) → C
  "test U U U"
  ]

-- Simple dependent type
test7 :: IO ()
test7 = main' "elab" $ unlines [
  "let Vec : U → U → U = λ A n. (x : A) → A;", -- simplified version
  "Vec U U"
  ] --passed

-- Run all tests
runAllTests :: IO ()
runAllTests = do
  putStrLn "Running test1:"
  test1
  putStrLn "\nRunning test2:"
  test2
  putStrLn "\nRunning test3:"
  test3
  putStrLn "\nRunning test4:"
  test4
  putStrLn "\nRunning test5:"
  test5
  putStrLn "\nRunning test6:"
  test6
  putStrLn "\nRunning test7:"
  
  


-- let Nat  : U = (N : U) -> (N -> N) -> N -> N;
-- let five : Nat = \N s z. s (s (s (s (s z))));
-- let add  : Nat -> Nat -> Nat = \a b N s z. a N s (b N s z);
-- let mul  : Nat -> Nat -> Nat = \a b N s z. a N (b N s) z;

-- let ten      : Nat = add five five;
-- let hundred  : Nat = mul ten ten;
-- let thousand : Nat = mul ten hundred;
-- ten 
-- passed
