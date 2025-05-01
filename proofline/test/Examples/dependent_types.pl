let id : (A : U) → A → A = λ A x. x;

-- Function composition
let comp : (A : U) → (B : U) → (C : U) → (B → C) → (A → B) → A → C = 
  λ A B C f g x. f (g x);

-- Church numerals
let Nat : U = (N : U) → (N → N) → N → N;
let zero : Nat = λ N s z. z;
let succ : Nat → Nat = λ n N s z. s (n N s z);

-- Addition for Church numerals
let add : Nat → Nat → Nat = 
  λ a b N s z. a N s (b N s z);

-- Multiplication for Church numerals  
let mul : Nat → Nat → Nat = 
  λ a b N s z. a N (b N s) z;

-- Some numbers
let one : Nat = succ zero;
let two : Nat = succ one;
let three : Nat = succ two;
let four : Nat = add two two;
let six : Nat = mul two three;

-- Test expression
comp Nat Nat Nat (add one) (mul two) three