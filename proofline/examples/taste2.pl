-- File: taste2.txt

-- Church-encoded natural numbers
let Nat : U = (N : U) -> (N -> N) -> N -> N;
let zero : Nat = \N s z. z;
let succ : Nat -> Nat = \n N s z. s (n N s z);
let one : Nat = succ zero;
let two : Nat = succ one;
let add : Nat -> Nat -> Nat = \a b N s z. a N s (b N s z);

-- Define equality type
let Eq : (A : U) -> A -> A -> U = \A x y. (P : A -> U) -> P x -> P y;
let refl : (A : U) -> (x : A) -> Eq A x x = \A x P px. px;

let id : (A : U) -> A -> A = \A x. x;

-- Proof that adding zero to a number gives the same number
let addzero : (n : Nat) -> Eq Nat (add n zero) n = 
  \n. refl Nat n;

let test : Eq Nat (add two zero) two = addzero two;
-- let wrong : Eq Nat ( add two one ) two = addzero two ;
-- calling test
test