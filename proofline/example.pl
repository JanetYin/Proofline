-- Simple identity function
\x. x

-- Polymorphic identity function
\{A} -> \x : A. x

-- Church numerals
let zero : (A : U) -> (A -> A) -> A -> A = 
  \A. \s. \z. z;

let succ : ((A : U) -> (A -> A) -> A -> A) -> (A : U) -> (A -> A) -> A -> A = 
  \n. \A. \s. \z. s (n A s z);

-- Pi type example
(A : U) -> (B : A -> U) -> (x : A) -> B x -> U

-- Sigma type example
(x : A) * B x

-- Pair example
(3, 4)

-- Projection example
fst.1