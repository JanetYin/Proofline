-- Example with holes

-- Function with a hole for the implementation
let f : (A : U) → A → A = 
  λ A x. _;

-- Pair type with holes
let Pair : U → U → U = λ A B. (x : A) × B;

-- Pair constructor with a hole for the second component
let mkPair : (A : U) → (B : U) → A → B → Pair A B = 
  λ A B x y. (x, _);

-- Function with a hole in the return type
let g : (A : U) → A → _ = 
  λ A x. x;

-- Testing expression with multiple holes
let test = 
  let p : Pair U U = mkPair U U U _;
  (f U (p.1), _);