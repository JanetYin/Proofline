-- we saw this definition for boolean type using let binding in the previous example
let Bool : U = (B : U) -> B -> B -> B;

-- we can define more complex functions or proofs by using more complex let binding

let trans : (A : U)(x y z : A) -> Eq A x y -> Eq A y z -> Eq A x z =
  \A x y z eqxy eqyz .

  \P px .               

  let py : P y = eqxy P px;

  let pz : P z = eqyz P py;

  pz;

trans