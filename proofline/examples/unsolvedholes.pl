-- Simple test with deliberately unsolved metavariables

-- A simple identity function with underscore type
let id : (A : _) -> A -> A = \A x. x;

-- Using the id function but leaving the type argument as a hole
let result1 = id _ U;

-- A function with underscore in the return type
let mystery : U -> _ = \x. x;

-- A pair with one component as a hole
let pair1 : U × (U -> U × U) = (U, _);

-- A dependent function with a hole in the body
let unsolved : (A : U) -> A -> (A -> A -> A  × U) = \A x. _;

-- Return something to evaluate
pair1