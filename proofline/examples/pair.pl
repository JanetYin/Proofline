-- Projection functions for pairs
let fst : (A : U) -> (B : U) -> A * B -> A = \A B p. p.1;
let snd : (A : U) -> (B : U) -> A * B -> B = \A B p. p.2;

-- fst U U (U, U -> U)  -- Returns U
-- snd U U (U, U -> U)     -- Returns U -> U

let Nat  : U = (N : U) -> (N -> N) -> N -> N;
let isEven : Nat -> U = \n. _;  -- Placeholder for an actual definition
let two : Nat = \N s z. s (s z);
let evenProoftwo : isEven two = _;  -- Placeholder for a proof
let evenExists : (n : Nat) × isEven n = (two, evenProoftwo);

evenExists