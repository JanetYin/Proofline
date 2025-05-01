-- taste1.txt
let Bool : U = (B : U) -> B -> B -> B;
let true : Bool = \B t f. t;
let false : Bool = \B t f. f;
let not : Bool -> Bool = \b B t f. b B f t;

(not true)