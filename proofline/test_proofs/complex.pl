let id : (A : U) → A → A = λ A x. x;
let comp : (A : U) → (B : U) → (C : U) → (B → C) → (A → B) → A → C = λ A B C f g x. f (g x);
let twice : (A : U) → (A → A) → A → A = λ A f x. f (f x);
twice U (id U) U