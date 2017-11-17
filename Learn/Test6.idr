module Foo

-- fact'' : ((Nat -> Nat) -> (Nat -> Nat))
-- fact'' f1 = (\x => case x of
--                         Z => Z
--                         (S k) => (S k) * (f1 k))
                        
-- fact' : ((Nat -> Nat) -> (Nat -> Nat)) -> Nat -> Nat
-- fact' f = f (\y => f  
-- fact' f (S k) = (S k) * (f k)

-- fact : Nat -> Nat
-- fact x = fact' fact' x


--cant do y combinator, because can't specify type of self within self
-- part-fact : ((Nat -> Nat) -> Nat -> Nat) -> Nat -> Nat
-- part-fact self n = 
--   case n of
--     Z => 1
--     (S k) => (* (S k) (self self k))
