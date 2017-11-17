%default total

--changing codata to dat would make "ones" non-total
codata Stream' : Type -> Type where
  (::) : (e : a) -> Stream' a -> Stream' a
  
ones : Stream' Nat
ones = 1 :: ones

