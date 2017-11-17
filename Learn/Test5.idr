module Test5

import Data.Vect


%default total

data In : (f : Type -> Type) -> Type where
  MkIn : In f

-- This step requires definitional type constructor injectivity and is
-- the source of the problem.
injIn : In x = In y -> x = y
injIn Refl = Refl

P : Type -> Type
P x = (a : (Type -> Type) ** (In a = x, a x -> Void))

func1 : P (In P) -> Void
func1 (a ** (InAEqInP, AAppInPImpVoid)) =
  let lem2 : (P (In P) -> Void) = replace {P=\ x => x (In P) -> Void} (injIn InAEqInP) AAppInPImpVoid
  in lem2 (P ** (Refl, lem2))

total -- for extra oumph!
lem2 : Void
lem2 =
  let foo : P (In P) = (P ** (Refl, func1))
  in func1 foo


bar : (x : Type) -> (x, x) -> Int
bar _ _ = 42

