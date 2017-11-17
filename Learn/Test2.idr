module ProveVoid

%default total

data In : (f : Type -> Type) -> Type where
  MkIn : In f

-- This step requires definitional type constructor injectivity and is
-- the source of the problem.
injIn : In x = In y -> x = y
injIn Refl = Refl

P : Type -> Type
P x = (a : (Type -> Type) ** (In a = x, a x -> Void))

InP : Type
InP = In P

func1 : P InP -> Void
func1 (v ** (Ha0, Ha1)) =
  let lem2 : (P (In P) -> Void) = replace {P=\ x => x (In P) -> Void} (injIn Ha0) Ha1
  in lem2 (P ** (Refl, lem2))

total -- for extra oumph!
lem2 : Void
lem2 =
  let foo : P InP = (P ** (Refl, func1))
  in func1 foo
