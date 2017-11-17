module ProveVoid

%default total

data In : (f : Type -> Type) -> Type where
  MkIn : (f : Type -> Type) -> In f
  
tToT : Type -> Type
tToT i = Int  

injIn' : {z : Type -> Type} -> {y : Type->Type} -> In z = In y -> z = y
injIn' {z=x} {y=x} (Refl {A=Type} {x=_}) = Refl

{-
injIn' : {A : Type} -> {z : A} -> {y : A} -> z = y -> z = y
injIn' Refl = Refl
-}


foo : Int
foo = 42

-- This step requires definitional type constructor injectivity and is
-- the source of the problem.
injIn : In z = In y -> z = y
injIn {z=z} {y=y} prf = 
  case prf of 
    (Refl {A=_} {x=_}) => Refl


P : Type -> Type
--P x = (a : (Type -> Type) ** (In a = x, a x -> Void))
P x = DPair (Type -> Type) (\a => (In a = x, a x -> Void))

InP : Type
InP = In P

func1 : P InP -> Void
func1 (v ** (inAEqXPrf, axIsVoidPrf)) =
  let 
    lem1 = (injIn inAEqXPrf) 
    lem2  = replace {P=\ x => x (In P) -> Void} lem1 
    lem3 : (P (In P) -> Void) = lem2 axIsVoidPrf
    ans = lem3 (P ** (Refl, lem3))
  in ?xxx --

total -- for extra oumph!
lem2 : Void
lem2 =
  let foo : P InP = (P ** (Refl, func1))
      ans = func1 foo
  in ?xxx2


fee : Type -> Type
--fee x = (b : Type ** List b)
fee x = DPair Type (\b => List b)
--fee x = DPair Type List

x : Int -> Int
x = (\z => 52)

 
 
 
