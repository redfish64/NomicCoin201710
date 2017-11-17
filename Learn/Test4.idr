module Test4

import TTUtils as T

%language ElabReflection

%default total

data In : (f : Type -> Type) -> Type where
  MkIn : In f

-- This step requires definitional type constructor injectivity and is
-- the source of the problem.
injIn : In z = In y -> z = y
injIn (Refl {A=Type} {x=_}) = Refl

injInTT : FnDesc
injInTT = %runElab (getTTForFn `{injIn})

-- P : Type -> Type
-- P v = (a : (Type -> Type) ** (In a = v, a v -> Void))

-- func1 : P (In P) -> Void
-- func1 (a ** (InAEqInP, AAppInPImpVoid)) =
--   let lem2 : (P (In P) -> Void) = -- replace {P=\ x => x (In P) -> Void} (injIn InAEqX) AXImpVoid
--     let e1 = (injIn InAEqInP)
--         lem3 = replace {P=\ x => x (In P) -> Void} (injIn InAEqInP) AAppInPImpVoid
--     in
--       ?xxx
--   in lem2 (P ** (Refl, lem2))

-- total -- for extra oumph!
-- lem2 : Void
-- lem2 =
--   let foo : P (In P) = (P ** (Refl, func1))
--   in func1 foo


-- bla : x = y -> y = x
-- bla Refl = Refl


-- -- lookupFunDefnExact
-- -- let z : FunDefn TT = DefineFun (NS (UN "injIn") ["ProveVoid"])
-- --              [MkFunClause (Bind (UN "x")
-- --               (PVar (Bind (UN "__pi_arg")
-- --                     (Pi (TType (UVar "./Learn/ProveVoid.idr" 84)) (TType (UVar "./Learn/ProveVoid.idr" 86)))
-- --                     (TType (UVar "./Learn/ProveVoid.idr" 87))))
-- --               (App (App (App (P Ref (NS (UN "injIn") ["ProveVoid"]) Erased) (P Bound (UN "x") Erased)) (P Bound (UN "x") Erased))
-- --                    (App (App (P (DCon 0 2) (UN "Refl") Erased) Erased) Erased)))
-- --               (Bind (UN "x")
-- --               (PVar (Bind (UN "__pi_arg")
-- --                     (Pi (TType (UVar "./Learn/ProveVoid.idr" 84)) (TType (UVar "./Learn/ProveVoid.idr" 86)))
-- --                     (TType (UVar "./Learn/ProveVoid.idr" 87))))
-- --               (App (App (P (DCon 0 2) (UN "Refl") Erased)
-- --                   (Bind (UN "__pi_arg1")
-- --                   (Pi (TType (UVar "./Learn/ProveVoid.idr" 97)) (TType (UVar "./Learn/ProveVoid.idr" 99)))
-- --                   (TType (UVar "./Learn/ProveVoid.idr" 100))))
-- --                    (P Bound (UN "x") Erased)))]

-- -- lookupTyExact
-- -- let z : (TTName, NameType, TT) = (NS (UN "injIn") ["ProveVoid"],
-- --           Ref,
-- --           Bind (UN "y")
-- --                (Pi (Bind (UN "__pi_arg") (Pi (TType (UVar "./ProveVoid.idr" 59)) (TType (UVar "./ProveVoid.idr" 61))) (TType (UVar "./ProveVoid.idr" 62)))
-- --              (TType (UVar "./ProveVoid.idr" 64)))
-- --                (Bind (UN "x")
-- --                (Pi (Bind (UN "__pi_arg") (Pi (TType (UVar "./ProveVoid.idr" 65)) (TType (UVar "./ProveVoid.idr" 67))) (TType (UVar "./ProveVoid.idr" 68)))
-- --              (TType (UVar "./ProveVoid.idr" 70)))
-- --                (Bind (UN "__pi_arg")
-- --                (Pi (App (App (App (App (P (TCon 7 4) (UN "=") Erased) (TType (UVar "./ProveVoid.idr" 71))) (TType (UVar "./ProveVoid.idr" 72)))
-- --                  (App (P (TCon 8 1) (NS (UN "In") ["ProveVoid"]) Erased) (V 0)))
-- --                   (App (P (TCon 8 1) (NS (UN "In") ["ProveVoid"]) Erased) (V 1)))
-- --                    (TType (UVar "./ProveVoid.idr" 73)))
-- --                (App (App (App (App (P (TCon 7 4) (UN "=") Erased)
-- --                        (Bind (UN "__pi_arg1")
-- --                        (Pi (TType (UVar "./ProveVoid.idr" 74)) (TType (UVar "./ProveVoid.idr" 76)))
-- --                        (TType (UVar "./ProveVoid.idr" 77))))
-- --                   (Bind (UN "__pi_arg1")
-- --                   (Pi (TType (UVar "./ProveVoid.idr" 79)) (TType (UVar "./ProveVoid.idr" 81)))
-- --                   (TType (UVar "./ProveVoid.idr" 82))))
-- --                    (V 1))
-- --               (V 2)))))

-- -- lookupFunDefnExact
-- -- let z : FunDefn TT = DefineFun (NS (UN "bla") ["ProveVoid"])
-- --              [MkFunClause (Bind (MN 504 "phTy")
-- --               (PVar (TType (UVar "./ProveVoid.idr" 268)))
-- --               (Bind (MN 505 "x")
-- --               (PVar (P Bound (MN 504 "phTy") Erased))
-- --               (App (App (App (App (P Ref (NS (UN "bla") ["ProveVoid"]) Erased) (P Bound (MN 504 "phTy") Erased)) (P Bound (MN 505 "x") Erased))
-- --                   (P Bound (MN 505 "x") Erased))
-- --                    (App (App (P (DCon 0 2) (UN "Refl") Erased) Erased) Erased))))
-- --               (Bind (MN 504 "phTy")
-- --               (PVar (TType (UVar "./ProveVoid.idr" 268)))
-- --               (Bind (MN 505 "x")
-- --               (PVar (P Bound (MN 504 "phTy") Erased))
-- --               (App (App (P (DCon 0 2) (UN "Refl") Erased) (P Bound (MN 504 "phTy") Erased)) (P Bound (MN 505 "x") Erased))))]

-- -- lookupTyExact
-- blaTyExact : (TTName, NameType, TT) 
-- blaTyExact = 
--   (NS (UN "bla") ["ProveVoid"],
--     Ref,
--     Bind (UN "phTy")
--       (Pi (TType (UVar "./ProveVoid.idr" 262)) (TType (UVar "./ProveVoid.idr" 264)))
--       (Bind (UN "x")
--         (Pi (V 0) (TType (UVar "./ProveVoid.idr" 265)))
--         (Bind (UN "y")
--           (Pi (V 1) (TType (UVar "./ProveVoid.idr" 266)))
--           (Bind (UN "__pi_arg")
--              (Pi (App (App (App (App (P (TCon 7 4) (UN "=") Erased) (V 2)) (V 2)) (V 1)) (V 0)) (TType (UVar "./ProveVoid.idr" 267)))
--              (App (App (App (App (P (TCon 7 4) (UN "=") Erased) (V 3)) (V 3)) (V 1)) (V 2))))))

-- fazoo : 3 = 4 -> Void
-- fazoo Refl impossible
