module Learn.Elab

import Language.Reflection.Utils

mkId : Elab ()
mkId = do x <- gensym "x"
          attack
          intro x
          fill (Var x)
          solve
          solve

%language ElabReflection

idNat : Nat -> Nat
idNat = %runElab mkId  

idNat2 : Nat -> Nat
idNat2 = %runElab (do intro `{{x}}
                      fill (Var `{{x}})
                      solve) 

idNat3 : Nat -> Nat 
idNat3 = ?myhole

--total                  
quicksort : List Nat -> List Nat
quicksort [] = []
quicksort (x::xs) =
  quicksort (filter (< x) xs) ++ x :: quicksort (filter (>= x) xs)

---------- Proofs ----------

Learn.Elab.myhole = %runElab (do x <- gensym "x"
                                 attack
                                 intro x
                                 fill (Var x)
                                 solve
                                 solve)
                           
data QSAcc : List Nat -> Type where
  QSAccCase0 : QSAcc []
  QSAccCase1 : QSAcc (filter (< x) xs) ->
               QSAcc (filter (>= x) xs) ->
               QSAcc (x :: xs)

qs' : (xs : List Nat) -> QSAcc xs -> List Nat
qs' [] QSAccCase0 = []
qs' (x :: xs) (QSAccCase1 l r) =
  qs' (filter (< x) xs) l ++
  x :: qs' (filter (>= x) xs) r

{- bc : (inFun, outFun, accPred : TTName) -> Elab ()
bc inFun outFun accPred = do
  (inFun', Ref, inTy) <- lookupTyExact inFun
     | _ => fail [TextPart "Not a function"]
  (_, args, res) <- lookupArgsExact inFun'
  declareDatatype $ Declare accPred args `(Type)
  let accArg = MkFunArg `{{acc}}
                (mkApp (Var accPred)
                   (map (Var . name) args))
                Explicit
                NotErased
  declareType $
    Declare outFun (args ++ [accArg]) res
  DefineFun _ oldClauses <- lookupFunDefnExact inFun'
  (newClauses, ctors) <-
    unzip <$> ?processClauses Z oldClauses
  defineDatatype $ DefineDatatype accPred ctors
  defineFunction $ DefineFun outFun newClauses
  
--%runElab (bc `{quicksort} `{{qs'}} `{{QSAcc}})
-}

-- foo : Nat -> Nat
-- foo k = ?foo_rhs

-- getAllNames : TT -> List TTName
-- getAllNames (P Bound (UN x) tm) = ?getAllNames_rhs_1
-- getAllNames (P Bound (NS n xs) tm) = ?getAllNames_rhs_13
-- getAllNames (P Bound (MN x y) tm) = ?getAllNames_rhs_14
-- getAllNames (P Bound (SN sn) tm) = ?getAllNames_rhs_15
-- getAllNames (P Ref n tm) = ?getAllNames_rhs_10
-- getAllNames (P (DCon x y) n tm) = ?getAllNames_rhs_11
-- getAllNames (P (TCon x y) n tm) = ?getAllNames_rhs_12
-- getAllNames (V x) = ?getAllNames_rhs_2
-- getAllNames (Bind n b tm) = ?getAllNames_rhs_3
-- getAllNames (App tm tm') = ?getAllNames_rhs_4
-- getAllNames (TConst c) = ?getAllNames_rhs_5
-- getAllNames Erased = ?getAllNames_rhs_6
-- getAllNames (TType uexp) = ?getAllNames_rhs_7
-- getAllNames (UType x) = ?getAllNames_rhs_8

data Foo = Fee | Food
