module BC

-- run with:  idris -i ./idris/libs/pruviloj Learn/ElabSlides.idr 

import public Language.Reflection.Utils
import public Pruviloj

%default total

||| Extract the pattern variable bindings from around a term.
patvarArgs : Raw -> (List (TTName, Raw), Raw)
patvarArgs (RBind n (PVar ty) tm) =
    let rest = patvarArgs tm
    in ((n, ty) :: fst rest, snd rest)
patvarArgs tm = ([], tm)

||| Find the conjunction of a context of reflected types, expressed as
||| an iterated sigma type.
conjoin : List (TTName, Raw) -> Raw
conjoin [] = `(() : Type)
conjoin ((h, t) :: ts) = `(DPair ~t ~(RBind h (Lam t) (conjoin ts)) : Type)

||| Get the projections of the tails of an iterated sigma type
proj' : List (TTName, Raw) -> TTName -> Raw -> Elab Raw
proj' [] n _ = fail [NamePart n, TextPart "not in iterated sigma"]
proj' ((f, t) :: fs) n x =
  if f == n
    then pure x
    else do inner <- proj' fs n x
            pure `(DPair.snd {a=~t}
                             {P=~(RBind n (Lam t) (conjoin fs))}
                             ~inner)

||| Get the projection corresponding to a particular assumption that
||| we've wrapped up in an iterated sigma.
proj : List (TTName, Raw) -> TTName -> Raw -> Elab Raw
proj (f::fs) n x =
    do inner <- proj' (f::fs) n x
       pure `(DPair.fst {a=~(snd f)}
                        {P=~(RBind (fst f) (Lam (snd f)) (conjoin fs))}
                        ~inner)
proj [] _ _ = empty

||| Rename a free variable
rename : (from, to : TTName) -> Raw -> Raw
rename from to (Var n) =
    Var $ if n == from then to else n
rename from to (RBind n b tm) =
    if n == from
      then RBind n (assert_total $ map (rename from to) b) tm
      else RBind n (assert_total $ map (rename from to) b) (rename from to tm)
rename from to (RApp tm tm') =
    RApp (rename from to tm) (rename from to tm')
rename from to RType = RType
rename from to (RUType u) = RUType u
rename from to (RConstant c) = RConstant c

mutual
  ||| Compute the free variables of a term.
  fv : Raw -> List TTName
  fv (Var n) = [n]
  fv (RBind n b tm) = nub (assert_total $ fvB b) ++ delete n (fv tm)
  fv (RApp tm tm') = nub $ fv tm ++ fv tm'
  fv RType = []
  fv (RUType x) = []
  fv (RConstant c) = []

  ||| Compute the free variables of a binder.
  fvB : Binder Raw -> List TTName
  fvB (Lam ty) = fv ty
  fvB (Pi ty kind) = nub $ fv ty ++ fv kind
  fvB (Let ty val) = nub $ fv ty ++ fv val
  fvB (Hole ty) = fv ty
  fvB (GHole ty) = fv ty
  fvB (Guess ty val) = nub $ fv ty ++ fv val
  fvB (PVar ty) = fv ty
  fvB (PVTy ty) = fv ty

||| Substitute a free variable, avoiding capture as necessary with gensyms.
subst : (from : TTName) -> (to : Raw) -> Raw -> Elab Raw
subst from to (Var n) =
  if from == n then pure to else pure (Var n)
subst from to (RBind n b tm) =
  do b' <- assert_total $ traverse (subst from to) b
     if from == n
       then do n' <- gensym "bound"
               RBind n' b' <$> assert_total (subst from to (rename n n' tm))
       else RBind n b' <$> subst from to tm
subst from to (RApp tm tm') = [| RApp (subst from to tm) (subst from to tm') |]
subst from to RType = pure RType
subst from to (RUType u) = pure (RUType u)
subst from to (RConstant c) = pure (RConstant c)

||| Perform multiple substitutions.
substs : List (TTName, Raw) -> Raw -> Elab Raw
substs [] tm = pure tm
substs ((n,t)::ss) tm = subst n t tm >>= substs ss


parameters (inFun : TTName, outFun : TTName, accPred : TTName)

  ||| The algorithm from Definition 5 in the paper. Given a pattern
  ||| variable context and a right-hand side, compute the additional
  ||| pattern variables and the new right-hand side to make the
  ||| function total with respect to the accessibility predicate.
  covering
  translate : Nat -> List (TTName, Raw) -> Raw -> Elab (List (TTName, Raw), Raw)
  translate arity gamma (Var n) =
    case lookup n gamma of
      Just _ => pure ([], Var n)
      Nothing =>
        if n == inFun
          then fail [TextPart "Cannot translate when ", NamePart inFun, TextPart "is not fully applied."]
          else pure ([], Var n)

  translate arity gamma (RBind n (Lam ty) tm) =
    do (phi', tm') <- translate arity (gamma ++ [(n, ty)]) tm
       case phi' of
         [] => pure ([], RBind n (Lam ty) tm')
         [(n', t')] =>
           do H <- gensym "H"
              let phi'' = [(H, RBind n (Pi ty `(Type)) t')]
              newBody <- subst n' (RApp (Var H) (Var n)) tm'
              pure (phi'', RBind n (Lam ty) newBody)
         nonempty =>
           do H <- gensym "H"
              let HTy = conjoin nonempty
              let phi'' = [(H, RBind n (Pi ty `(Type)) HTy)]
              toSubst <- for nonempty $ \(n', _) =>
                           do to <- proj nonempty n' (RApp (Var H) (Var n))
                              pure (n', to)
              newBody <- substs toSubst tm'
              pure (phi'', RBind n (Lam ty) newBody)
  translate arity gamma (RBind n b tm) = fail [TextPart "unsupported binding form"]
  translate arity gamma (RApp tm tm') =
    case unApply (RApp tm tm') of
      (Var n, args) =>
        do (phis, args') <- List.unzip <$> traverse (translate arity gamma) args
           (recPhi, recArg) <-
             if n == inFun
               then
                 if length args /= arity
                   then fail [TextPart "Cannot translate when ", NamePart inFun, TextPart "is not fully applied."]
                   else
                     do h <- gensym "h"
                        let hTy = mkApp (Var accPred) args'
                        pure ([(h, hTy)], [Var h])
               else pure ([], [])
           pure (concat phis ++ recPhi, mkApp (Var (if n == inFun then outFun else n)) (args' ++ recArg))
      (op, args) => fail [TextPart "can't apply", RawPart op]
  translate arity gamma RType = pure ([], RType)
  translate arity gamma (RUType u) = pure ([], RUType u)
  translate arity gamma (RConstant c) = pure ([], RConstant c)

  ||| Wrapper around `translate` to create new definitions and constructors suitable for Idris.
  ||| @ which which constructor is this?
  ||| @ clauses remaining pattern-matching clauses
  export covering
  processClauses : (which : Nat) -> (clauses : List (FunClause TT)) -> Elab (List (FunClause Raw, ConstructorDefn))
  processClauses which [] = pure []
  processClauses which (MkImpossibleClause _ :: xs) = processClauses which xs
  processClauses which (MkFunClause lhs rhs :: xs) =
    do (gamma, lhsBody) <- patvarArgs <$> (forget lhs)
       (_,     rhsBody) <- patvarArgs <$> (forget rhs)
       let cn = mkN which accPred
       let pats = snd (unApply lhsBody)
       (phi, newRhs) <- translate (length pats) gamma rhsBody
       let newLhs = mkApp (Var outFun)
                          (pats ++ [mkApp (Var cn) (map (Var . fst) (gamma ++ phi))])
       let ctor = Constructor cn ([MkFunArg n ty Implicit NotErased | (n, ty) <- gamma] ++
                                  [MkFunArg n ty Explicit NotErased | (n, ty) <- phi])
                                 (mkApp (Var accPred) pats)
       let withPats = \tm => foldr (\nty, body : Raw => RBind (fst nty) (PVar (snd nty)) body) tm (gamma ++ phi)
       rest <- processClauses (S which) xs
       pure $ (MkFunClause (withPats newLhs) (withPats newRhs), ctor) :: rest
    where
      mkN : Nat -> TTName -> TTName
      mkN i (NS n ns) = mkN i n
      mkN i (UN n) = UN (n ++ "Case" ++ show i)
      mkN i (MN j str) = MN j (str ++ "Case" ++ show i)
      mkN i _ = UN $ "x_Case" ++ show i

  ||| Perform the Bove-Capretta translation.
  export covering
  bc : Elab ()
  bc =
    do (inFun', Ref, inTy) <- lookupTyExact inFun
         | _ => fail [NamePart inFun, TextPart "does not uniquely determine a function"]
       -- Step one: declare the datatype and function
       -- The datatype will take the same args as the initial function.
       (_, args, res) <- lookupArgsExact inFun'
       declareDatatype $ Declare accPred args `(Type)
       -- The function will recurse over the datatype
       declareType $ Declare outFun (args ++ [MkFunArg `{{acc}} (mkApp (Var accPred) (map (Var . name) args)) Explicit NotErased]) res

       -- step two: for each clause, make a corresponding constructor
       DefineFun _ oldClauses <- lookupFunDefnExact inFun'
       (newClauses, ctors) <- unzip <$> processClauses Z oldClauses
       defineDatatype $ DefineDatatype accPred ctors

       -- step three: define the function by recursion over the datatype
       defineFunction $ DefineFun outFun newClauses
       pure ()

||| Perform the Bove-Capretta transformation, then generate a new
||| version of the function that is total, assuming that a proof
||| obligation is fulfilled.
||| @ inFun the program to transform
||| @ outFun the name for the new underlying function
||| @ wrapper the name for the generated wrapper
||| @ accPred the name for the generated accessibility predicate
||| @ totalProof the name to use for the totality proof obligation
export covering
bc' : (inFun, outFun, wrapper, accPred, totalProof : TTName) -> Elab ()
bc' inFun outFun wrapper accPred totalProof =
    do bc inFun outFun accPred
       (_, args, res) <- lookupArgsExact inFun
       declareType $ Declare wrapper args res
       declareType $ Declare totalProof args (mkApp (Var accPred) (map (Var . name) args))
       let withPats = \tm => foldr (\arg, body : Raw => RBind (name arg) (PVar (type arg)) body)
                                   tm
                                   args
       defineFunction $ DefineFun wrapper [
           MkFunClause (withPats (mkApp (Var wrapper) (map (Var . name) args)))
                       (withPats (mkApp (Var outFun) (map (Var . name) args ++
                                                      [mkApp (Var totalProof)
                                                             (map (Var . name) args)])))
         ]

decl syntax "toBC" {inFun} "==>" {outFun} "|" {pred} =
  %runElab (bc `{inFun} `{{outFun}} `{{pred}})

decl syntax "makeTotal" {inFun} "==>" {outFun} "|" {pred} "as" {wrapper} "by" {totalProof} =
  %runElab (bc' `{inFun} `{{outFun}} `{{wrapper}} `{{pred}} `{{totalProof}})
