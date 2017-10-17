import Data.Vect
 
||| Constant Types
data ANTConst = I Int | BI Integer | Fl Double | Ch Char | Str String
 
||| Context
||| @n total number of vars
data ANTContext : {n : Nat} -> Type
data Expr : ANTContext -> Type
 
||| Modeled from https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf page 10
data ANTContext : {n : Nat} -> Type where
  |||
  |||  --------
  |||  |- valid
  Empty : ANTContext {n=0}
  
  |||  G |- S : Type(i)
  |||  ------------------
  |||  G;\x:S |- valid
  AddVar : (ctx : ANTContext {n}) -> Expr ctx -> ANTContext {n=(S n)}

   
getExprLemma1 : ANTContext -> Expr ctx -> Expr (AddVar ctx val)

getExpr : {n : Nat} -> (ctx : ANTContext {n}) -> Fin n -> Expr ctx
getExpr {n = Z} Empty FZ impossible
getExpr {n = Z} Empty (FS _) impossible
getExpr {n = (S k)} (AddVar ctx val) FZ = getExprLemma1 ctx val
getExpr {n = (S k)} (AddVar ctx val) (FS x) = ?getExpr_rhs_3
   
betaEq : Expr ctx -> Expr ctx -> Type
betaEq = ?x
 
||| This represents when a context implies something. It is laid out
||| as the basic rules of the type theory system
||| Modeled from https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf page 11
data Expr : ANTContext -> Type where
  |||  G |- valid
  |||  ------------
  |||  G |- Type(n) : Type(n+1)
  TypeN : (n : Nat) -> ANTContext -> Expr

  |||  G |- valid
  |||  ------------
  |||  G |- i : Type(0)
  ConstI : {n : Nat} -> ANTContext {n} -> 
  
  |||     G |- f : (x : S) -> T   G |- s : S
  ||| App ----------------------------------
  |||     G |- f s : T[s/x]
  App : (ctx : ANTContext {n})
    -> (fInd : Fin n)
    -> (sInd : Fin n)
    -> betaEq (getExpr ctx fInd) (getExpr ctx sInd)
    -> Expr ctx
  |||     G;\x:S |- e : T   G |- (x : S) -> T : Type(n)
  ||| Lam ---------------------------------------------
  |||     G |- \x:S.e : (x : S) -> T
  ||| Given a context, an arg type within that context, and an expression
  ||| that uses that var, returns a Lam (variable name not specified
  ||| since we are using DeBrujin indexes)
  Lam : (ctx : ANTContext) -> (argType : Expr ctx)
    -> Expr (AddVar ctx argType) -> Expr ctx


getExprLemma1 x y = ?getExprLemma1_rhs
