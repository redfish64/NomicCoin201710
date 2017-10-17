import Data.Vect
 
||| Constant Types
data ANTConst = I Int | BI Integer | Fl Double | Ch Char | Str String
 
||| Context
||| @n total number of vars
data ANTContext : {n : Nat} -> Type
data Expr : ANTContext -> Type
 
data ANTContext : {n : Nat} -> Type where
  Empty : ANTContext {n=Z}
  AddVar : (ctx : ANTContext {n}) -> (value : Expr ctx) -> ANTContext {n=(S n)}
 
getExpr : {ctx : ANTContext} -> (n : Nat) -> ANTContext {n} -> Fin n -> Expr ctx
getExpr Z Empty _ impossible
getExpr (S k) (AddVar c value) FZ = value
getExpr (S k) (AddVar c value) (FS x) = getExpr k ctx x

betaEq : Expr ctx -> Expr ctx -> Type
betaEq = ?x
 
||| This represents when a context implies something. It is laid out
||| as the basic rules of the type theory system
||| Taken from https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf page 11
data Expr : ANTContext -> Type where
  |||     G |- x : T
  |||
--  ValidExpr : (ctx : ANTContext)

  |||     G |- f : (x : S) -> T   G |- s : S
  ||| App ----------------------------------
  |||     G |- f s : T[s/x]
  App : (ctx : ANTContext {n})
    -> (fInd : Fin n)
    -> (sInd : Fin n)
    -> betaEq (getExpr n ctx fInd) (getExpr n ctx sInd)
    -> Expr ctx
    
  |||     G;\x:S |- e : T   G |- (x : S) -> T : Type(n)
  ||| Lam ---------------------------------------------
  |||     G |- \x:S.e : (x : S) -> T
  ||| Given a context, an arg type within that context, and an expression
  ||| that uses that var, returns a Lam (variable name not specified
  ||| since we are using DeBrujin indexes)
  Lam : (ctx : ANTContext) -> (argType : Expr ctx)
    -> Expr (AddVar ctx argType) -> Expr ctx

  
