import Data.Vect
 
||| Context
||| @n total number of vars
data ANTContext : {n : Nat} -> Type
data Expr : ANTContext -> Type
data ANTTypedExp : (ctx : ANTContext) -> Type 
 
 
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

getExprLemma1 : ANTContext -> Expr ctx -> Expr (AddVar ctx unrelatedVar)

||| TODO 2 getExpr doesn't prove it's getting the right expression
||| it could return anything. We need a view
getExpr : {n : Nat} -> (ctx : ANTContext {n}) -> Fin n -> ANTTypedExp ctx
   
betaEq : Expr ctx -> Expr ctx -> Type
betaEq = ?x

-- I Int | BI Integer | Fl Double | Ch Char | Str String
 
||| This represents when a context implies something. It is laid out
||| as the basic rules of the type theory system
||| Modeled from https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf page 11
data Expr : ANTContext -> Type where
  |||  G |- valid
  |||  ------------
  |||  G |- Int : Type(0)
  ||| Part 1: the type "Int" can be implied from any context
  IntT : {ctx : ANTContext} -> Expr ctx

  |||  G |- valid
  |||  ------------
  |||  G |- Type(n) : Type(n+1)
  ||| Part 1: Every context implies Type(n) exists
  ||| Part 2 of this, that TypeN has a Type(n+1) is in data ANTType
  TypeN : (n : Nat) -> (g : ANTContext) -> Expr g

  |||  G |- valid
  |||  ------------
  |||  G |- i : Int
  IntC : {g : ANTContext} -> (i : Int) -> Expr g
  
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

||| This is an expression with an associated type.  
|||
||| Note that the type is just a plain expression, even though, every
||| expression must have a type. This is because we can't have a typed
||| expression refer to itself in it's declaration, or it becomes an
||| infinite loop.
data ANTTypedExp : {ctx : ANTContext} -> (exp : Expr ctx) -> (typ : Expr ctx) -> Type where
  |||  G |- valid
  |||  ------------
  |||  G |- Type(n) : Type(n+1)
  ||| Part 2 TypeN has a Type(n+1) is in data ANTType
  TypeSNofN : (n : Nat) -> (g : ANTContext) -> ANTTypedExp (TypeN n g) (TypeN (S n) g)
  
  |||  G |- valid
  |||  ------------
  |||  G |- Int : Type(0)
  ||| Part 2 The type of Int is Type(0)
  Type0OfIntT : (i : Int) -> (g : ANTContext) -> ANTTypedExp IntT (TypeN 0 g)

  |||  G |- valid
  |||  ------------
  |||  G |- Int : Type(0)
  ||| Part 2 The type of IntC is IntT
  IntTOfInt : (i : Int) -> (g : ANTContext) -> ANTTypedExp (IntC i) IntT


getExprLemma1 x y = ?getExprLemma1_rhs

getExpr {n = Z} Empty FZ impossible
getExpr {n = Z} Empty (FS _) impossible
getExpr {n = (S k)} (AddVar ctx val) FZ = getExprLemma1 ctx val
getExpr {n = (S k)} (AddVar ctx val) (FS x) = getExprLemma1 ctx (getExpr ctx x)
