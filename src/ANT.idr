import Data.Vect
||| Modeled from https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf page 10

data ANTContext : {n: Nat} -> Type

data Binding : ANTContext -> Type

||| All contexts that are definable using this structure are valid.
|||
||| They can have variables added into them as specified by Binding
data ANTContext : {n: Nat} -> Type where
  Empty : ANTContext {n=0}
  AddBinding : (ctx : ANTContext {n=n}) 
                -> (b : Binding ctx)
                -> ANTContext {n=S n}

data ANTExp : ANTContext -> Type

||| This simple returns TypeN
||| Since data constructors can't recursively reference each other, this
||| breaks the loop
mkTypeN : (g : ANTContext) -> Nat -> ANTExp g

typeForExp : {g : ANTContext} -> ANTExp g -> ANTExp g


||| Context B encompass context A if any expression that is implied
||| by A can also be implied by B. This means that no matter what vars
||| are added to a context, it still implies *at least* the same
||| expressions as it did before. B is a descendent of A
|||
||| In other words, an *inner* context can reference anything from
||| an *outer* context, and all context applied in the outer also
||| apply to the inner.
|||
||| Ex: let x = (15 : Int) in (let y = 3 : Int) in x + y)
|||                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ -- outer context (A), x is defined
|||      inner context (B), x and y are defined -- ^^^^^
data ContextEncompasses : (des : ANTContext) -> (anc : ANTContext)
     -> Type where
  Self : {self : ANTContext} -> ContextEncompasses self self
  Child : (b : Binding anc) -> {prf : ContextEncompasses des anc}
         -> ContextEncompasses des (AddBinding anc b)
  
||| Represents a variable into the context. Note this is not the same thing
||| as creating a Lam/Pi/Let Expression. It is rather the resulting
||| context to be used *within* the body of a lam/pi/let expression that
||| has been created. Ex:
|||
||| Let x = (16 : Int) in (....)
|||                        ^^^^-- binding should be added to context for this part
|||
||| See ANTExp.Lam to create a Lam, etc.
|||
data Binding : (ctx : ANTContext) -> Type where
  |||  G |- S' : Type(i)
  |||  ------------------
  |||  G;\x:S' |- valid
  LamBinding : (S' : ANTExp ctx) 
            -> (typeForExp S' = (mkTypeN ctx i)) -> Binding ctx

  |||  G |- S' : Type(i)
  |||  ------------------
  |||  G;forall x:S' |- valid
  PiBinding : (S' : ANTExp ctx) 
            -> (typeForExp S' = (mkTypeN ctx i)) -> Binding ctx

  |||  G |- S' : Type(i)    G |- s : S'
  |||  --------------------------------
  |||  G;let x = s : S' |- valid
  LetBinding : (S' : ANTExp ctx) 
            -> (typeForExp S' = (mkTypeN ctx i)) 
            -> (s : ANTExp ctx)
            -> (typeForExp s = S')
            -> Binding ctx


--isEquivalentExp : ANTExp des -> ANTExp anc -> ContextEncompasses des anc -> Type

pushDownType : (b: Binding g) -> ANTExp g -> ANTExp (AddBinding g b)
pushDownType b x = (ContextEncompassesImpliesSame 

||| This is a *valid* expression of the context. An expression can
||| only be created if a context implies it by running one or more of
||| its base rules
|||
||| Basically if a rule ends in:
|||    ....
|||    -------
|||    G |- x
||| then this represents that "G |- x" is true, according to the rules
data ANTExp : ANTContext -> Type where

  ||| This is our own made up rule. It's not in the design doc, but it
  ||| inherently implied:
  |||
  |||  G |- X
  |||  ---------
  |||  G;a |- X
  |||
  ||| It means that if you add a new variable, a to G, everything that
  ||| G implied before is still valid
  ContextEncompassesImpliesSame :
    (ce : ContextEncompasses desc anc) -> ANTExp anc -> ANTExp desc

  |||       G |- valid
  |||  Type ------------
  |||       G |- Type(n) : Type(n+1)
  ||| Part 1: Every context implies Type(n) exists
  ||| Part 2 of this, that TypeN has a Type(n+1) is in typeForExpr
  TypeN : (g : ANTContext) -> (n : Nat) -> ANTExp g
  
  |||       (\x:S) in G
  |||  Var1 ------------
  |||       G |- x : S
  |||
  |||       (forall x:S) in G
  |||  Var2 ------------
  |||       G |- x : S
  |||
  |||       (let x = s : S) in G
  |||  Var3 ------------
  |||       G |- x : S
  ||| Part 1: A valid var lookup
  ||| Part 2 of this, that x has a type of S, is in typeForExpr
  Var : (g : ANTContext {n} ) -> (n : Fin n) -> ANTExp g

  |||         G;forall x:S' |- T : Type(m)    G |- S' : Type(n)
  ||| Forall  ------------------------------------------------- (Exists(p).m <= p, n <= p)
  |||         G |- (x : S') -> T : Type(p)
  |||
  ||| Note that T is in (G;forall x:S'), but even so, (x : S') -> T is
  ||| in G The idea being if you can create a context from G that has
  ||| the additional "forall x:S'" in it and it implies that T exists,
  ||| then you can say that "(x : S') -> T" aka "forall x:S'.T" is implied
  ||| in G
  -- TODO 2 this only represents that (x : S') -> T is created, it doesn't specify
  --  the type as Type(p).. should we specify type in typeForExp??? seems so
  Pi : { n : Nat }
    -> { m : Nat }
    -> (g : ANTContext) 
    -> (S' : ANTExp g) 
    -> (prf : typeForExp S' = (mkTypeN g n))
    -> (T : ANTExp (AddBinding g (PiBinding S' prf)))
    -> ANTExp g


  |||     G;\x:S' |- e : T   G |- (x : S') -> T : Type(n)  
  ||| Lam ---------------------------------------------
  |||     G |- \x:S'.e : (x : S') -> T
  |||
  ||| Note, not in rule, but implied (S' : Type(n))... must be because
  ||| that's the only way to create G;\x:S', see Binding
  |||
  ||| Given a context, an arg type within that context, and an expression
  ||| that uses that var, returns a Lam.
  Lam : (g : ANTContext {n=i}) 
    -> (S' : ANTExp g) 
    -> (prf : typeForExp S' = (mkTypeN g i)) -- necessary because LamBinding needs it
    -> (T : ANTExp g) 
    -> (e : ANTExp (AddBinding g (PiBinding S' prf)))
    -> (typeForExp e) = (pushDownType (PiBinding S' prf) T)
    -> ANTExp g



--    -> (prf2 : typeForExp {g=(AddBinding g (PiBinding S' prf))} e)
-- -> (isEquivalentExp (typeForExp e) ?T ?Parent)
-- mkContextLamType : {g : ANTContext} -> {S' : ANTExp g} -> Type
-- mkContextLamType {g} = Bool 

--    -> (typeForExp (Pi g S' prf ?lemmaVarStillExistsInUpdatedContext) = (mkTypeN g k))
mkTypeN g k = TypeN g k


{-

-- lemmaVarStillExists : (g : ANTContext) -> (e : ANTExp g) 
--                     -> (e2 : ANTExp (AddVar g binder 


lemmaLamTypeOfTypeIsTypeN : (n : Nat) -> (argType : ANTExp g) -> (i : Nat) 
                    -> typeForExp argType = mkTypeN g i

lemmaPiTypeOfTypeIsTypeN : (n : Nat) -> (argType : ANTExp g) -> (i : Nat) 
  -> typeForExp argType = mkTypeN g i


    

typeForExp (TypeN g n) = TypeN g (S n)
--typeForExp (Lam g argType exp) = ?typeForExp_rhs_2
typeForExp (Pi g S' prf T) = TypeN g ?chooseNForPi 
  where 
    chooseNForPi : {g : ANTContext} -> ANTExp g -> ANTExp g -> Nat
    chooseNForPi x y = ?chooseNForPi_rhs
    

{-    
||| All contexts that are definable using this structure are valid.
|||
||| They can have variables added into them.  Variables come in 3
||| times, Lambda, Forall, and Let
|||
||| TODO 3 - Not sure why we need seperate types, idris impl decl says
||| "We record the binding ||| form in the context, as well as the
||| type, because type checking relies on evaluation. In particular,
||| let bound names may reduce."
--TODO 2.5 do we need "n"?
data ANTContext :  {n : Nat} -> Type where
  |||
  |||  --------
  |||  |- valid
  Empty : ANTContext {n}
  
  |||  G |- S' : Type(i)
  |||  ------------------
  |||  G;\x:S' |- valid
  |||
  |||  G |- S' : Type(i)
  |||  ------------------
  |||  G;forall x:S' |- valid
  |||
  ||| Adds a variable into the context. Note this is not the same
  ||| thing as creating a Lam Expression. It is rather the resulting
  ||| context to be used *within* the body of a lam expression that
  ||| has been created
  |||
  ||| See ANTExp.Lam to create a Lam, etc.
  AddVar : {i : Nat} -> {n : Nat} -> (ctx : ANTContext {n=n})
    -> (binder : Binder)
    -> (S' : ANTExp ctx)
    -> (typeForExp S' = (mkTypeN ctx i))
    -> ANTContext {n=n} -- HACK {n=S n}

    |||  G |- S : Type(i)
  |||  ------------------
  |||  G;forall x:S |- valid
  AddForallVar : {S : Expr} -> {i : Nat} -> (ctx : ANTContext) 
    -> ImpliesExpr ctx (TypedExp S (TypeN i)) 
    -> ANTContext {S n}
  
  |||  G |- S : Type(i)   G |- s : S
  |||  --------------------------------
  |||  G;let x => s : S |- valid
  AddForallVar : {s : Expr} -> {S : Expr} -> {i : Nat} -> (ctx : ANTContext) 
    -> ImpliesExpr ctx (TypedExp S (TypeN i)) 
    -> ImpliesExpr ctx (TypedExp s S) 
    -> ANTContext {S n}



data ImpliesExpr : ANTContext -> TypedExp -> Type
  
 
  
||| Context
||| @n total number of vars
data ANTContext : {n : Nat} -> Type
data Expr : ANTContext -> Type
data ANTTypedExp : {ctx : ANTContext} -> (exp : Expr ctx) -> (typ : Expr ctx) -> Type where
 
 
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
    -> Ex
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
  Type0OfIntT : (g : ANTContext) -> ANTTypedExp IntT (TypeN 0 g)

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
-}

{-

||| Constants, c ::= Type i  (type universes)
|||                | i (integer literal)
|||                | str (string literal)
data ANTConst : Type where
  CType : Nat -> ANTConst
  CInt : Int -> ANTConst
  CStr : String -> ANTConst
  
||| Binders, b ::= \x : t (abstraction)
|||              | let x => t : t (let binding)
|||              | forall x : t (function space)
data ANTBinder : Type where
  BAbs : {n : Nat} -> ANTBinder
  BLet : {n : Nat} -> ANTBinder
  BForall : {n : Nat} -> ANTBinder

||| Terms, t ::= c (constant)
|||            | x (variable)
|||            | b. t (binding)
|||            | t t (application)
|||            | T (type constructor)
|||            | D (data constructor)
data ANTTerm : Type where
  TConst : ANTConst -> ANTTerm
  ||| each var has a name that must be unique
  TVar : String -> ANTTerm    
  TBinding : ANTBinder -> ANTTerm
  TApp : ANTTerm -> ANTTerm -> ANTTerm
  TTypeCons : ANTTerm -- TODO 2 how to represent this?
  TDataCons : ANTTerm -- TODO 2 how to represent this?
  
data ContextualExpr : ANTTerm -> Type where
  TConst : 

data ANTContext :  {n : Nat} -> Type where
  |||
  |||  --------
  |||  |- valid
  Empty : ANTContext {n}
  
  |||  G |- S : Type(i)
  |||  ------------------
  |||  G;\x:S |- valid
  AddLambdaVar : (n : Nat) -> (i : Nat) -> (varName : String) 
    -> (ctx : ANTContext {n}) 
    -> (S : ContextualExpr ctx (TypeN i))
    -> ANTContext {n=S n}

-}
-}
