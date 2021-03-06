module TTUtils

import Prelude.Strings as S
import Language.Reflection.Utils
-- import Pruviloj.Core
-- import Pruviloj

%language ElabReflection

-- implementation Show TT where
--   show _ = "foo"
  
-- implementation Show x => Show (Binder x) where
--   show _ = "binder"

-- implementation Show x => Show (FunDefn x) where
--    show ttname = "TTname"

public export
record FnDesc where
  constructor MkFnDesc
  nameType : NameType
  fnType : TT
  fnDef : (FunDefn TT)
  
-- implementation Show FnDesc where
--   show fd = 
--     "nameType: " ++ (show (nameType fd)) ++ "\n"
--     ++ "fnType: " ++ (show (fnType fd)) ++ "\n"
--     ++ "fnDef: " ++ (show (fnDef fd)) ++ "\n"

public export
getTTForFn : TTName -> Elab ()
getTTForFn n =
  do 
    (_,nt,t) <- lookupTyExact n
    fnd <- lookupFunDefnExact n
    fill `(MkFnDesc ~(quote nt) ~(quote t) ~(quote fnd))
    --    fill `((~(quote nt)))
    solve
    
bar : Int -> Int
bar xxx = 42    

bar2 : Nat -> Nat
bar2 xxx = S xxx

bar3 : Nat -> Nat
bar3 = \x => S x

bar4 : (n : Nat) -> Nat
bar4 = \x => S x

bar5 : Nat -> Nat
bar5 Z = Z
bar5 (S k) = k

foo' : FnDesc
foo' = %runElab (getTTForFn `{bar})

foo2 : FnDesc
foo2 = %runElab (getTTForFn `{bar2})
  
foo3 : FnDesc
foo3 = %runElab (getTTForFn `{bar3})
  
foo4 : FnDesc
foo4 = %runElab (getTTForFn `{bar4})

foo5 : FnDesc
foo5 = %runElab (getTTForFn `{bar5})
  
