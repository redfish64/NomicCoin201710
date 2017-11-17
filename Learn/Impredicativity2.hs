{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls #-}
data False    -- No constructors

data R :: * -> * where
  MkR :: (c (c ()) -> False) -> R (c ())

cond_false :: R (R ()) -> False
cond_false x@(MkR f) = f x

-- May cause divergence in the type checker
absurd :: False
absurd = cond_false (MkR cond_false)

absurd2 :: False
absurd2 = let x = MkR cond_false in cond_false x


data R' = MkR' (R' -> False)

foo :: R' -> False
foo (MkR' prf) =
  prf (MkR' foo)

foo2 :: False
foo2 = let r = MkR' foo
       in foo r
         
    


