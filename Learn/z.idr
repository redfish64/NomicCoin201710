
%default total

data Foo : Type where
  Fee : Foo

x : Foo
x = Fee

data Nat : Type where
  Z : Nat
  S : (k:Nat)         -> Nat
  

