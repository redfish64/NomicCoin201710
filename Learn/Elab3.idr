module Elab3

import BC
import public Language.Reflection.Utils

%language ElabReflection

quicksort : List Nat -> List Nat
quicksort [] = []
quicksort (x::xs) =
  quicksort (filter (< x) xs) ++
  x :: quicksort (filter (>= x) xs)

--%runElab bc `{quicksort} `{{qs'}} `{{QSAcc}}
-- %runElab bc' `{quicksort} `{{qs'}} `{{qs2}}  `{{QSAcc}} `{{Elab3.qsProof}}

-- elab3.qsProof = ?todo


mkHole : String -> Raw -> Elab TTName
mkHole n ty = 
  do s <- gensym n
     claim s ty
     pure s

auto : Elab () 
auto = do g <- goalType 
          case g of 
            `(() : Type) => 
              exact `(() : ()) 
            `((~A, ~B) : Type) => 
             do a <- mkHole "A" A 
                b <- mkHole "B" B 
                exact `(MkPair {A=~A} {B=~B} 
                          ~(Var a) ~(Var b)) 
                focus a; auto 
                focus b; auto 

goal : ((), ((), ())) 
goal = %runElab auto 


partial 
auto' : Elab () 
auto' = do compute 
           attack 
           try intros 
           hs <- map fst <$> getEnv 
           for_ hs $ 
             \ih => try (rewriteWith (Var ih)) 
           hypothesis <|> search' 100 [] 
           solve 
