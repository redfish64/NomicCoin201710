{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


--http://okmij.org/ftp/Haskell/impredicativity-bites.html

type P = forall a. a -> [a]

prd :: P
prd x = [x]

tprd1 = case prd prd of [f] -> f True


-- tprd2 = case prd prd of [f] -> (f True, f 'a')
-- Error: Couldn't match expected type `Bool' with actual type `Char'

newtype I = I{unI :: forall a. a -> [a]}

imprd :: I
imprd = I (\x -> [x])

tim = case unI imprd imprd of [I f] -> (f True, f 'a')

type family Elem x :: *
type instance Elem [a]       = a
type instance Elem (Maybe a) = a
