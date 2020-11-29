--primeiro ponto

quadrado:: Int->Int
quadrado x = x*x

listaQuadrado::(f->f)->[f]->[f]
listaQuadrado _ [] = []
listaQuadrado f l = [f (e)|e<-l]

soma:: [Int]->Int
soma []=0
soma (x:xs) = x+soma xs 

somaQuadrado:: [Int]->Int
somaQuadrado l = soma(novaLista)
    where
        novaLista = listaQuadrado quadrado l


valorPositivo::[Int]->Bool
valorPositivo (x:xs)
    |xs == [] && x < 0 = False
    |xs == [] && x >= 0 = True
    |x<0 = False
    |otherwise = valorPositivo xs



--seguda parte 

--menor valor da lista 
valorMinimuAux :: [Int]->Int
valorMinimuAux l =  minimum l


valorMinimu :: [Int]->Int
valorMinimu f = result
    where
        result = valorMinimuAux f


-- segundo ponto


valorIgual:: [Int]->Bool
valorIgual (x:xs) 
    |xs == [] && x/= x = False
    |xs == [] && x== x = True
    |x/=(pega xs) = False
    |otherwise = valorIgual xs 


valorIgualAux :: [Int]->Bool
valorIgualAux l = result
    where
        result = valorIgual l 


pega::[Int]->Int
pega l = result
    where 
        result = l!!0




valorMairoZero :: [Int]->Bool
valorMairoZero (x:xs)
    |xs == [] && x<0 = False
    |xs == [] && x>=0 = True
    |x<0 = False
    |otherwise = valorMairoZero xs


valorOrdem :: [Int]->Bool
valorOrdem (x:xs)
    |xs ==[] = True
    |x>(pega xs) = False
    |x<(pega xs) = True
    |otherwise = valorOrdem xs
    