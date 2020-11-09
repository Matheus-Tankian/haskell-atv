

--soma lista
somaLista::[Int]->Int
somaLista [] = 0
somaLista(x:xs) = x + somaLista xs

--replica numero 

replicaChar:: Char->Int->[Char]
replicaChar c n
    |n == 0 = []
    |otherwise = c:replicaChar c (n-1) 

--funcao inversa

inverte:: [Char]->[Char]
inverte [] = []
inverte (x:xs) = inverte xs ++ (x:[])

--conta Vizinhos

contaVizinhosIguais :: [Char]->Int
contaVizinhosIguais [] = 0
contaVizinhosIguais (x:xs)
    |xs ==[] = 0
    |x == head xs = 1 + contaVizinhosIguais xs
    |otherwise = contaVizinhosIguais xs

--diferente de 3 e maiores q 2

diferente :: Int -> Int -> Bool
diferente a b
  |a == b = False
  |otherwise = True

contador :: (Int->Bool) -> [Int] -> Int
contador _ [] = 0
contador f (a:x)
  | f(a) == True = 1 + contador f x
  |otherwise = contador f x



ehFator:: Int->[Int]
ehFator num  = [n|n<-[1..num], ((mod num n) == 0)]


ehPerfeito :: Int->[Int]
ehPerfeito num = [x| x <- [1 .. num-1],((mod num x ) == 0 )]


fazPar :: [Int]->[Int]->[(Int,Int)]
fazPar n m = [(a,b)|a<-n,b<-m]



type Valor = ([Int],[Int])

exemploConcat:: Valor->[Int]
exemploConcat (a,b) = a++b  