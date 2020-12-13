--1
fatia :: [Char]->Int->Int->[Char]
fatia l i j = result
    where
        result = (drop (i-1) (take j l))



--2
contaOcorrenciaNumero :: Int -> [Int] -> Int
contaOcorrenciaNumero p (h:t)
    | t==[] && p == h = 1
    | t==[] = 0
    | p == h = 1 + contaOcorrenciaNumero p t
    | otherwise = contaOcorrenciaNumero p t

removeNumero :: Int -> [Int] -> [Int]
removeNumero p (h:t)
    | t==[] && (p /= h) = [h]
    | t==[] = []
    | p == h = removeNumero p t
    | otherwise = h : removeNumero p t

ocorrenciaNumero :: [Int] -> [(Int,Int)]
ocorrenciaNumero [] = []
ocorrenciaNumero (h:t)
    |otherwise = (h,contaOcorrenciaNumero h (h:t)) : ocorrenciaNumero (removeNumero h (h:t))

ordena:: [(Int,Int)] -> [(Int,Int)]
ordena [] = []
ordena (s:xs) = ordena [x|x <- xs,snd x < snd s] ++ [s] ++ ordena [x|x <- xs,snd x >= snd s]

--main
ordenaOcorrencia:: [Int] -> [Int]
ordenaOcorrencia lista = [fst x| x <- ordena (ocorrenciaNumero lista)]



--3

palavraIgual :: [Char] -> [Char] -> Bool
palavraIgual p1 p2 = p1 == p2
    
    
contaOcorrencia :: [Char] -> [[Char]] -> Int
contaOcorrencia p (h:t)
    | t==[] && palavraIgual p h = 1
    | t==[] = 0
    | palavraIgual p h = 1 + contaOcorrencia p t
    | otherwise = contaOcorrencia p t

removePalavra :: [Char] -> [[Char]] -> [[Char]]
removePalavra p (h:t)
    | t==[] && not (palavraIgual p h) = [h]
    | t==[] = []
    | palavraIgual p h = removePalavra p t
    | otherwise = h : removePalavra p t
--main
ocorrencia :: [[Char]] -> [([Char],Int)]
ocorrencia [] = []
ocorrencia (h:t)
    |otherwise = (h,contaOcorrencia h (h:t)) : ocorrencia (removePalavra h (h:t))



--4

contaOcorrenciaCaractere :: Char -> [Char] -> Int
contaOcorrenciaCaractere _ [] = 0 
contaOcorrenciaCaractere p (h:t)
    | p /= h = 0
    | otherwise = 1 + contaOcorrenciaCaractere p t

removeCaractere :: Char -> [Char] -> [Char]
removeCaractere _ [] = []
removeCaractere p (h:t)
    | p == h = removeCaractere p t
    |otherwise = h:t

--h:removeCaractere p t

ocorrenciaCaractere :: [Char] -> [(Char,Int)]
ocorrenciaCaractere [] = []
ocorrenciaCaractere (h:t)
    |otherwise = (h,contaOcorrenciaCaractere h (h:t)) : ocorrenciaCaractere (removeCaractere h t)


zipTextoAux :: [(Char,Int)] -> [Char]
zipTextoAux [] = []
zipTextoAux (h:t)
    | snd h > 1 = fst h : show (snd h) ++ zipTextoAux t 
    | otherwise = fst h : zipTextoAux t

--main
zipTexto :: [Char] -> [Char]
zipTexto texto = zipTextoAux (ocorrenciaCaractere texto)




--5 parte 

multiplasFuncoes:: [(a->a)]->[a]->[a]
multiplasFuncoes func l = [f x|f<-func, x<-l]

--6 

aplicacaoExclusiva::(i->Bool)->(i->i->i)->[i]->[i]->[i]
aplicacaoExclusiva f1 f2 l1 l2 =  [f2 x y| x<-l1, y<-l2, ((f1 x)==True),((f1 y)==True)]


--7

contaVizinhosIguais::(Eq i)=>[i]->Int
contaVizinhosIguais [] = 0
contaVizinhosIguais (a:x)
  |x==[] = 1
  |a == head x = 1+contaVizinhosIguais x
  |otherwise = contaVizinhosIguais x


--8

contador::(Eq a)=>(a->Bool)->[a]->Int
contador f (a:x)
  |x==[] && f a = 1
  |x==[] = 0
  |f a ==True = 1+contador f x
  |otherwise = contador f x


--9

filtraPar ::(b->Bool)->(a->Bool)->[b]->[a]->[(b,a)]
filtraPar f g n m = [(a,b)|a<-n,b<-m,((f a)==True), ((g b)==True)]


--10

selecionaExecuta::(a->Bool)->(a->a)->[a]->[a]
selecionaExecuta f g l = [g x|x<-l,((f x)==True)]




--ex testes

--maior ::(b->Bool)->(b->Bool)->[b]->[b]
--maior n m l = [x|x<-l, (((n x)==True) && ((m x)==True) )]

maior ::(a->Bool)->(a->a)->[a]->[a]
maior n m l = [m x|x<-l, ((n x)==True)]


--9 e 10 base imcompleta 
--fazPar ::(a->a)->[i]->[j]->[(i,j)]
--fazPar f n m = [(a,b)|a<-n,b<-m]