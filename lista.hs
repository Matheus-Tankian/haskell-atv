


--questao 1

impares :: [Int]
impares = [x|x<-[1..100],((mod x 2)==1)]

pares:: [Int]
pares = [x|x<-[10..100],((mod x 2)==0)]

impares2 :: Int->[Int]
impares2 n = [x|x<-[1..n],((mod x 2)==1)]

multiplos :: Int->[Int]
multiplos n = [x|x<-[1..n],((mod x 3)==0),((mod x 5)==0)]

type Quadrado = [(Int, Int)]

quadrado ::Int->Quadrado
quadrado x = [(n,n*n)|n<-[1..x]]

--2 questao 

listaFibonacci :: Int->[Int]
listaFibonacci n = ([(fib x) |x <- [1..n]])



fib :: Int->Int
fib x
    |x == 0 = 0
    |x == 1 = 1
    |otherwise = fib (x-1) + fib (x-2)


    
type Matrix = [(Int,Int)]



--Matriz 3x4

matrizMostra :: Matrix 
matrizMostra = [(x,y)|x<-[1..3], y<-[1..4]]

--Matriz NxM

matrizValor :: Int->Int->Matrix
matrizValor a b = [(x,y)|x<-[1..a],y<-[1..b]] 






--3 questao


funHexaAux "0000" = "0"
funHexaAux "0001" = "1"
funHexaAux "0010" = "2"
funHexaAux "0011" = "3"
funHexaAux "0100" = "4"
funHexaAux "0101" = "5"
funHexaAux "0110" = "6"
funHexaAux "0111" = "7"
funHexaAux "1000" = "8"
funHexaAux "1001" = "9"
funHexaAux "1010" = "A"
funHexaAux "1011" = "B"
funHexaAux "1100" = "C"
funHexaAux "1101" = "D"
funHexaAux "1110" = "E"
funHexaAux "1111" = "F"


funHexa:: String->String
funHexa "" = ""
funHexa s
    |mod (length s) 4 /=0 = funHexa ("0"++s)
    |otherwise = funHexaAux (take 4 s) ++ funHexa (drop 4 s)

--questao 4 hanoi

funHanoi:: Int->String->String->String->String
funHanoi x t1 t2 t3
    |x == 1 = t1 ++"->"++ t3
    |otherwise = (funHanoi(x-1) t1 t3 t2) ++"||"++ (funHanoi 1 t1 t2 t3) ++"||"++ (funHanoi(x-1) t2 t1 t3)