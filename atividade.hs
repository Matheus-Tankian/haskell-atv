--1 Defina a função potencia :: Int -> Int -> Int em Haskell, que calcula a potência do primeiro
--parâmetro pelo segundo, considerando que o expoente será sempre positivo.

potencia :: Int -> Int -> Int
potencia x y 
    |y == 0 = 1
    |otherwise = x * potencia x (y-1)



--2 Implemente uma função nPrimo :: Int -> Int em Haskell, que recebe um número n como parâmetro
--e encontre o enésimo primo.

ehPrimo:: Int->Bool
ehPrimo x 
    |x < 2 = False
    |otherwise = ehPrimoAux x 2

ehPrimoAux:: Int->Int->Bool
ehPrimoAux x y
    |y*y > x = True
    |mod x y == 0  = False
    |otherwise = ehPrimoAux x (y+1)

ehTeste:: Int->Int->Int->Int
ehTeste x y a 
    |y == a = (x-1)
    |ehPrimo x == True = ehTeste (x+1) (y+1) a
    |otherwise = ehTeste (x+1) y a

homePrimo :: Int->Int
homePrimo x 
    |x < 2 = 0
    |otherwise = ehTeste 2 0 x 


-- 3 Construa a função fibonacciPrimo :: Int -> Int que retorna o enésimo número primo da sequência de
--Fibonacci.

getNum :: Int->[Int]
getNum n = ([(fib x)|x <- [1..n], fibAux x == True])


getFun n x
    |(length(getNum n) == x) = getNum n
    |otherwise = getFun (n+1) x


homeFib n
    |otherwise = getFun 1 n 


fibAux:: Int->Bool
fibAux x 
    |x < 2 = False
    |otherwise = ehPrimo(fib x)


fib :: Int->Int
fib x
    |x == 0 = 0
    |x == 1 = 1
    |otherwise = fib (x-1) + fib (x-2)
   

-- 4 Dado um número natural n > 0, n é dito perfeito se a soma de seus divisores, incluindo o número 1,
--é igual ao próprio n. O primeiro número natural perfeito é o número 6, porque 6 = 1 + 2 + 3. Defina
--uma função ehPerfeito(n) que informe se n é, ou não, um número perfeito.


ehPerfeitoAux :: Int->[Int]
ehPerfeitoAux num =[x| x <- [1 .. num-1],((mod num x ) == 0 )]

ehPerfeito :: Int-> Bool
ehPerfeito n 
    |(sum(ehPerfeitoAux n) == n) = True
    |otherwise = False


--5 Considere o algoritmo a seguir que gera uma sequência de números naturais não nulos, a partir de
--um número natural n > 0. Se n for par, divida-o por 2. Se n for ímpar, multiplique-o por 3 e some 1.
--Repita este processo com o novo valor de n, até que ele seja igual a 1, se for possível. Por
--exemplo, para n = 22, a sequência é: 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2 e 1. Para
--cada n, define-se o tamanho do ciclo de n como a quantidade de números da sequência gerada,
--incluindo o número 1. No exemplo acima, o tamanho do ciclo para n = 22 é 16. Defina uma função
--tamciclo(n) que dê como resultado o tamanho do ciclo de n.



homeTamCiclo :: Int->Int
homeTamCiclo n 
    |n == 0 = 0
    |otherwise = tamCiclo n 1

tamCiclo :: Int -> Int -> Int
tamCiclo a count
     | a == 1 = count
     | mod a 2 == 0 =  tamCiclo (div a 2) (count + 1)
     | otherwise = tamCiclo ((a*3)+1) (count + 1)
