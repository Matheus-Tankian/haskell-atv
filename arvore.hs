-- Árvore
data DBTree = Nil | Node (DBTree) Product (DBTree)

tab::Int->String
tab n
  |n==0 = ""
  |otherwise = "     " ++ tab (n-1)

display::DBTree->Int->String
display Nil n = ""
display (Node esq t dir) n = (display dir (n+1)) ++ (tab n)  ++ (show t) ++"\n"++(display esq (n+1))

instance Show DBTree where
  show arv = display arv 0

-- Produto
data Product = Nul| Product{name::String,price::Float,qnt::Int}

instance Show Product where
  show (Product name price qnt) = name++":"++(show price)++":"++(show qnt)

instance Eq Product where
    (==) (Product n1 p1 q1) (Product n2 p2 q2) = n1==n2

instance Ord Product where
    (<=) (Product n1 p1 q1) (Product n2 p2 q2) = n1<=n2

insertProduct::DBTree->Product->DBTree
insertProduct Nil p = Node Nil p Nil
insertProduct (Node esq n dir) p
  |p<n = Node (insertProduct esq p) n dir
  |p>n = Node esq n (insertProduct dir p)
  |otherwise = (Node esq n dir)


searchProduct::DBTree->String->Product
searchProduct Nil p = Nul
searchProduct (Node esq (Product name price qnt) dir) p 
    |name == p = Product name price qnt 
    |p<name = searchProduct esq p 
    |p>name = searchProduct dir p 
-- -- Ex: searchProduct arv "c"

updateProduct::DBTree->Product->DBTree
updateProduct Nil p = (Node Nil p Nil)
updateProduct(Node esq (Product name price qnt) dir) (Product nameAux priceAux qntAux)
  |nameAux == name = (Node esq(Product nameAux priceAux qntAux) dir)
  |nameAux <name = (Node(updateProduct esq(Product nameAux priceAux qntAux)) (Product name price qnt)dir)
  |nameAux > name =(Node esq (Product name price qnt) (updateProduct dir (Product nameAux priceAux qntAux))) 

  
-- -- Ex: updateProduct arv (Product "c" 5 15)

total::DBTree->Float
total Nil = 0
total(Node esq (Product name price qnt)dir) = (price* fromIntegral qnt) + total esq + total dir
-- -- Ex: total arv

sellProduct::DBTree->String->Int->DBTree
sellProduct Nil _ _ = Nil
sellProduct (Node esq(Product name price qnt)dir) nameTo qntSell
  |(nameTo == name && qnt == qntSell) = delete (Node esq (Product name price qnt)dir)
  |(nameTo == name && qnt > qntSell) = (Node esq (Product name price (qnt - qntSell)) dir)
  |nameTo < name = (Node(sellProduct esq nameTo qntSell) (Product name price qnt) dir)
  |nameTo > name = (Node esq (Product name price qnt) (sellProduct dir nameTo qntSell))
  |otherwise = (Node esq (Product name price qnt) dir)



delete::DBTree->DBTree
delete (Node Nil p dir) = dir
delete (Node esq p Nil) = esq
delete (Node esq p dir) = (Node esq(Product name price qnt) (sellProduct dir name qnt))
  where
    (Product name price qnt) = menorDireita dir

menorDireita::DBTree->Product
menorDireita (Node Nil p _) = p
menorDireita (Node esq _ _) = menorDireita esq  

-- -- Ex: sellProduct arv "c" 5

a=Product "a" 1 1
b=Product "b" 2 4
c=Product "c" 3 9
d=Product "d" 4 10
e=Product "e" 5 7

tmp1=insertProduct Nil b
tmp2=insertProduct tmp1 a
tmp3=insertProduct tmp2 d
tmp4=insertProduct tmp3 c
arv=insertProduct tmp4 e