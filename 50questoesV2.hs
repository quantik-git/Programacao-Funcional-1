import Data.Char
import Data.Either

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end | start < end  = start : enumFromTo' (start + 1) end
                      | start == end = [end]

enumFromThenTo' :: Int -> Int-> Int -> [Int]
enumFromThenTo' start nextStep end | start < end   = start : enumFromThenTo' nextStep pattern end
                                   | start == end  = [end]
                                   | pattern > end = []
                                    where pattern = nextStep * 2 - start

(!++) :: [a] -> [a] -> [a]
(!++) (x:[]) lista2 = x : lista2
(!++) (x:xs) lista2 = x : (!++) xs lista2

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) index = (!!!) xs (index - 1)

reverse' :: [a] -> [a]
reverse' lista = case lista of
    []     -> []
    (x:[]) -> [x]
    (x:xs) -> reverse' xs !++ [x]

take' :: Int -> [a] -> [a]
take' 1 (x:xs)          = [x]
take' quantidade (x:[]) = [x]
take' quantidade (x:xs) = x : take' (quantidade - 1) xs

drop' :: Int -> [a] -> [a]
drop' 0 lista = lista
drop' n (x:xs) = drop' (n - 1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:[]) (y:ys) = [(x, y)]
zip' (x:xs) (y:[]) = [(x, y)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = if n == x
    then True
    else elem' n xs

replicate' :: Int -> a -> [a]
replicate' 1 elemento = [elemento]
replicate' n elemento = elemento : replicate' (n - 1) elemento

intersperce' :: a -> [a] -> [a]
intersperce' n (x:[]) = [x]
intersperce' n (x:xs) = x : n : intersperce' n xs

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:xs) = agrupar [x] x xs
    where
        agrupar lista elemento [] = [lista]
        agrupar lista elemento (h:t)
            | elemento == h = agrupar (lista !++ [h]) h t
            | otherwise     = lista : agrupar [h] h t

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x !++ concat' xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : prefixos [x] xs
    where
        prefixos lista [] = [lista]
        prefixos lista (h:t) = lista : prefixos (lista ++ [h]) t

inits' :: [a] -> [[a]]
inits' [] = []
inits' lista = (init lista) : inits' (init lista) 

tails :: [a] -> [[a]]
tails [] = [[]]
tails tudo@(x:xs) = tudo : sufixos x xs
    where
        sufixos lista [] = []
        sufixos lista (h:t) = t : sufixos h t

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = error "O prefixo a ser testado excede a lista"
isPrefixOf (x:xs) (y:ys) = if x == y then isPrefixOf xs ys else False

isSuffixOf :: Eq a => [a]-> [a] -> Bool
isSuffixOf _ [] = error "O sufixo a ser testado excede a lista"
isSuffixOf lista1 lista2@(y:ys) = if length lista1 == length lista2
    then isPrefixOf lista1 lista2
    else isSuffixOf lista1 ys

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf subLista@(x:xs) (y:ys) = if x == y
    then isSubsequenceOf xs ys
    else isSubsequenceOf subLista ys

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices elemento lista = contador 0 elemento lista
    where
        contador _ _ [] = []
        contador indice elemento (x:xs) = if elemento == x
            then indice : contador (indice + 1) elemento xs
            else contador (indice + 1) elemento xs

nub ::  Eq a => [a] -> [a]
nub lista@(x:xs) = reverse' (mem [x] lista)
    where
        mem lista [] = lista
        mem lista (y:ys) = if checkDif lista y
            then mem (y : lista) ys
            else mem lista ys
        checkDif (x:[]) elemento = if x == elemento
            then False
            else True
        checkDif (x:xs) elemento = if x == elemento
            then False
            else checkDif xs elemento

delete :: Eq a => a -> [a] -> [a]
delete elemento (x:xs) = if elemento == x
    then xs
    else x : delete elemento xs

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) lista [] = lista
(\\) lista ocorrencias@(y:ys) = (\\) (delete y lista) (delete y ocorrencias)

union :: Eq a => [a] -> [a] -> [a]
union lista [] = lista
union lista (x:xs) = if elem x lista
    then union lista xs
    else union lista xs ++ [x]

intersect :: Eq a => [a] -> [a] -> [a]
intersect lista [] = []
intersect lista@(x:xs) (y:ys) = (filtrar lista y) ++ intersect lista ys
    where
        filtrar [] elemento = []
        filtrar (x:xs) elemento = if elemento == x
             then x : filtrar xs elemento
             else filtrar xs elemento

insert :: Ord a => a -> [a]-> [a]
insert elemento lista@(f:s:t) = if elemento < f
    then elemento : lista
    else if elemento > f && elemento < s
        then f : elemento : s : t
        else f : s : insert elemento t

unwords' :: [String] -> String
unwords' [] = []
unwords' (x:xs) = x ++ " " ++ unwords' xs

unlines' :: [String] -> String
unlines' (x:[]) = x ++ "\n"
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

pMaior ::  Ord a => [a] -> Int
pMaior (x:xs) = maior xs 1 x 0
    where
        maior [] _ _ indiceMaior = indiceMaior
        maior (x:xs) indice valor indiceMaior = if x > valor
            then maior xs (indice + 1) x indice
            else maior xs (indice + 1) valor indiceMaior

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [x] = False
temRepetidos (x:xs) = if elem x xs
    then True
    else temRepetidos xs

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) = if elem (ord x) [48..57]
    then x : algarismos xs
    else algarismos xs

posImpares :: [a] -> [a]
posImpares lista = posImpar lista 0
    where
        posImpar [] _ = []
        posImpar (x:xs) flag = if flag == 0
            then posImpar xs 1
            else x : posImpar xs 0

posPares ::  [a] -> [a]
posPares lista = posImpar lista 1
    where
        posImpar [] _ = []
        posImpar (x:xs) flag = if flag == 0
            then posImpar xs 1
            else x : posImpar xs 0

isSorted :: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (x:y:xs) = if x <= y
    then isSorted (y:xs)
    else False

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) | x < y     = x:y:ys
                 | otherwise = y:(insert x ys)

iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (x:xs) = insert' x (iSort' xs)

menor ::  String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) = menor xs ys

elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet elemento [] = False
elemMSet elemento (x:xs) = if elemento == fst x then True else elemMSet elemento xs

lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (x:xs) = 2 + lengthMSet xs

converteMSet ::  [(a,Int)] -> [a]
converteMSet [] = []
converteMSet (x:xs) = transformar x ++ converteMSet xs
  where transformar (a,0) = []
        transformar (a,b) = a : transformar (a,b-1)

insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet elemento (x:xs) = if elemento == fst x 
then somar x : xs else x : insereMSet elemento xs
  where somar (a,b) = (a,b+1)
  
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet elemento (x:xs) = if elemento == fst x 
then xs else x : removeMSet elemento xs

constroiMSet ::  Ord a => [a] -> [(a,Int)]
constroiMSet (x:y:xs) = constroi x (y:xs) 1
  where
    constroi x (y:[]) acc = if x == y then (x, (acc+1)) : []  else (x,acc) : (y,1) : []
    constroi x (y:xs) acc = if x == y then constroi y xs (acc+1)  else (x,acc) : constroi y xs 1

-- Lista para testes
listaEithers = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]

partitionEithers' ::  [Either a b] -> ([a],[b])
partitionEithers' lista = (as lista, bs lista)
  where
    as ((Left x):xs)  = x : as xs
    as ((Right x):xs) = as xs
    as otherwise      = []
    bs ((Right x):xs) = x : bs xs
    bs ((Left x):xs)  = bs xs
    bs otherwise      = []

-- Auxiliares
isLeft' (Left _) = True
isLeft' _ = False
getLeft' (Left x) = x
-- Implementações alternativas
filterLeft1 lista = [x | (Left x) <- lista]
filterLeft0 lista = foldr (\x acc -> if (isLeft' x) then (getLeft' x):acc else acc) [] lista
filterLeft2 lista = map getLeft' (filter isLeft' lista)

-- Lista para testes
a = [(1,'A'),(2,'B'),(3,'C')]

catMaybes ::  [Maybe a] -> [a]
catMaybes []            = []
catMaybes ((Just a):xs) = a : catMaybes xs
catMaybes (Nothing:xs)  = catMaybes xs

-- List comprehension implementation
catMaybes' lista = [a | (Just a) <- lista]


data Movimento = Norte | Sul | Este | Oeste deriving Show

getMov ::  Movimento -> String
getMov Norte = "Norte"
getMov Sul   = "Sul"
getMov Este  = "Este"
getMov Oeste = "Oeste"

posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (m:ms) | (getMov m) == "Norte" = posicao (x,y+1) ms
                     | (getMov m) == "Sul"   = posicao (x,y-1) ms
                     | (getMov m) == "Este"  = posicao (x+1,y) ms
                     | (getMov m) == "Oeste" = posicao (x-1,y) ms

caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xAtual, yAtual) (xAlvo, yAlvo) = vertical xAtual xAlvo ++ horizontal yAtual yAlvo
  where
    vertical x1 x2 | x1 > x2  = Oeste : vertical (x1-1) x2
                   | x1 < x2  = Este : vertical (x1+1) x2
                   | x1 == x2 = []
    horizontal y1 y2 | y1 > y2  = Sul : horizontal (y1-1) y2
                     | y1 < y2  = Norte : horizontal (y1+1) y2
                     | y1 == y2 = []

vertical ::  [Movimento] -> Bool
vertical [x] = if (getMov x) == "Norte" || (getMov x) == "Sul" 
  then True
  else False
vertical (x:xs) = if (getMov x) == "Norte" || (getMov x) == "Sul" 
  then vertical xs
  else False

data Posicao = Pos Int Int deriving Show

maisCentral ::  [Posicao] -> Posicao
maisCentral (x:xs) = centro xs x
  where
    centro [] solucao = solucao
    centro ((Pos a b):ys) solucao@(Pos c d) = if (a + b) <= (c + d)
      then centro ys (Pos a b)
      else centro ys solucao

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos a b) list = [(Pos c d) | (Pos c d) <- list, c >= (a-1) && c <= (a+1) && d >= (b-1) && d <= (b+1)]

vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' _ [] = []
vizinhos' (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
  then (Pos xv yv):vizinhos' (Pos x y) ps 
  else vizinhos' (Pos x y) ps

mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada ((Pos x a):[]) = True
mesmaOrdenada ((Pos x a):(Pos y b):xs) = if a == b
  then mesmaOrdenada ((Pos y b):xs)
  else False

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK lista = length [(Vermelho) | (Vermelho) <- lista] == 2
-- End
