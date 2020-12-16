-- Exercicio 1
-- any odd [1..10] == True
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

-- zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ _ _ = []

-- takeWhile odd [1,3,4,5,6,6] == [1,3]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = []
-- dropWhile odd [1,3,4,5,6,6] == [4,5,6,6]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = t
-- span p l = (takeWhile p l, dropWhile p l)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) | f h = (h:s1,s2)
              | otherwise = ([],h:t)
    where (s1,s2) = span' f t

-- deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)] == [(3,3),(4,2)]
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f x (h:t) | f x h = t
                   | otherwise = h : deleteBy f x t

-- sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]
sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insere (h) (sortOn f t)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a then a:insere x b else x:a:b

-- Exercicio 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n ps = filter (\x -> snd x == n) ps

conta :: Int -> Polinomio -> Int
conta n p = length $ filter (\x -> n == snd x) p

grau :: Polinomio -> Int
grau ps = foldl (\acc x -> if acc > snd x then acc else snd x) 0 ps

deriv :: Polinomio -> Polinomio
deriv ps = filter (/= (0,0)) $ map (\(b,e) -> if e > 0 then (b * fromIntegral e, e - 1) else (0,0)) ps

calcula :: Float -> Polinomio -> Float
calcula a = foldl (\acc (b,e) -> acc + b * (a ^ e)) 0

simp :: Polinomio -> Polinomio
simp = filter (\(b,e) -> e /= 0)

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) = map (\(b,e) -> (b*x,y+e))

ordena :: Polinomio -> Polinomio
ordena = sortOn (snd)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]

soma :: Polinomio -> Polinomio -> Polinomio
soma p r = normaliza $ (++) p r

produto :: P
produto p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1

-- Exercício 3
type Mat a = [[a]]

dimOK :: Mat a -> Bool
--que testa se uma matriz est ́a bem constru ́ıda (i.e., setodas as linhas tˆem a mesma dimens ̃ao).(b)
dimMat :: Mat a -> (Int,Int)
--que calcula a dimens ̃ao de uma matriz.(c)
addMat :: Num a => Mat a -> Mat a -> Mat a
--que adiciona duas matrizes.(d)
transpose :: Mat a -> Mat a
--que calcula a transposta de uma matriz.(e)
multMat :: Num a => Mat a -> Mat a -> Mat a
--que calcula o produto de duasmatrizes.(f)
zipWMat :: (a -> b -> c) ->  Mat a -> Mat b -> Mat c
--que, `a semelhan ̧cado que acontece com a fun ̧c ̃aozipWith, combina duas matrizes.  Use essa fun ̧c ̃aopara definir uma fun ̧c ̃ao que adiciona duas matrizes.(g)
triSup :: Num a => Mat a -> Bool
--que testa se uma matriz quadrada  ́e trian-gular superior (i.e., todos os elementos abaixo da diagonal s ̃ao nulos).(h)
rotateLeft :: Mat a -> Mat a
--que roda uma matriz 90opara a esquerda. 
