length :: [a] -> Int

reverse :: [a] -> [a]

(++) :: [a] -> [a] -> [a]

zip :: [a] -> [b] -> [(a,b)]

drop :: Int -> [a] -> [a]

null :: [a] -> Bool

head :: [a] -> a

take :: Int -> [a] -> [a]

fst :: (a,b) -> a

zip :: [a] -> [b] -> [(a,b)]

(+) :: Num a => a -> a -> a

mod :: Integral a => a -> a -> a

(/) :: Fractional a => a -> a -> a

(==) :: Eq a => a -> a -> Bool

(<) :: Ord a => a -> a -> Bool

max :: Ord a => a -> a -> a

not :: Bool -> Bool

isUpper :: Char -> Bool

isLower :: Char -> Bool

isLetter :: Char -> Bool

toUpper :: Char -> Char

toLower :: Char -> Char

map :: (a -> b) -> [a] -> [b]

filter :: (a -> Bool) -> [a] -> [a]

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

all, any :: (a -> Bool) -> [a] -> Bool

foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (a -> b -> a) -> a -> [b] -> a

    sum = foldr (+) 0
    
    product = foldr (*) 1
    
    and = foldr (&&) True
    
    or = foldr (||) False
    
    length = foldr (\_ n->n+1) 0

(.) :: (b -> c) -> (a -> b) -> a -> c

repeat :: a -> [a]

cycle :: [a] -> [a]

iterate :: (a -> a) -> a -> [a]


-- --------------------------------------------------- qsort
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort maiores
    where 
        menores = [y | y<-xs, y<=x]
        maiores = [y | y<-xs, y>x]

-- --------------------------------------------------- init
> init [1,2,3,4,5]
[1,2,3,4]

-- --------------------------------------------------- compreensao
> [(x,y) | x<-[1,2,3], y<-[4,5]]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- --------------------------------------------------- concat
> concat [[1,2,3],[4,5],[6,7]]
[1,2,3,4,5,6,7]

-- --------------------------------------------------- fibonacci
fibs :: [Integer]
fibs = 0 : 1 : [a+b | (a,b)<-zip fibs (tail fibs)]

-- --------------------------------------------------- crivo de Eratóstenes
primos :: [Integer]
primos = crivo [2..]
crivo :: [Integer] -> [Integer]
crivo (p:xs) = p : crivo [x | x<-xs, x‘mod‘p/=0]