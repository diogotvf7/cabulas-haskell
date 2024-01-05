type Species = (String, Int)
type Zoo = [Species]

-- ("T-rex", 12)
-- ("Cao", 12300)
-- ("Tartaruga", 1200)
-- ("Cabra", 3200)
-- ("Camelo", 635)
-- [("T-rex", 12), ("Cao", 12300), ("Tartaruga", 1200), ("Cabra", 3200), ("Camelo", 635)]

-- 1.
isEndangered :: Species -> Bool
isEndangered (name, population)
    | population <= 100 = True
    | otherwise = False

-- 2.
updateSpecies :: Species -> Int -> Species
updateSpecies (name, population) n = (name, population + n)

-- 3.
filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] f = []
filterSpecies (x:xs) f
    | f x       = x : filterSpecies xs f
    | otherwise = filterSpecies xs f

-- 4.
countAnimals :: Zoo -> Int
countAnimals z = sum (map snd z)

-- 5.
substring :: (Integral a) => String -> a -> a -> String
substring s start end =
    let 
        start' = fromIntegral start
        length = fromIntegral end - fromIntegral start + 1
    in take length (drop start' s) 

-- 6.
hasSubstr :: String -> String -> Bool
hasSubstr s1 s2 = hasSubstrAux s1 s2 s2

hasSubstrAux :: String -> String -> String -> Bool 
hasSubstrAux _ "" _ = True 
hasSubstrAux (a:xs1) (b:xs2) s
    | a == b    = hasSubstrAux xs1 xs2 s
    | otherwise = hasSubstrAux xs1 s s 
hasSubstrAux "" _ _ = False

--7.
sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr zoo str = sortSpeciesWithSubstrAux zoo str [] []

sortSpeciesWithSubstrAux :: Zoo -> String -> Zoo -> Zoo -> (Zoo, Zoo)
sortSpeciesWithSubstrAux [] _ l1 l2 = (l1, l2)
sortSpeciesWithSubstrAux (s:zoo) str l1 l2
    | hasSubstr (fst s) str = sortSpeciesWithSubstrAux zoo str (s:l1) l2
    | otherwise             = sortSpeciesWithSubstrAux zoo str l1 (s:l2)

-- 8.
rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : [ a + b | (a, b) <- zip rabbits (tail rabbits) ]

-- 9.
rabbitYears :: (Integral a) => a -> Int
rabbitYears nRabbits = length (takeWhile (<= nRabbits) rabbits)

-- ----------------

data Dendrogram = 
    Leaf String 
    | Node Dendrogram Int Dendrogram

myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

-- 10.
dendroWidth :: Dendrogram -> Int
dendroWidth dendrogram = left dendrogram + right dendrogram 

left (Leaf str) = 0
left (Node lNode x _) = x + left lNode

right (Leaf str) = 0
right (Node _ x rNode) = x + right rNode

-- 11.
dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds (Node left x right) lMax = 
    dendroVisiter left x lMax "left" ++ dendroVisiter right x lMax "right"

dendroVisiter :: Dendrogram -> Int -> Int -> String -> [String]
dendroVisiter (Leaf str) l lMax _ = [str]
dendroVisiter (Node left x right) l lMax "left" 
    | l + x <= lMax     = dendroVisiter left (l+x) lMax "left" ++ dendroVisiter right (l-x) lMax "right"
    | otherwise         = dendroVisiter right (l-x) lMax "right"
dendroVisiter (Node left x right) l lMax "right"
    | l + x <= lMax     = dendroVisiter left (l+x) lMax "right" ++ dendroVisiter right (l-x) lMax "left"
    | otherwise         = dendroVisiter right (l-x) lMax "left"

