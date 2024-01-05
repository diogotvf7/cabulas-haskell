-- 1.
maxpos :: [Int] -> Int
maxpos list = foldr max 0 list

-- 2.
dups :: [a] -> [a]
dups [] = []
dups (x:y:xs) = (x : x : y : dups xs) 
dups (x:xs) = (x:x:xs)
-- dups (x:xs) = (x : x : skip xs)
    -- where
        -- skip [] = []
        -- skip (y:ys) = (y : dups ys)

-- 3.
transforma :: String -> String
transforma [] = []
transforma (x:xs)
    | elem x "aeiou"    = x:'p':x:transforma xs
    | otherwise         = x:transforma xs 

-- ---------------------
type Vector = [Int]
type Matriz = [[Int]]

-- 4.
transposta :: Matriz -> Matriz
