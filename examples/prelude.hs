otherwise :: Bool
otherwise = True

map :: (a -> b) -> [a] -> [b]
map f ls =
    case ls of
        []     -> []
        (x:xs) -> f x : map f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b ls =
    case ls of
        []     -> b
        (a:as) -> f a (foldr f b as)

take :: Int -> [a] -> [a]
take n ls =
    case ls of
        _      | n <= 0 -> []
        []     -> []
        (x:xs) -> x : take (n - 1) xs

length :: [a] -> Int
length ls =
    case ls of
        []     -> 0
        (_:xs) -> 1 + length xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f ls =
    case ls of
        []     -> ([], [])
        (x:xs) ->
            let (ms, ns) = partition f xs
            in  if f x then (x : ms, ns) else (ms, x : ns)
