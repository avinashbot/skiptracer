map' f ls =
    case ls of
        []     -> []
        (x:xs) -> (f x) : (map' f xs)

foldr' f b ls =
    case ls of
        []     -> b
        (a:as) -> f a (foldr' f b as)

main = foldr' (+) 0 (map' (+ 1) [1, 2, 3])
