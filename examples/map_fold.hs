map' f ls =
    case ls of
        []     -> []
        (x:xs) -> (f x) : (map' f xs)

foldr' f b ls =
    case ls of
        []     -> b
        (a:as) -> f a (foldr' f b as)

main = foldr' (\a b -> a + b) 0 (map' (\n -> n + 1) [1, 2, 3])
