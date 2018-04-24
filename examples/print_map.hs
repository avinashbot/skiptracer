map' f ls =
    case ls of
        []     -> []
        (x:xs) -> f x : map' f xs

main = print (map' (* 2) [1, 2, 3])
