map f ls =
    case ls of
        []     -> []
        (x:xs) -> (f x) : (map f xs)

main = map (\n -> n + 1) [1, 2, 3]
