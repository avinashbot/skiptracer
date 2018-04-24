length' ls =
    case ls of
        []     -> 0
        (_:xs) -> 1 + length' xs

main = length' [1, 2, 3, 4]
