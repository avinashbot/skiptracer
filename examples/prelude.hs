map f ls =
    case ls of
        []     -> []
        (x:xs) -> f x : map f xs

foldr f b ls =
    case ls of
        []     -> b
        (a:as) -> f a (foldr f b as)

take n ls =
    if   n <= 0
    then []
    else
        case ls of
            []     -> []
            (x:xs) -> x : take (n - 1) xs

length ls =
    case ls of
        []     -> 0
        (_:xs) -> 1 + length xs

partition f ls =
    case ls of
        []     -> ([], [])
        (x:xs) ->
            let (ms, ns) = partition f xs
            in  if f x then (x : ms, ns) else (ms, x : ns)
