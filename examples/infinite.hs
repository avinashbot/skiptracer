map' f ls =
    case ls of
        []     -> []
        (x:xs) -> f x : map' f xs

take' n ls =
    if   n <= 0
    then []
    else
        case ls of
            []     -> []
            (x:xs) -> x : take' (n - 1) xs

numbers = 1 : map' (+1) numbers

main = print (take' 10 numbers)
