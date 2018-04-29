import           Prelude ()

(++) l r =
    case l of
        []     -> r
        (x:xs) -> x : (xs ++ r)

filter f ls =
    case ls of
        []     -> []
        (x:xs) -> let rest = filter f xs in if f x then x : rest else rest

qsort ls =
    case ls of
        []     -> []
        (x:xs) -> let smaller = filter (<= x) xs
                      larger  = filter (> x)  xs
                  in qsort smaller ++ [x] ++ qsort larger

main = print (qsort [3, 5, 1, 4, 2])
