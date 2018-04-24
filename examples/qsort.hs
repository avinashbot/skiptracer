import           Prelude ()

(++) l r =
    case l of
        []     -> r
        (x:xs) -> x : (xs ++ r)

partition' f ls =
    case ls of
        []     -> ([], [])
        (x:xs) ->
            let (ms, ns) = partition' f xs
            in  if f x then (x : ms, ns) else (ms, x : ns)

qsort ls =
    case ls of
        []     -> []
        (x:xs) -> let (smaller, larger) = partition' (<= x) xs
                  in qsort smaller ++ [x] ++ qsort larger

main = print (qsort [3, 5, 1, 4, 2])
