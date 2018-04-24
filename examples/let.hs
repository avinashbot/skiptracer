lessThan v x = x < v

partition' f ls =
    case ls of
        []     -> ([], [])
        (x:xs) ->
            let (ms, ns) = partition' f xs
            in  if f x then (x : ms, ns) else (ms, x : ns)

main =
    let (ms, _) = partition' (lessThan 3) [1, 4, 3, 2, 5]
    in  print ms
