fib n =
    case n of
        0 -> 0
        1 -> 1
        _ -> fib (n - 1) + fib (n - 2)

main = fib 10
