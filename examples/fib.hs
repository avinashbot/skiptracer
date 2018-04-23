fib n =
    case n of
        0 -> 0
        1 -> 1
        _ | n > 0 -> fib (n - 1) + fib (n - 2)

main = fib 10
