(define foo [n] (
    (if (< n 2)
        n
        (+ (call foo [(- n 1)]) (call foo [(- n 2)])))
))

(print call $foo [32])