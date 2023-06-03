(define add [a b] (
    (define inner [a b] (
        (+ a b)
    ))
    (call inner [a b])
))

(print call $add [20 20])