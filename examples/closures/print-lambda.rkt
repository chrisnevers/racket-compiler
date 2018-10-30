(let ([f (lambda ([x : (Vector Int Int)]): Bool
        (if (not (zero? (vector-ref x 1)))
            #t
            #f))])
    (print f))
