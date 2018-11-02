(define (set-index  [a : (Array (Array Int))]
                    [i : Int])
        : Void
        (array-set! a i (array 42)))

(let ([my-array (array (array 1 2 3) (array 4 5 6))])
(let ([_ (set-index my-array 1)])
    my-array))
