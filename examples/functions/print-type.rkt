(define (vedy-interesting-type  [f : (Int -> Bool)]
                                [v : (Vector Int Bool)]) : Void
        (vector-set! v 0 5))

(define (int-to-bool [x : Int]) : Bool
    (if (zero? x)
        #f
        #t))

(print vedy-interesting-type)
