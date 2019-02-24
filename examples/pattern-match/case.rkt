(define-type MyType
    (inty Int)
    (vecy (Vector Bool Int)))

(define (print-bool [x : MyType]) : Bool
    (case x
        ((inty i) (zero? i))
        ((vecy v) (not (vector-ref v 0)))))

(let ([x (vecy (vector #f 3))])
    (print-bool x))
