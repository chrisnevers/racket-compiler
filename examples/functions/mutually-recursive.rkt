(define (is5 [x : Int]) : Int
    (if (eq? x 5)
        5
        (is7 (- x 1))))

(define (is7 [x : Int]) : Int
    (if (eq? x 7)
        7
        (is5 (- x 1))))

(is7 10)
