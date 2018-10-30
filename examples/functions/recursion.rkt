(define (get-ten [x : Int]) : Int
    (if (eq? x 10)
        x
        (get-ten (+ x 1))))

(begin
    (get-ten (- 10)))
