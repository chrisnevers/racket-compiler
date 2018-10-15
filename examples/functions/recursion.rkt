(define (get-ten [x : Int]) : Int
    (if (eq? x 10)
        x
        (begin
            (print x)
            (get-ten (+ x 1)))))

(get-ten (- 10))
