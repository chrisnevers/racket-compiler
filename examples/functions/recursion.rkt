(define (get-ten [x : Int]) : Int
    (if (eq? x 10)
        x
        (begin
            (print x)
            (get-ten (+ x 1)))))

(begin
    (get-ten (- 10))
    (print 10)
    (print get-ten))
