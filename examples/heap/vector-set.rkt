(let ((v (vector 4 #t 5)))
    (begin
        (vector-set! v 0 15)
        (vector-ref v 0)))