(define (change-mutable [x : (Ref Int)]) : Void (:= x 5))

(let ((x (box 5)))
    (begin
        (change-mutable x)
        (! x)))
