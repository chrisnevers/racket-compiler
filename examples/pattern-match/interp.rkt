(define-type RExp
    (RInt Int)
    (RNeg RExp))

(define (interp-exp [x : RExp]): Int
    (case x
        ((RInt i) i)
        ((RNeg e) (- (interp-exp e)))))

(interp-exp (RNeg (RNeg (RInt 5))))
