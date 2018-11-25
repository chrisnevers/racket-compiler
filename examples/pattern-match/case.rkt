;;;
;;; First Way
;;;

(define-type MyType
    [inty Int]
    [booly Bool])

(define (print-bool [x : MyType]) : Bool
    (case x
        [(inty i) (zero? i)]
        [(booly b) (not b)]))

(booly #t)
;;; (let ([x (inty 3)])
;;; (let ([y (booly #t)])
;;; (begin
;;;     (print-bool x)
;;;     (print-bool y))))
