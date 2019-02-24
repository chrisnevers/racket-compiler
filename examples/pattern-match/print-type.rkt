(define-type ascii
    (AsInt Int)
    (AsChar Char))

(define (print-this-ish [x : ascii]) : Void
    (case x
        ((AsInt i) (print i))
        ((AsChar c)   (print c))))

print-this-ish
