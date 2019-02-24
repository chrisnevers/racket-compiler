(define-syntax and (lambda (stx)
    (if (second stx) (third stx) #f)))

(define-syntax or (lambda (stx)
    (if (second stx) #t (third stx))))

(define-type List A
    (Nil)
    (Cons (Vector A List)))
