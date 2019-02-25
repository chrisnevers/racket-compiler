(import "stdlib/list")

(define-syntax and (lambda ([stx])
    (if (second stx) (third stx) #f)))

(define-syntax or (lambda ([stx])
    (if (second stx) #t (third stx))))

