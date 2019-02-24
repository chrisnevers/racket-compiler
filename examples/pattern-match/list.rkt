(define-type List
    (Nil  Bool)
    (Cons (Vector Int List)))

(Cons (vector 4 (Cons (vector 5 (Cons (vector 6 (Nil #f)))))))
;;; (print (Nil #f))
