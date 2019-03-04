;;; id : (Forall A (A -> A))
;;; (inst id Bool) : Bool -> Bool

(let ((id (Lambda A
            (lambda ((x : A)) : A
                x))))
    (begin
        (print ((inst id Bool) #t))
        (print ((inst id Int) 5))
        (print ((inst id (Array Int)) (array 3)))
        ((inst id (Vector Int Int)) (vector 1 2))))

;;; ===>
;;; (let ([id (Lambda A (lambda ([x : A] : A x)))])
;;;     ((lambda ([x : Bool] : Bool x) x) #t))
