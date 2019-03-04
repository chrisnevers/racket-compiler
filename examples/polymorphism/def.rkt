(define (id) : (Forall A (-> A A))
    (Lambda A (lambda ((y : A)): A y)))

;;; (inst id Bool)
id
