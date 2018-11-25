(define-type NewType
    [Neg Int]
    [Add (Vector Int Int)])

(let ([x (Add (vector 5 (Neg 4)))])
    (print x))
