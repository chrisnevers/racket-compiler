(define-type List A
    [Nil A]
    [Cons (Vector A List)])

((inst Cons Int) (vector 6 ((inst Nil Int) 5)))
