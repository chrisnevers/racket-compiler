(define-type List A
    [Nil A]
    [Cons (Vector A List)])

(let ([bool ((inst Nil Bool) #t)])
(let ([int  ((inst Nil Int) 5)])
    int))
