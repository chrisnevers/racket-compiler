(let ([a read])
(let ([b (if (> a 10)
        10
        (- 10))])
    (+ a b)))
