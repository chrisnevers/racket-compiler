(let ([a 5])
(let ([b 7])
(let ([c read])
    (if (and (eq? a 5) (and (eq? b 7 ) (eq? c 10)))
        #t
        #f))))
