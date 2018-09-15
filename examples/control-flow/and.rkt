(program
    (let ([a 5])
    (let ([b read])
        (if (and (eq? a 5) (eq? b 10))
            #t
            #f))))
