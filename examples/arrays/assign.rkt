(let ([booleans (array #t #f)])
(let ([copy-it booleans])
(begin
    (array-set! copy-it 0 #f)
    (print booleans))))
