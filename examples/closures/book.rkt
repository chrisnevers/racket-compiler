(define (f [x : Int]) : (Int -> Int)
    (let ([y 4])
        (lambda ([z : Int]) : Int
            (+ x (+ y z)))))

(let ([g (f 5)])
(let ([h (f 3)])
    (+ (g 11) (h 15))))
