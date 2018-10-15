(define (map-vec    [f : (Int -> Int)]
                    [v : (Vector Int Int)])
        : (Vector Int Int)
    (vector (f (vector-ref v 0)) (f (vector-ref v 1))))

(define (add1 [x : Int]) : Int (+ x 1))

(vector-ref (map-vec add1 (vector 0 41)) 1)
