(define (xor [r : Int][l : Int]) : Bool (or (and (eq? l 0) (eq? r 1)) (and (eq? l 1) (eq? r 0)))) (xor 1 0)