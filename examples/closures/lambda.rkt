; (let ([y 10]) (let ([foo (lambda ([x : Int]) : Int (+ x y))]) (foo 10)))
(let ([y 10]) ((lambda ([x : Int]) : Int (+ x y)) 10))
