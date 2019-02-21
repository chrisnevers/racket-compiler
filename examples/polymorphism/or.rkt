(let ([generic-or (Lambda A
            (lambda ([x : A]) : (-> A A)
                (lambda ([y : A]) : A
                    (or x y))))])
(let ([rhs ((inst generic-or Bool) #f)])
    (rhs #f)))
