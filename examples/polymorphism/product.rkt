(define-type Product A B
    [Inl A]
    [Inr B])

((inst (inst Inl Int) Bool) 5)
