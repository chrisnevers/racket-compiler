(let id : (Forall A (-> A A)) := (Lambda A (lambda ([x : A]) : A x)) in
  (if ((inst id Bool) true)
      (vector-ref ((inst id (Vec Int Int)) (vector 5 6)) 0)
      42))

===>

(let id : (Forall A (-> A A)) := (Lambda A (lambda ([x : A]) : A x)) in
  (if ((lambda ([x : Bool]) : Bool x) true)
      (vector-ref ((lambda ([x : (Vec Int Int)]) : (Vec Int Int) x) (vector 5 6)) 0)
      42))

(inst M T) ;; Not!

(inst (or X (Lambda A M) T))

;;; x is variable
