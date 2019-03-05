(import "stdlib/list")

;;; Short circuit logical and
(define-syntax and
  (syntax-case ()
    [(_) #t]
    [(_ x) x]
    [(_ x y) (if x y #f)]
    [(_ x y z ...) (if x (and y z ...) #f)]))

;;; Short circuit logical or
(define-syntax or
  (syntax-case ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2) (if e1 #t e2)]
    [(_ e1 e2 e3 ...) (let ((t e1)) (if t t (or e2 e3 ...)))]))

(define-syntax xor
  (syntax-case ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2) (if e1 (not e2) e2)]))

(define-syntax when
  (syntax-case ()
    [(_ e0 e1 e2 ...) (if e0 (begin e1 e2 ...))]))

(define-syntax unless
  (syntax-case ()
    [(_ e0 e1 e2 ...) (when (not e0) e1 e2 ...)]))

;;; Sugar for nested if-else expressions
(define-syntax cond
  (syntax-case (else =>)
    [(_ (else e1 ...)) (begin e1 ...)]
    [(_ (e0))
      (let ((t e0)) (if t t #f))]
    [(_ (e0) c1 ...)
      (let ((t e0)) (if t t (cond c1 ...)))]
    [(_ (e0 => e1))
      (let ((t e0)) (if t (e1 t) #f))]
    [(_ (e0 => e1) c1 ...)
      (let ((t e0)) (if t (e1 t) (cond c1 ...)))]
    [(_ (e0 e1 ...))
      (if e0 (begin e1 ...))]
    [(_ (e0 e1 ...) c1 ...)
      (if e0 (begin e1 ...) (cond c1 ...))]))

;;; Access value inl of sum type
(define-syntax get-left
  (syntax-case () [
      (_ x) (vector-ref x 1)]))

;;; Access value inr of sum type
(define-syntax get-right
  (syntax-case ()
    [(_ x) (vector-ref x 2)]))

;;; Depending on the side of a sum type
;;; bind the given variable and execute
;;; the correct body. This infers
;;; the branch position inl, inr.
;;; Different from 'case' (hardcoded in compiler),
;;; which checks the type constructors when
;;; evaluating.
(define-syntax split
    (syntax-case ()
        [(split x (id1 b1) (id2 b2))
            (if (inl? x)
                (let ((id1 (get-left x))) b1)
                (let ((id2 (get-right x))) b2))]))

;;; Syntactic sugar for creating, setting,
;;; and accessing refs, or boxes
(define-syntax box
  (syntax-case ()
    [(_ i) (vector i)]))

(define-syntax !
  (syntax-case ()
    [(_ i) (vector-ref i 0)]))

(define-syntax :=
  (syntax-case ()
    [(_ i e) (vector-set! i 0 e)]))

(define-syntax incr
  (syntax-case ()
    [(_ x) (:= x (+ (! x) 1))]))

(define-syntax add!
  (syntax-case ()
    [(_ x n) (:= x (+ (! x) n))]))

;;; Evaluate if condition on integer
;;; The result of the condition is stored in 'it'
;;; 0 => false , _ => true
(define-syntax aif
  (syntax-case ()
    [(aif chk thn els)
     (let ((it chk)) (if (zero? it) els thn))]))

(define-syntax for
  (syntax-case ()
    [(_ (b1 cnd e1) e2)
      (let ((_ b1))
        (while cnd
          (begin e2 e1)))]))

(define-syntax for-let
  (syntax-case ()
    [(_ ((:= id val) cnd e1) e2)
      (let ((id (box val)))
      (let ((_ (:= id val)))
        (while cnd
          (begin e2 e1))))]))
