#lang racket/base
(require racket/match
         racket/math)

;;; OO system where object takes lambda of
;;; method you want to call
;;; (let ([private-fields ...])
;;; (lambda (method-call) (case method-call [method1 => behavior] ...)))


(let ()
  (struct get-x ())
  (struct get-y ())
  (struct distance-from-origin ())
  (struct set-x (nx))
  (struct set-y (ny))
  (define (make-posn-object x y)
    (λ (method)
      (match method
        [(get-x) x]
        [(get-y) y]
        [(set-x nx) (set! x nx)]
        [(set-y ny) (set! y ny)]
        [(distance-from-origin)
         (sqrt (+ (sqr x) (sqr y)))])))

  (define o (make-posn-object 3 4))
  (vector (o (get-x))
          (o (get-y))
          (o (distance-from-origin))
          (o (set-x 10))
          (o (distance-from-origin))))

(let ()
  (define-syntax-rule
    (define-class (class-name init-field ...)
      [(method method-args ...) method-body] ...)
    (begin
      (struct method (method-args ...)) ...
      (define (class-name init-field ...)
        (λ (call)
          (match call
            [(method method-args ...) method-body] ...)))))

  (define-class (make-posn-object x y)
    [(get-x) x]
    [(get-y) y]
    [(set-x nx) (set! x nx)]
    [(set-y ny) (set! y ny)]
    [(distance-from-origin) (sqrt (+ (sqr x) (sqr y)))])

  (define o (make-posn-object 3 4))
  (vector (o (get-x))
          (o (get-y))
          (o (distance-from-origin))
          (o (set-x 10))
          (o (distance-from-origin))))