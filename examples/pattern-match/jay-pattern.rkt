;;; Language:
M := .... | (inl M T) | (inr T M) | (case M [X : T => M] [X : T => M])
V := .... | (inl V T) | (inr T V)
E := .... | (inl E T) | (inr T E) | (case E [X : T => M] [X : T => M])
T := .... | T + T

;;; Dynamic Semantics:
E[(case (inl V1 T1)
    [X1 : T1 => M1]
    [X2 : T2 => M2])]
    -> E[M1[X1 <- V1]]

E[(case (inr T1 V2)
    [X1 : T1 => M1]
    [X2 : T2 => M2])]
    -> E[M2[X2 <- V2]]

;;; Static Semantics:
;;; tyinl :
    (has-type G M T1) => (has-type G (inl M T2) (+ T1 T2))
;;; tyinr :
    (has-type G M T2) => (has-type G (inr T1 M) (+ T1 T2))
;;; tycase :
    (has-type G M0 (+ T1 T2)
    (has-type G[X1 : T1] M1 T3)
    (has-type G[X2 : T2] M2 T3) =>
    (has-type G (case M0 [X1 : T1 => M1] [X2 : T2 => M2]) T3)

;;; You would use syntactic sugar to turn something like

(define-type mytype [truey Bool] [inty Int])
(case x
 [(truey b) (not b)
 [(inty i) (zero? i)]])

;;; into

(define mytype (+ Bool Int))
(define (truey [x : Bool]) (inl x Int))
(define (inty [x : Int]) (inr Bool x))
(case x [b : Bool => (not b) [i : Int => (zero? i)]])