;;; Sample program
(define-type my-type
    [in-l Bool]
    [in-r Int])

(let ([x (in-l #t)])
    (case x
        [(in-l #f) 1]
        [(in-l _)  2]
        [(in-r n)  n]))


;;; Conversion

;;; Compiler will produce the following info:
;;; _ty_in_l:
;;;     .quad 8         ; user-type number
;;;     .asciz "in-l"   ; keep user type names at runtime
;;;     .quad 1         ; length
;;;     .quad _ty_bool  ; types ...

;;; _ty_in_r:
;;;     .quad 8         ; user-type number
;;;     .asciz "in-r"   ; keep user type names at runtime
;;;     .quad 1         ; length
;;;     .quad _ty_int   ; types ...

;;; _ty_my_type:
;;;     .quad 8         ; user-type number
;;;     .asciz "my-type"; keep user type names at runtime
;;;     .quad 2         ; length
;;;     .quad _ty_in_l  ; types ...
;;;     .quad _ty_in_r



;;; (let ([x (in-l #t)])
;;; =>
x = (vector type-ref(in-l) #t) ; (vector {ptr to type constructor tag} {value})


;;; (case x
;;;         [(in-l #f) 1]
;;;         [(in-l _)  2]
;;;         [(in-r n)  n])
;;; =>
let x-type = (vector-ref x 0) in
let case-type = type-ref(in-l) in
if (and
    (x_type == case_type)
    (let x_val = (vector-ref x 1) in x_val == #f)
) then 1
else
    let case-type = type-ref(in-l)
    ; compiler will skip any '_' checks
    if (x_type == case_type) then 2
else
    let case-type = type-ref(in-r)
    if (x_type == case_type) then
        let n = (vector-ref n 1) in n
    else
        throw match_error
