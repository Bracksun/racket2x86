#lang racket
(define (pe-neg r)
  (cond [(fixnum? r) (- 0 r)]
        [else `(- ,r)]))
(define (pe-add r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (+ r1 r2)]
        [else `(+ ,r1 ,r2)]))
(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,(app pe-arith r1))
      (pe-neg r1)]
    [`(+ ,(app pe-arith r1) ,(app pe-arith r2))
      (pe-add r1 r2)]))



;;;exp ::= (read) | (- (read)) | (+ exp exp)
;;;residual ::= int | (+ int exp) | exp
(define (pe-add-new r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (+ r1 r2)]
        [(fixnum? r1)
         (match r2
           [`(+ ,x1 ,x2) #:when (fixnum? x1)
              `(+ ,(+ r1 x1) ,x2)]
           [`(+ ,x1 ,x2) #:when (fixnum? x2)
              `(+ ,(+ r1 x2) ,x1)]
           [_ `(+ ,r1 ,r2)])]
        [(fixnum? r2) (pe-add-new r2 r1)]
        [else `(+ ,r1 ,r2)]))

;(define (pe-residual r)
;  (match r
;    [(? fixnum?) r]
;    [`(+ ,i ,(pe-arith-exp e))
;     (pe-add-new i e)]
;    [e (pe-arith-exp e)]))

(define (pe-arith-exp e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- (read)) `(- (read))]
    [`(+ ,(app pe-arith-exp r1) ,(app pe-arith-exp r2))
     (pe-add-new r1 r2)]))

(pe-arith-exp '(+ 1 (+ (read) 1)))