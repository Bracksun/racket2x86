#lang racket
;;;exp ::= int | (read) | (- exp) | (+ exp exp)
;;;      | var | (let ([var exp]) exp)
;;;R1 ::= (program exp)

(define (interp-R1 env)
  (lambda (e)
    (define recur (interp-R1 env))
    (match e
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,(app recur v)]) ,body)
        (define new-env (cons (cons x v) env))
        ((interp-R1 new-env) body)]
      [(? fixnum?) e]
      [`(read)
        (define r (read))
        (cond [(fixnum? r) r]
              [else (error 'interp-R1 "expected an integer" r)])]
      [`(- ,(app recur v))
        (- 0 v)]
      [`(+ ,(app recur v1) ,(app recur v2))
        (+ v1 v2)]
      [`(program ,e) ((interp-R1 '()) e)]
      )))

(define (lookup exp env)
  (cond
    [(null? env) (error exp "is not defined")]
    [(eq? exp (caar env)) (cdar env)]
    [else (lookup exp (cdr env))]
    ))

;;; A subset of the x86 assembly language
;;;reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi |
;;;        r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
;;;arg ::= $int | %reg | int(%reg)
;;;instr ::= addq arg, arg | subq arg, arg | negq arg | movq arg, arg |
;;;          callq label | pushq arg | popq arg | retq
;;;prog ::= .globl main
;;;         main: instr+

;;; $n :: n is integer
;;; %rax :: rax is a register
;;; n(%r) :: register r and then offsets the address by n bytes
;;; addq s, d

;;; Memory layout of a frame
;;;   Position   |  Contents
;;;    8(%rbp)   |  return address
;;;    0(%rbp)   |  old rbp
;;;   -8(%rbp)   |  variable 1
;;;   -16(%rbp)  |  variable 2
;;;     ...      |  ...
;;;    0(%rsp)   |  variable n

;;; Abstract syntax for x86 assembly
;;; arg ::= (int int) | (reg register) | (deref register int)
;;; instr ::= (addq arg arg) | (subq arg arg) | (negq arg) | (movq arg arg)
;;;         | (callq label) | (pushq arg) | (popq arg) | (retq)
;;; x86 ::= (program int instr+)



;;; R_1 --(uniquify)--> R_1 --(flatten)--> C_0

;;; C_0 intermediate language
;;; arg ::= int | var
;;; exp ::= arg | (read) | (- arg) | (+ arg arg)
;;; stmt ::= (assign var exp) | (return arg)
;;; C_0 ::= (program (var*) stmt+)


;;; C_0 --(select-instr)--> X86* --(assign-homes)--> X86* --(patch-instr)--> X86

;;; Implementation of uniquify pass
(define (uniquify alist)
  (lambda (e)
    (match e
      [(? symbol?) (if (null? alist) e (car alist))]
      [(? integer?) e]
      [`(let ([,x ,e]) ,body) (let ([uni-x (gensym x)])
                                `(let ([,uni-x ,((uniquify alist) e)]) ,((uniquify (cons uni-x alist)) body)))]
      [`(program ,e)
       `(program ,((uniquify alist) e))]
      [`(,op ,es ...)
       `(,op ,@(map (uniquify alist) es))] ;;use of comma-@ operator to splice a list of S-expressions into an enclosing S-expression.
)))
(define unify (uniquify '()))

;;; helper function
;;; map3 function is useful for applying a function to each element fo a list, in the case where the function returns three values.
;;; The result of map3 is three lists.
(define (map3-internal rlist1 rlist2 rlist3)
  (lambda (f e-list)
    (cond
      [(null? e-list) (values rlist1 rlist2 rlist3)]
      [else (let-values ([(r1 r2 r3) (f (car e-list))])
                   ((map3-internal (cons r1 rlist1)
                                   (cons r2 rlist2)
                                   (cons r3 rlist3)) f (cdr e-list)))]
      )))
(define map3 (map3-internal '() '() '()))

(define (flatten assign-lst var-lst) 
  (lambda (e)
    (match e
      [(? symbol?) ]
      [(? integer?) ]
      [`(let ([,x ,e]) ,body)
       (let-values ([(r-new-exp r-assign-lst r-var-lst) ((flatten assign-lst exp-lst) e)])
         ((flatten (cons `(assign ,x ,r-new-exp) r-assign-lst) (cons x r-var-lst)) body))]
      [`(program ,e)
       (let-values ([(r-new-exp r-assign-lst r-var-lst) ((flatten assign-lst exp-lst) e)])
         (append (cons 'program (cons r-var-lst r-assign-lst)) `(return ,r-new-exp)))]
      [`(,op ,es
                                                         