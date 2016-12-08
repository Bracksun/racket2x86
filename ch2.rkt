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