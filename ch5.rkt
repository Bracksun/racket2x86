#lang racket


(define expose-allocation
  (lambda (exp)
    (match exp
      [`(has-type (vector ,es ...) ,tp)
       (let* ([len (length es)]
              [bytes (* 8 (+ len 1))]
              [v-name (gensym 'v)])
         (define each-elem-let
           (lambda (exps)
             (if (null? exps)
                 `(let ([,(gensym 'collect) (if (< (+ (global-value free_ptr) ,bytes)
                                                   (global-value fromspace_end))
                                                   (void)
                                                   (collect ,bytes))])
                    (let ([,v-name (allocate ,len ,tp)])
                      ,(vector-set-let 0)))
                 `(let ([,(string->symbol (string-append "x" (number->string (- len (length exps))))) (car exps)])
                    ,(each-elem-let (cdr exps))))))
         (define vector-set-let
           (lambda (idx)
             (if (eq? idx len)
                 v-name
                 `(let ([,(gensym 'vector-set) (vector-set! ,v-name ,idx ,(string->symbol (string-append "x" (number->string idx))))])
                    ,(vector-set-let (+ idx 1))))))
         (each-elem-let es))]
      )))