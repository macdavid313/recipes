;;;; stack.ss

#!chezscheme
(library (stack)
  (export make-stack stack? stack-push! stack-pop! stack-empty? stack-top stack-size)
  (import (chezscheme))

  (define-record-type (stack mk-stack stack?)
    (fields (mutable items)))

  (define (make-stack)
    (mk-stack (list)))

  (define (stack-push! stack item)
    (stack-items-set! stack (cons item (stack-items stack))))

  (define (stack-pop! stack)
    (if (null? (stack-items stack))
        (error 'stack-pop! "stack is empty")
        (let ([item (car (stack-items stack))])
          (stack-items-set! stack (cdr (stack-items stack)))
          item)))

  (define (stack-empty? stack)
    (null? (stack-items stack)))

  (define (stack-top stack)
    (if (null? (stack-items stack))
        (error 'stack-top "stack is empty")
        (car (stack-items stack))))

  (define (stack-size stack)
    (length (stack-items stack)))
  ) ;; end of library
