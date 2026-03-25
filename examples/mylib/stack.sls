;; 数据结构库：栈
(library (mylib stack)
  (export make-stack push! pop! peek empty? stack->list)
  (import (chezscheme))

  (define-record-type stack
    (fields (mutable items))
    (protocol
     (lambda (new)
       (lambda ()
         (new '())))))

  (define (push! stk item)
    (unless (stack? stk)
      (error 'push! "not a stack"))
    (stack-items-set! stk (cons item (stack-items stk))))

  (define (pop! stk)
    (unless (stack? stk)
      (error 'pop! "not a stack"))
    (when (null? (stack-items stk))
      (error 'pop! "stack is empty"))
    (let ([item (car (stack-items stk))])
      (stack-items-set! stk (cdr (stack-items stk)))
      item))

  (define (peek stk)
    (unless (stack? stk)
      (error 'peek "not a stack"))
    (when (null? (stack-items stk))
      (error 'peek "stack is empty"))
    (car (stack-items stk)))

  (define (empty? stk)
    (unless (stack? stk)
      (error 'empty? "not a stack"))
    (null? (stack-items stk)))

  (define (stack->list stk)
    (unless (stack? stk)
      (error 'stack->list "not a stack"))
    (stack-items stk))

  ) ;; end of library
