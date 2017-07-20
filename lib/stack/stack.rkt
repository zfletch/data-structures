#lang racket

(provide make-stack stack-empty? stack-pop stack-push)

(define (make-stack) '())

(define (stack-empty? stack) (equal? stack (make-stack)))

(define (stack-pop fn stack)
  (if (stack-empty? stack)
    (fn '() (make-stack))
    (fn (car stack) (cdr stack))))

(define (stack-push val stack) (cons val stack))
