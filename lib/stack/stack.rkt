#lang racket

(provide make-stack stack-empty? stack-pop stack-push)

(define (make-stack) '())

(define (stack-empty? stack) (equal? stack '()))

(define (stack-pop fn stack)
  (if (stack-empty? stack)
    (fn '() '())
    (fn (car stack) (cdr stack))))

(define (stack-push val stack) (cons val stack))
