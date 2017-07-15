#lang racket

(provide make-stack stack-empty? stack-push stack-pop)

(define (make-stack) '())

(define (stack-empty? stack) (equal? stack '()))

(define stack-push car)

(define (stack-pop stack) stack)
