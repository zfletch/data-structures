#lang racket

(require rackunit
         stack/stack)

(define stack (make-stack))
(define full-stack (foldl stack-push stack '(1 2 3 4 5 6 7 8 9 10)))

(test-case
  "make-stack"
  (check-equal? stack '()))

(test-case
  "stack-empty?"
  (check-true (stack-empty? stack))
  (check-false (stack-empty? full-stack)))

(test-case
  "stack-pop"
  (foldl (lambda  (val stack)
              (stack-pop (lambda (stack-val new-stack)
                           (check-equal? stack-val val)
                           new-stack)
                         stack))
         full-stack
         '(10 9 8 7 6 5 4 3 2 1 ())))

(test-case
  "stack-push"
  (check-false (stack-empty? (stack-push 1 stack))))
