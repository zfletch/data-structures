#lang racket

(provide create insert exists? delete in-order pre-order post-order)

(define (create) '())

(define (insert tree value) (insert-node tree (list value '() '())))

(define (exists? tree value)
  (if (null? tree)
    #f
    (let ((tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (cond
        ((< value tree-value) (exists? left value))
        ((> value tree-value) (exists? right value))
        (else #t)))))

(define (delete tree value)
  (if (null? tree)
    '()
    (let ((tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (cond
        ((< value tree-value) (list tree-value (delete left value) right))
        ((> value tree-value) (list tree-value left (delete right value)))
        (else (insert-node left right))))))

(define (in-order tree) (in-order-helper tree '()))
(define (pre-order tree) (pre-order-helper tree '()))
(define (post-order tree) (post-order-helper tree '()))

; helper functions

(define (insert-node tree node)
  (cond
    ((null? tree) node)
    ((null? node) tree)
    (else
      (let ((value (car node)) (tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
        (if (< value tree-value)
          (list tree-value (insert-node left node) right)
          (list tree-value left (insert-node right node)))))))

(define (in-order-helper tree accumulator)
  (if (null? tree)
    accumulator
    (let ((value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (in-order-helper right (cons value (in-order-helper left accumulator))))))

(define (pre-order-helper tree accumulator)
  (if (null? tree)
    accumulator
    (let ((value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (pre-order-helper right (in-order-helper left (cons value accumulator))))))

(define (post-order-helper tree accumulator)
  (if (null? tree)
    accumulator
    (let ((value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (cons value (post-order-helper right (post-order-helper left accumulator))))))
