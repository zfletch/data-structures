#lang racket

(provide make-binary-tree
         binary-tree-empty?
         binary-tree-exists?
         binary-tree-insert
         binary-tree-delete
         binary-tree-in-order)

(define (make-binary-tree) '())

(define (make-node value) (list value (make-binary-tree) (make-binary-tree)))

(define (binary-tree-empty? tree) (equal? tree (make-binary-tree)))

(define (binary-tree-exists? value tree)
  (if (binary-tree-empty? tree)
    #f
    (let ((tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (cond
        ((< value tree-value) (binary-tree-exists? value left))
        ((> value tree-value) (binary-tree-exists? value right))
        (else #t)))))

(define (binary-tree-insert value tree) (insert-helper (make-node value) tree))

(define (insert-helper node tree)
  (cond
    ((binary-tree-empty? tree) node)
    ((binary-tree-empty? node) tree)
    (else
      (let ((value (car node)) (tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
        (if (< value tree-value)
          (list tree-value (insert-helper node left) right)
          (list tree-value left (insert-helper node right)))))))

(define (minimum-node tree)
    (let ((tree-value (car tree)) (left (cadr tree)))
      (cond
        ((binary-tree-empty? left) tree)
        (else (minimum-node left)))))

(define (binary-tree-delete value tree)
  (if (binary-tree-empty? tree)
    tree
    (let ((tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (cond
        ((< value tree-value) (list tree-value (binary-tree-delete value left) right))
        ((> value tree-value) (list tree-value left (binary-tree-delete value right)))
        ((binary-tree-empty? left) right)
        ((binary-tree-empty? right) left)
        (else
          (let* ((minimum (minimum-node right)) (minimum-value (car minimum)))
            (list minimum-value left (binary-tree-delete minimum-value right))))))))

(define (binary-tree-in-order fn tree)
  (binary-tree-in-order-helper fn tree '()))

(define (binary-tree-in-order-helper fn tree lst)
  (if (binary-tree-empty? tree)
    lst
    (let ((tree-value (car tree)) (left (cadr tree)) (right (caddr tree)))
      (binary-tree-in-order-helper
        fn
        left
        (cons (fn tree-value) (binary-tree-in-order-helper fn right lst))))))
