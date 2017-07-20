#lang racket

(require rackunit
         binary-tree/binary-tree)

(define tree-size 100)
(define tree (make-binary-tree))
(define full-tree (foldl binary-tree-insert tree (shuffle (range tree-size))))

(test-case
  "binary-tree-empty?"
  (check-true (binary-tree-empty? tree))
  (check-false (binary-tree-empty? full-tree)))

(test-case
  "binary-tree-exists?"
  (check-true (binary-tree-exists? 50 full-tree))
  (check-false (binary-tree-exists? tree-size full-tree)))

(test-case
  "binary-tree-insert"
  (check-false (binary-tree-exists? 50 tree))
  (check-true (binary-tree-exists? 50 (binary-tree-insert 50 tree))))

(test-case
  "binary-tree-in-order"
  (check-equal?
    (binary-tree-in-order identity full-tree)
    (range tree-size)))

(test-case
  "binary-tree-delete"
  (check-equal?
    (let ((random (shuffle (range tree-size)))
          (tree-pair (cons full-tree (range tree-size))))
      (foldl (lambda (val tree-pair)
               (let ((new-tree (binary-tree-delete val (car tree-pair)))
                     (new-lst (remove val (cdr tree-pair))))
                 (check-equal? (binary-tree-in-order identity new-tree) new-lst)
                 (cons new-tree new-lst)))
             tree-pair
             random))
    (cons (make-binary-tree) '())))
