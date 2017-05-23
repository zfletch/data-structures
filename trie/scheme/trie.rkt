#lang racket

(provide make-trie trie-insert trie-exists?)

(define (make-trie) '())
(define (trie-insert trie key) (trie-insert-helper trie (string->list key)))
(define (trie-exists? trie key) (trie-exists-helper trie (string->list key)))

(define (trie-insert-helper trie lst)
  (let ((head (car lst)) (tail (cdr lst)))
    (if (null? trie)
      (if (null? tail)
        (list (list head #t '()))
        (list (list head #f (trie-insert-helper '() tail))))
      (let ((value (caar trie)) (end? (cadar trie)) (children (caddar trie)))
        (if (equal? value head)
          (if (null? tail)
            (cons (list value #t children) (cdr trie))
            (cons (list value end? (trie-insert-helper children tail)) (cdr trie)))
          (cons (car trie) (trie-insert-helper (cdr trie) lst)))))))

(define (trie-exists-helper trie lst)
  (let ((head (car lst)) (tail (cdr lst)))
    (if (null? trie)
      #f
      (let ((value (caar trie)) (end? (cadar trie)) (children (caddar trie)))
        (if (equal? value head)
          (if (null? tail)
            end?
            (trie-exists-helper children tail))
          (trie-exists-helper (cdr trie) lst))))))

; (trie-insert (trie-insert (make-trie) "hello") "hellno")
; (trie-exists? (trie-insert (trie-insert (make-trie) "hello") "hellno") "foo")
