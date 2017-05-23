#lang racket

(provide make-array-list array-list-set array-list-get array-list-size)

(define (make-array-list) (cons 0 (make-vector 10 0)))

(define (array-list-get array-list index)
  (if (>= index (array-list-size array-list))
    0
    (vector-ref (array-list-vector array-list) index)))

(define (array-list-set array-list index value)
  (let* ((array (array-list-vector array-list))
         (vector-size (vector-length array))
         (size (max (+ index 1) (array-list-size array-list)))
         (new-vector-size (if (> size vector-size) (* size 2) vector-size))
         (new-vector (copy-vector array (make-vector new-vector-size 0))))
    (begin (vector-set! new-vector index value) (cons size new-vector))))

(define array-list-size car)

(define array-list-vector cdr)

(define (copy-vector small large) (copy-vector-helper small large 0) )

(define (copy-vector-helper small large index) 
  (if (>= index (vector-length small))
    large
    (begin
      (vector-set! large index (vector-ref small index))
      (copy-vector-helper small large (+ index 1)))))

; (define array-list (make-array-list))
; (array-list-set array-list 0 2)
; (array-list-set (array-list-set array-list 5 3) 10 1)
; (array-list-set (array-list-set array-list 5 3) 40 1)
