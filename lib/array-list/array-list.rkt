#lang racket

(provide make-array-list
		 array-list-empty?
		 array-list-length
		 array-list-get
		 array-list-set
		 array-list-add)

(define (make-array-list) (cons 0 (make-vector 10 0)))

(define (array-list-empty? array-list)
  (= (array-list-length array-list) 0))

(define array-list-length car)

(define (array-list-get array-list index)
  (if (>= index (array-list-length array-list))
    0
    (vector-ref (array-list-vector array-list) index)))

(define (array-list-set array-list index value)
  (let* ((array (array-list-vector array-list))
         (vector-length (vector-length array))
         (length (max (+ index 1) (array-list-length array-list)))
         (new-vector-length (if (> length vector-length) (* length 2) vector-length))
         (new-vector (copy-vector array (make-vector new-vector-length 0))))
    (begin (vector-set! new-vector index value) (cons length new-vector))))

(define (array-list-add array-list value)
  (array-list-set array-list (array-list-length array-list) value))

(define array-list-vector cdr)

(define (copy-vector small large) (copy-vector-helper small large 0) )

(define (copy-vector-helper small large index) 
  (if (>= index (vector-length small))
    large
    (begin
      (vector-set! large index (vector-ref small index))
      (copy-vector-helper small large (+ index 1)))))
