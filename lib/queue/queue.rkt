#lang racket

(provide make-queue queue-empty? queue-shift queue-push)

(define (make-queue) '(() . ()))

(define (queue-empty? queue) (equal? queue (make-queue)))

(define (queue-shift fn queue)
  (if (queue-empty? queue)
    (fn '() (make-queue))
    (let* ((forwards (car queue))
          (backwards (cdr queue))
          (new-forwards '())
          (new-backwards (append (reverse forwards) backwards)))
      (fn (car new-backwards) (cons new-forwards (cdr new-backwards))))))

(define (queue-push val queue)
  (let ((forwards (car queue))
        (backwards (cdr queue)))
    (cons (cons val forwards) backwards)))
