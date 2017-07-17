#lang racket

(require rackunit
         queue/queue)

(define queue (make-queue))
(define full-queue (foldl queue-push queue '(1 2 3 4 5 6 7 8 9 10)))

(test-case
  "queue-empty?"
  (check-true (queue-empty? queue))
  (check-false (queue-empty? full-queue)))

(test-case
  "queue-shift"
  (foldl (lambda  (val queue)
              (queue-shift (lambda (queue-val new-queue)
                           (check-equal? queue-val val)
                           new-queue)
                         queue))
         full-queue
         '(1 2 3 4 5 6 7 8 9 10 ())))

(test-case
  "queue-push"
  (check-false (queue-empty? (queue-push 1 queue))))
