#lang racket
 
(require rackunit
         "array-list.rkt")

(let ((array-list (make-array-list)))
  (check-equal? (array-list-empty? array-list) #t "Array list should be empty")
  (check-equal? (array-list-length array-list) 0 "Array list should have length of 0")
  (let ((array-list (array-list-add array-list 1)))
	(check-equal? (array-list-empty? array-list) #f "Array list should not be empty")
	(check-equal? (array-list-length array-list) 1 "Array list should have length of 1")
	(check-equal? (array-list-get array-list 0) 1 "The first element should be 1")
	(let ((array-list (array-list-set array-list 205 8)))
	  (check-equal? (array-list-length array-list) 206 "Array list should have length of 206 (ony more than last element's index)")
	  (let ((array-list (array-list-add array-list 9)))
		(check-equal? (array-list-get array-list 0) 1 "The first element should be 1")
		(check-equal? (array-list-get array-list 100) 0 "Elements not explicitly set should b 0")
		(check-equal? (array-list-get array-list 205) 8 "The 205th element should be 8")
		(check-equal?
		  (array-list-get array-list (- (array-list-length array-list) 1))
		  9
		  "The last element should be 9")))))
