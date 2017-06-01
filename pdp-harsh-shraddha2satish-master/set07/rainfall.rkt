;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This program is meant to collect the readings
;; of rainfall and finding the non average of the non negative readings.
;; If a reading reads -999, the operation aborts there.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requires
(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides for automated testing
(provide rainfall)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
(define BASE 0)
(define TEST-RESULT1 2)
(define TEST-RESULTCHECK 3)
(define TEST-RESULTSUM 50)
(define THRESHOLD -999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declaration of constants for testing
(define sample-list (list 10 20 -2 -9 20 -999))
(define sample-list2 (list 1 2 3 -2 -999))
(define sample-999 (list -999))
(define sample-empty (list empty))
(define list1 (list 10 20 -2 -9 20))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainfall: ListOfReadings -> PosReal
;; Given: A list of readings of rainfall whose average should be found
;; Returns: The average of the positive readings of the list.
;; Where: negative readings are bypassed and operation is aborted if
;; -999 is encountered.
;; Examples: (rainfall (list 3 4 5 6 -1 7 -999 0)) 5
;; Design Strategy: Combining simpler functions
;; Function Definition:
(define (rainfall list-of-readings)
  
  (/ (sum (sublist list-of-readings))
     (sublist-length (sublist list-of-readings))))
;; Tests
(begin-for-test
  (check-equal? (rainfall sample-list2) TEST-RESULT1)
  "Returns the average for the rainfall recorded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List template for list-of-readings
;; list-fn: list -> ??
;; (define (list-fn lst)
;;     (cond
;;       [(empty? lst) ... ]
;;       [else (... (first lst)
;;                  (list-fn (rest lst)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sublist: ListOfReadings -> ListOfReadings
;; Given: a list of rainfall readings that may contain
;; positive, negative values and -999
;; Returns: A sublist of the given sublist where the length
;; of the sublist has been restricted to the first occurance of
;;;-999(threshold value).
;; Examples:
;; (sublist sample-list)
;; (list 100 200 -20 -9 20)
;; Design strategy: Using list template on lon
;; Function definition:
(define (sublist lon)
  (cond
    [(empty? lon) empty]
    [else (if (equal? (first lon) THRESHOLD)
              empty
              (cons (first lon) (sublist (rest lon))))]))
;; Tests
(begin-for-test
  (check-equal? (sublist sample-list) list1
                "Returns the sublist of the sample list")
  (check-equal? (sublist sample-empty) (list '())
                "Returns the sublist of the empty list")
  (check-equal? (sublist sample-999) '()
                "Returns the sublist of the sample list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum: ListOfReadings -> PosReal
;; Given: a list of rainfall readings in which positive and negative
;; values may be present, but does not contain -999.
;; Returns: the sum of the positive readings of the list
;; Examples: (sum sample-list)
;; 320
;; Design Strategy: using list template on lon
;; Function definition:
(define (sum lon)
  (cond
    [(empty? lon) BASE]
    [else (if (>= (first lon) BASE)
              (+ (first lon) (sum (rest lon)))
              (sum (rest lon)))]))
;; Tests
(begin-for-test
  (check-equal? (sum sample-list) TEST-RESULTSUM
                 "Returns the sum of the sample list")
  (check-equal? (sum sample-999) BASE
   "Returns the sublist of the sample list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; len: ListOfReadings -> PosReal
;; Given: a list of readings of rainfall
;; Returns: The length of the sublist containing only positive or
;; 0 reading
;; Examples:
;; (len sample-list)
;; 3
;; (len sample-999)
;; 0
;; Design Strategy: using list template on lon
;; Function definition:
(define (sublist-length lon)
  (cond
    [(empty? lon) BASE]
    [else (if (< (first lon) BASE)
              (sublist-length (rest lon))
              (add1 (sublist-length (rest lon))))]))
;; Tests
(begin-for-test
  (check-equal? (sublist-length sample-list) TEST-RESULTCHECK
                 "Returns the sublistlength of the sample list")
  (check-equal? (sublist-length sample-999) BASE
 "Returns the sublist length of the sample list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


