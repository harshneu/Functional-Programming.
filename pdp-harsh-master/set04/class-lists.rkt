;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;A class Attebdance List.
;;The code works on an algorithm to remove the duplication in  the rosters
;;of Prof.Felleissen and Prof.Shivers regarding the ambiguty of the students
;;in each of their classes that might have occurred after the
;;incident of their collision.

(require rackunit)                    ;;Testing Framework.
(require "extras.rkt")


;; A List of Slips (LOS) is one of:
;; -- empty
;; -- (cons Number LOS)

;; Template:
;; los-fn : LOS -> ??
;; (define (los-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (los-fn (rest lst)))]))

;;Data Definition:

;;A slip is a (make-slip color String String)
;;A Color is one of
;;--"yellow"
;;--"blue"
;;Interp:color is the color of the slip which each professor keeps
;;and records for the attendance.
;;Interp:name 1 is the first name of the student in the professor's class.
;;       name-2 is the last name of the student in the professor's class.
;;Example:(SLIP-1 (make-slip "blue" "Harsh" "Shukla"))
;;        (Slip-2 (make-slip "yellow" "Wang" "Xi"))
;;Strategy:Use Simple Function.

;;Template:
;;slip-fn:slip->??.
;;(define (slip-fn n)
;;... (color)
;;   ...(name-1)
;;   ...(name-2)

(define-struct slip (color name1 name2))

;;felleisen-roster : ListOfSlip -> ListOfSlip                  
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Felleisen's class, without duplication.
;;Strategy: Combine simpler functions.
;;Example:
;;(felleisen-roster LOS-2)
;;(list (make-slip "yellow" "Harsh" "Shukla"))

;;Template:
;;list-fn:ListOfX -> ??
;;(define (list-fn lst)
;;(cond
;;  [(empty? lst)...]
;;  [else(... (first lst)
;;            (list-fn (rest lst)))]))


(define (felleisen-roster los)
  (concat1 los empty))

;;Tests:See tests below.

;;concat1: ListOfSlip-> ListOFSlip.
;;Given:A list of slip.
;;Returns:A list where we apply operations to sort the list for the Professors.
;;Interp: los is the list of slip of the professors.
;;Strategy:Combine simpler functions.
;;Example:
;;(list
;; (make-slip "yellow" "Harry" "Potter")
;; (make-slip "yellow" "Harsh" "Shukla")
;; (make-slip "yellow" "John" "Nash"))

(define (concat1 los fans)
  (cond
    [(empty? los) fans]
    [(string=? (slip-color (first los)) "yellow")
     (concat1(rest los) (update-list (first los) fans))]
    [else (concat1(rest los) fans)]))
;;Tests:
(begin-for-test
  (check-equal?
   (concat1 los1 los2)
   (list
    (make-slip "yellow" "Harry" "Potter")
    (make-slip "yellow" "Harsh" "Shukla")
    (make-slip "yellow" "John" "Nash"))
   "Returns the list after concatenation"))


;;shivers-roster: ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Shivers' class, without duplication.
;;Strategy:Combine simpler functions.
;;Interp:los is the list that the professor keeps for recording the attendance.
;;Example:
;;(shivers-roster los1)
;;(list (make-slip "blue" "Harsh" "Shukla"))
;;Use template as of felleisen-roster.

(define (shivers-roster los)
  (concat los empty))

;;Tests: See tests below.

;;concat1: ListOfSlip-> ListOFSlip.
;;Given:A list of slip.
;;Returns:A list where we apply operations to sort the list for the Professors.
;;Interp: los is the list of slip of the professors.
;;Strategy:Combine simple function.
(define (concat los sans)
  (cond
    [(empty? los)sans]
    [(string=? (slip-color (first los)) "blue")
     (concat(rest los) (update-list (first los) sans))]
    [else (concat(rest los) sans)]))
;;Tests:
(begin-for-test
  (check-equal?
   (concat los1 los2)
   (list
    (make-slip "blue" "shukla" "harsh")
    (make-slip "blue" "Harsh" "Singh")
    (make-slip "blue" "Harry" "Potter")
    (make-slip "blue" "Harsh" "Shukla")
    (make-slip "yellow" "John" "Nash"))
   "Returns the list after concatenation"))

;;update-list:Slip->list
;;Given:A slip with name of students.
;;Returns:A list of the students with duplication.
;;Strategy:Use simpler function.
;;Example:(update-list (make-slip "blue" "Harsh" "Singh") los1)
;         (list
; (make-slip "blue" "Harsh" "Shukla")
; (make-slip "yellow" "Harsh" "Shukla")
; (make-slip "blue" "Harry" "Potter")
; (make-slip "blue" "Harsh" "Singh")
; (make-slip "blue" "Harsh" "Shukla"))

(define (update-list s ls)
  (if (check-dup? s ls)
      (cons s ls)
      ls))
;;Tests:See test below.


;;check-dup?:ListOfSlips->Boolean.
;;Given:A list of Slips.
;;Returns :A boolean value true if there is no duplication else false.
;;Strategy:Combine Simpler function.
;;Example:(check-dup? (make-slip "blue" "Harsh" "Singh") los1)
;;       #false

(define (check-dup? s ls)
  (cond [(empty? ls) true]
        [(equal-slip? (first ls) s) false]
        [else (check-dup? s(rest ls))]))

;;equal-slip? :Slip-> Slip.
;;Given:Slips for comparison.
;;Returns:A boolean value after comparing the slips and
;;checks only for name values.
;;Strategy: Use simpler function.
;;Example:
;;(equal-slip? (make-slip "blue" "Harsh" "Shukla")
;; (make-slip "yellow" "Harshal" "Shukla"))
;;#false

(define (equal-slip? s1 s2)
  (or
   (and
    (equal? (slip-name1 s1) (slip-name1 s2))
    (equal? (slip-name2 s1) (slip-name2 s2)))
   (and
    (equal? (slip-name1 s1) (slip-name2 s2))
    (equal? (slip-name2 s1) (slip-name1 s2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Example list for Testing.
(define los1
  (list
   (make-slip "blue" "Harsh" "Shukla")
   (make-slip "yellow" "Harsh" "Shukla")
   (make-slip "blue" "Harry" "Potter")
   (make-slip "blue" "Harsh" "Shukla")
   (make-slip "blue" "Harsh""Shukla")
   (make-slip "yellow" "Harsh" "Shukla")
   (make-slip "blue" "Harsh" "Singh")
   (make-slip "yellow" "Shukla" "Harsh")
   (make-slip "yellow" "Harry" "Potter")
   (make-slip "blue" "Shukla" "Harsh")
   (make-slip "blue" "shukla" "harsh")))
(define los2
  (list
   (make-slip "yellow" "John" "Nash")))


;;Tests:
(begin-for-test
  (check-equal? (felleisen-roster los1)
                (list
                 (make-slip "yellow" "Harry" "Potter")
                 (make-slip "yellow" "Harsh" "Shukla"))
                "Should Return the list after eliminating
duplication in Prof.Felleisen's class")
  
  (check-equal? (shivers-roster los1)
                (list
                 (make-slip "blue" "shukla" "harsh")
                 (make-slip "blue" "Harsh" "Singh")
                 (make-slip "blue" "Harry" "Potter")
                 (make-slip "blue" "Harsh" "Shukla"))
                "Should Return the list after eliminating duplication
 in Prof. Shiver's class"))




