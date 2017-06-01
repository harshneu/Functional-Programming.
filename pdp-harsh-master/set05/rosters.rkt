;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)                  
(require "extras.rkt")

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 roster=?
 rosterset=?
 enrollments-to-rosters)

;; A List of X (LOX) is one of:
;; -- empty
;; -- (cons Number LOX)

;; Template:
;; loX-fn : LOX -> ??
;; (define (lox-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                 (los-fn (rest lst)))]))


;;Data Definition

;;Enrollment.
(define-struct enrollment(student class))

;;Constructor Template:
;;An Enrollment is a (make-enrollment Student Class)

;;Interpretation:
;; Enrollment represents the assertion that student s is
;;enrolled in class c.
;;Interp: A student s can be of any type and is enrolled in the class
;;of a particular subject.
;;Interp: A class c is the subject in which the student has been enrolled.
;;Template:
;;enrollment-fn:enrollment->??.
;;(define (enrollment-fn n)
;;... 
;;   ...(enrollment-student s)
;;   ...(enrollment-class c)


;;Roster.
(define-struct roster(classname students))

;;Constructor Template:
;;A roster is a (make-roster classname students)

;;Interpretation:
;;(make-roster c ss) represents that the students in class c are exactly
;;the students in set ss.
;;Interp: A classname c is the name of the class in which the givenlist of
;;students are enrolled in. 
;;Interp: A students ss is the list of all the students in the list
;;of students ,a single list should not return duplicate values.


;;Template:
;;roster-fn:roster->??.
;;(define (roster-fn n)
;;... 
;;   ...(roster-classname c)
;;   ...(students-fn (roster-students ss))

;;Template for listofstudents
;; los-fn : LOS -> ??
;; (define (loS-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (student-class)
;;                 (student-name)))]))

;;Class
(define-struct class(name))

;;Constructor Template:

;;A Class is a (make-class subject setofstudents)
;;(make-class s ss) represents the students who have enrolled themselves
;;in the classof a particular subject.
;;Interp:A subject s is the class in which a list of students
;;have enrolled themselves
;; in a particular class.
;;Interp: A setofstudent ss is the list of all the students in the list
;;of students.


;;Template:
;;class-fn:class->??.
;;(define (class-fn n)
;;... 
;;   ...(class-name n)



;;Student
(define-struct student (name subject))
;;A student is a (make-student name class)
;;(make-student n sub) represents the detail of the students
;;which includes their name
;;and classes in which they have enrolled themselves.
;;Interp:A name s is thename of the student.
;;Interp: A subject is the subject in which the student has enrolled into.


;;Template:
;;student-fn:student->??.
;;(define (student-fn n)
;;... 
;;   ...(name)
;;   ...(subject)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Example list for Testing:
;;ListRoster1
(define sor1
  (list
   (make-roster "PDP" (list "Harsh" "Varun" "Rahul" "Mohit"))
   (make-roster "Networks" (list "Harsh" "Varun" "MOhit" "Salman"))          
   ))
;;ListRoster2
(define sor2
  (list
   (make-roster "PDP" (list "Harsh" "Varun" "Rahul" "Mohit"))
   (make-roster "Networks" (list "Harsh" "Varun" "MOhit" "Salman"))          
   ))
;;ListRoster3
(define sor
  (list
   (make-roster "PDP" (list "Harsh" "Varun" "Rahul" "Mohit"))
   (make-roster "Networks" (list "Harsh" "Varuni" "MOhit" "Salman")) ))

;;Roster1
(define roster1
  (make-roster "PDP" (list "Harsh" "Varun" "Rahul" "Mohit")))
;;Roster2
(define roster2
  (make-roster "PDP" (list "Harsh" "Varun" "Rahul" "Mohit")))
;;Roster3
(define roster4
  (make-roster "Networks" (list "Harsh" "Varuni" "MOhit" "Salman")))
;;ListofEnrollment1
(define soe1
  (list
   (make-enrollment "Harsh" "PDP")
   (make-enrollment "Harsh" "PDP")
   (make-enrollment "Varun" "PDP")
   (make-enrollment "Megha" "Networks")
   (make-enrollment "Sachin" "Networks")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;roster=? : ClassRoster ClassRoster -> Boolean
;;Given: A member for the function. 
;;RETURNS: true iff the two arguments represent the same roster
;;Strategy:use template for struct roster.

;;Examples:
;;(roster=? roster1 roster2)
;;#false


;;Function Definition.
(define (roster=? roster1 roster2)
  (and
   (equal? (roster-classname roster1)(roster-classname roster2))
   (equal? (roster-students roster1) (roster-students roster2))))


;;Tests
(begin-for-test
  (check-false (roster=? roster1 roster4)
               "Should return false as the rosters aren't equal")
  (check-true (roster=? roster1 roster2)
              "Should return true as the rosters are equal"))


;rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;;Given: Two lists for comparison. 
;;RETURNS: true iff the two arguments represent the same set of rosters
;;Strategy:use template on the list for list of roster.

;;Examples:
;;rosterset=? sor1 sor)
;;#false


;;Function Definition.
(define(rosterset=? sor1 sor)
  (and
   (subset? sor1 sor)
   (subset? sor sor1)))
;;Tests
(begin-for-test
  (check-false (rosterset=? sor1 sor)
               "Should return false as the rosterslist aren't equal")
  (check-true (rosterset=? sor1 sor2)
              "Should return true as the rosterslist are equal"))


;;subset=? :SetofRoster->SetofRoster -> Boolean
;;Given:Two set of roster for comparison.
;;Returns:Checks for condition if  element of a single set belongs to
;;another set too.
;;Strategy:Use HOF ormap .

;;Examples:
;;(subset? sor1 sor2)
;;#true


;;Function Definition.
(define (subset? set1 set2)
  (andmap
   ;;SetOfRoster->Boolean
   ;;Returns:True,iff the sets are subset of each other.
   (lambda (a) (member? a set2))
   set1))

;;Tests
(begin-for-test
  (check-true (subset? sor1 sor2)
              "Should return true as the set1 is a subset of other")
  (check-false (subset? sor1 sor)
               "Should return false as the set1 is not a subset of other"))

;;enrollments-to-rosters: SetOfEnrollment -> SetOfClassRoster.
;;GIVEN: a set of enrollments.
;;RETURNS: the set of class rosters for the given enrollments.
;;Examples:
;;(enrollments-to-rosters soe1)
;;(list
;; (make-roster "Networks" (list "Megha"))
;; (make-roster "PDP" (list "Varun" "Harsh")))
;;Design Strategy:Use template for lists.

;;FunctionalDefinition:

(define (enrollments-to-rosters soe)
  (concat soe empty))

;;concat: ListOfSlip ListOfSlip String-> ListOFSlip.
;;Given:A list of slip.
;;Returns:A list where we apply operations on the rosters.
;;Interp: soe is the set of enrollments.
;;Strategy:Use template for lists.

;;Example:
;; (concat soe1 sor1)
;;(list
;; (make-roster
;;  "PDP" (list "Harsh" "Varun" "Rahul" "Mohit"))
;; (make-roster
;;  "Networks"(list "Megha" "Harsh" "Varun" "MOhit" "Salman")))


;;Function Definition.
(define (concat soe fl)
  (cond [(empty? soe) fl]
        [(check-presence? (first soe) fl)
         (concat (rest soe)
                 (update-list (first soe) fl))]
        [else(concat (rest soe)
                     (add-list (first soe) fl))]))

;;contains?:SetOfEnrollment SetOfEnrollment->Boolean.
;;Given:A seet of enrollment and a set of rosters.
;;Returns:An boolean value if both the sets have the entries for an enrollment.
;;Design Strategy:Use HOF ormap on set of enrollment and set of rosters.

;;Examples:
;;(contains?  soe1 sor)
;;#false

;;Function Definition.
(define (contains? l sn)
  (ormap
   ;;Any->Bool
   ;;Returns:True iff an enroollment belongs to a particular set.
   (lambda (e)
     (equal? sn e)) l))


;;tests:See test below.


;;check-presence:enrollment ListofRoster->Boolean
;;Given:An enrollment and a list of rosters
;;Returns:A boolean value after checking whether an enrollment
;;is there in the roster or not.
;;Design Strategy:Use HOF ormap on the function .

;;Example:
;;(check-presence? (make-enrollment "Harsh" "PDP")sor1)
;;#true


;;Function Definition.
(define (check-presence? e fl)
  (ormap
   ;;Any->Bool
   ;;Returns:True iff the student is already there in the enroolment list.
   (lambda (l)
     (equal?
      (enrollment-class e)
      (roster-classname l)))
   fl))

;;Tests:
(check-true (check-presence? (make-enrollment "Harsh" "PDP") sor1)
            "Returns a true value after matching the conditions")


;;update-list:enrollment ListofRoster->ListofEnrollment.
;;Given:An enrollment and a list of rosters
;;Returns:An updated list after adding the given list.
;;Design Strategy:Use HOF map .

;;Example:
;;(update-list (make-enrollment "Harshal" "PDP")sor1)
;;(list
;; (make-roster
;;  "PDP" (list "Harshal" "Harsh" "Varun"
;;   "Rahul" "Mohit"))(make-roster
;;  "Networks" (list "Harsh" "Varun" "MOhit" "Salman")))


;;Function Definition.
(define (update-list e fl)
  (map
   ;;Any->Listof Enrollment.
   ;;Returns:An updated list for the roster.
   (lambda (l) (if (and (equal?
                         (enrollment-class e)
                         (roster-classname l))
                        (not-dup? e fl))
                   (make-roster (roster-classname l)
                                (cons (enrollment-student e)
                                      (roster-students l))) l))
   fl))

;;Tests:Used in test cases.


;;not-dup?:enrollment ListofRoster->Boolean.
;;Given:An enrollment and a list of rosters
;;Returns:A boolean value after checking that the entry
;;we are trying to make isn't any
;;duplicate entry.
;;Design Strategy:Use HOF ormap .

;;Example:
;;(not-dup?  (make-enrollment "Harshal" "PDP")sor1)
;;#true

;;Function Definition.
(define (not-dup? s l)
  (not(ormap
       ;;Any->Bool
       ;;Returns:True iff the new entry is not the duplicate one.
       (lambda (e)
         (and (contains?
               (roster-students e)
               (enrollment-student s))
              (equal? (roster-classname e)
                      (enrollment-class s)))) l)))

;;Tests:
(check-true (not-dup?  (make-enrollment "Harshal" "PDP")sor1)
            "Return true as no duplication is recorded")



;;add-list:enrollment SetofRoster->SetOfRoster.
;;Given:An enrollment and a list of rosters
;;Returns:An updated SetofRoster after adding the given list.
;;Strategy:Combine simpler function.

;;Examples:
;;(add-list (make-enrollment "Harshal" "PDP") sor)
;;(list
;; (make-roster "PDP" (list "Harshal"))
;; (make-roster
;;  "PDP"
;;  (list "Harsh" "Varun" "Rahul" "Mohit"))
;; (make-roster
;;  "Networks"
;;  (list "Harsh" "Varuni" "MOhit" "Salman")))

;;Function Definition.
(define (add-list s fl)
  (cons(make-roster (enrollment-class s) (cons (enrollment-student s) empty))
       fl))

;;Tests:
(begin-for-test
  (check equal?
         (enrollments-to-rosters soe1)
         (list
          (make-roster "Networks" (list "Sachin" "Megha"))
          (make-roster "PDP" (list "Varun" "Harsh")))
         "Should take a set of enrollments and return a set of roster."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;