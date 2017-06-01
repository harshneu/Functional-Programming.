;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-location "02" "coffee-machine.rkt")
(require rackunit)
(require "extras.rkt")

(provide initial-machine
         machine-next-state
         machine-output
         machine-remaining-coffee
         machine-remaining-hotchocolate
         machine-bank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Data Definition
;;  Whenever we give an input into the machine the change must be observed.

;;   A machinestate is a (make-machinestate Number Number Number Number)
;;   Interpretation:
;;   coffee is the number of cups of coffee present in the machine.
;;   hotchocolates is the number of cups of hotchoclate which are present
;;   in the machine.
;;   bank is the amount of money that is  stored in the machine. 
;;   change is the amount that must be given as change if a customer
;;   buys anything from the machine.

;;   Example:(machine-state [10 10 10 10])


     (define-struct machinestate [coffee hotchocolate bank change])


;;   A CustomerInput is one of
;;   -- a PosInt          interp: insert the specified amount of money, in cents
;;   -- "coffee"          interp: request a coffee
;;   -- "hot chocolate"   interp: request a hot chocolate
;;   -- "change"          interp: return all the unspent money that the
;;                                customer has inserted


;;   initial-machine : NonNegInt NonNegInt -> MachineState
;;   initial-machine gives the initial state of the machine.
;;   GIVEN:    a number of cups of coffee and of hot chocolate
;;   RETURNS:  the state of a machine loaded with the given number of cups
;;          of coffee and of hot chocolate, with an empty bank.

;;   Example:(initial-machine 0 1)(make-machinestate 0 1 0 0))


     (define (initial-machine coffee hotchocolate)
       (make-machinestate coffee hotchocolate 0 0))


;;    machine-next-state : MachineState CustomerInput -> MachineState
;;    GIVEN: a machine state and a customer input
;;    RETURNS: the state of the machine that should follow the customer's
;;    input
;;    machine-next-state should be used after some input has been given to
;;    our machine as it can be used as for the update processes.

;;    Example:(machine-next-state (initial-machine 0 1)
;;                       200) "coffee")"Out of Item")
;;        (machine-output (machine-next-state (initial-machine 10 1)
;;                                             200) "coffee")"coffee")


     (define (machine-next-state ms ci)
       (cond [(and (integer? ci) (> ci 0))
              (make-machinestate (machinestate-coffee ms)
                                 (machinestate-hotchocolate ms)
       (machinestate-bank ms) (+ (machinestate-change ms) ci))]
             [(equal? ci "coffee") (buy-coffee ms)]
             ;[(equal? ci "hot chocolate") (buy-hotchocolate ms)]
             [(equal? ci "change") (return-change ms)]
             [else (buy-hotchocolate ms)]))
     
 
;;buy-coffee :String->string
;;Given:An input for coffee
;;Returns:Substracts overall availablity of the coffee by 1
;;        and adds 150cents to the bank..
;;Example:buy-coffee (make-machinestate 1 0 0 0))(make-machinestate 0 0 0 0))

     (define (buy-coffee ms)
       (if (can-buy-coffee? ms)
           (make-machinestate (sub1 (machinestate-coffee ms))
                              (machinestate-hotchocolate ms)
                              (+ (machinestate-bank ms) 150)
                              (- (machinestate-change ms) 150))
           ms))

;;buy-hotchocolate :String->string
;;Given:An input for hotchocolate.
;;Returns:Substracts overall availablity of the hotchocolate by 1
;;        and adds 60cents to the bank..

;;Example:buy-hotchocolate (make-machinestate 1 1 0 0))
;;     (make-machinestate 1 0 60 0))

     (define (buy-hotchocolate ms)
       (if (can-buy-hotchocolate? ms)
           (make-machinestate (machinestate-coffee ms)
                              (sub1 (machinestate-hotchocolate ms))
                              (+ (machinestate-bank ms) 60)
                              (- (machinestate-change ms) 60))
           ms))

      
;;can-buy-coffee :String->boolean.
;;Given:An input for coffee.
;;Returns:True if their is some coffee left 
;;        in the machine.     .
     

     (define (can-buy-coffee? ms) (and (>= (machinestate-change ms) 150)
                                       (> (machinestate-coffee ms) 0)))


;;can-buy-hotchocolate :String->boolean.
;;Given:An input for hotchocolate.
;;Returns:True if their is some hotchocolate left 
;;        in the machine.


     (define (can-buy-hotchocolate? ms) (and (>= (machinestate-change ms) 60)
                                       (> (machinestate-hotchocolate ms) 0)))

;;return-change:Returns the change if an extra amount of money
;;is inserted in the system.

     (define (return-change ms)
       (make-machinestate (machinestate-coffee ms)
                      (machinestate-hotchocolate ms) (machinestate-bank ms) 0))


;;A MachineOutput is one of
;;-- "coffee"         interp: machine dispenses a cup of coffee
;;-- "hot chocolate"  interp: machine dispenses a cup of hot chocolate
;;-- "Out of Item"    interp: machine displays "Out of Item"
;;-- a PosInt         interp: machine releases the specified amount of
;;                            money, in cents
;;-- "Nothing"        interp: the machine does nothing
;;Example: (machine-output (machine-next-state (initial-machine 1 0) 200)
;;                                            "hot chocolate")"Out of Item"


(define (machine-output ms ci)
  (cond [(and (equal? ci "coffee") (>= (machinestate-change ms) 150))
         (if (> (machinestate-coffee ms) 0) "coffee" "Out of Item")]
        [(and (equal? ci "hot chocolate") (>= (machinestate-change ms) 60))
        (if (> (machinestate-hotchocolate ms) 0) "hot chocolate" "Out of Item")]
        [(and (equal? "change" ci) (> (machinestate-change ms) 0))
         (machinestate-change ms)]  
        [else "Nothing"]))


;;machine-remaining-coffee:
;;Given:machinestate
;;returns:The present state of the machine in regards of how many cups of
;;coffee is still left in the machine. 


(define (machine-remaining-coffee ms)
  (machinestate-coffee ms))
(begin-for-test
  (check-equal? (machine-remaining-coffee (initial-machine 1 1))1))


;;machine-bank:
;;Given:machinestate
;;returns:The present state of the machine in regards of how much money
;;is left in the machine.


(define (machine-bank ms)
  (machinestate-bank ms))
(define-struct CustomerInput [a])
(make-CustomerInput "CoffeeMachine")
(begin-for-test
 (check-equal? (make-CustomerInput "CoffeeMachine")
               (make-CustomerInput "CoffeeMachine")))


;;machine-remaining-hotchocolate:
;;Given:machinestate
;;returns:The present state of the machine in regards of how many cups of
;;hotchocolate is still left in the machine.


(define (machine-remaining-hotchocolate ms)
  (machinestate-hotchocolate ms))


;;Tests:
(begin-for-test
  (check-equal? (initial-machine 0 1)(make-machinestate 0 1 0 0))
  
  
  (check-equal? (machine-next-state (initial-machine 0 1 ) 200)
                (make-machinestate 0 1 0 200))
  
  
  
  (check-equal? (machine-output (machine-next-state (initial-machine 0 1)
                                             200) "coffee")"Out of Item")
  (check-equal? (machine-output (machine-next-state (initial-machine 10 1)
                                                   200) "coffee")"coffee")
  (check-equal? (machine-output (machine-next-state (initial-machine 10 1)
                                           200) "hotchocolates")"Nothing")
  (check-equal? (machine-output (machine-next-state (initial-machine 10 1)
                                     200) "hot chocolate")"hot chocolate")
  (check-equal? (machine-next-state (machine-next-state (initial-machine 1 1)
                        200)  "hotchocolate") (make-machinestate 1 0 60 140))
  
  
  
  (check-equal? (machine-next-state (machine-next-state (initial-machine 1 1)
                              200)  "coffee") (make-machinestate 0 1 150 50))
  (check-equal? (machine-next-state (machine-next-state (initial-machine 1 1)
                                 200)  "change") (make-machinestate 1 1 0 0))
  (check-equal? (machine-output (machine-next-state (initial-machine 10 1)
                                                    200) "change") 200) 
  (check-equal? (machine-output (machine-next-state (initial-machine 1 0) 200)
                                                "hot chocolate")"Out of Item")
  (check-equal?       (machine-remaining-hotchocolate (initial-machine 1 1))1)
  (check-equal?       (machine-bank (initial-machine 1 1))0)
  (check-equal?                     (machine-next-state (initial-machine 10 10)
                                    100) (make-machinestate 10 10 0 100))
  
  
  (check-equal? (machine-next-state (machine-next-state (initial-machine 10 10)
                         500)  "hotchocolate") (make-machinestate 10 9 60 440))
  (check-equal? (machine-next-state (machine-next-state (initial-machine 0 0)
                                 100)  "change") (make-machinestate 0 0 0 0))
  (check-equal? (machine-next-state (machine-next-state (initial-machine 10 10)
                               170)  "coffee") (make-machinestate 9 10 150 20))
  (check-equal? (machine-next-state (machine-next-state (initial-machine 0 0)
                         100)  "hotchocolate") (make-machinestate 0 0 0 100))
  (check-equal? (buy-coffee (make-machinestate 0 0 0 0))
                            (make-machinestate 0 0 0 0))
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;