;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-location "02" "fsm.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(provide initial-state
         next-state
         accepting-state?
         error-state?)
(provide initial-state
         next-state
         accepting-state?
         error-state?)

;;    Data Definition:
;;    A state is one of:
;;    ---q1
;;    ---q2
;;    ---q3
;;    ---q4
;;    ---q5
;;    ---error-state
;;    ---accepting-state

    (define q1 "initial-state will take any input fron
           'a','b'")
    (define q2 "will accept 'c' as the next state input")
    (define q3 "will accept 'a','b' as the next state input")
    (define q4 "will accept d as the next state input")
    (define q5 "will accept 'e','f' as the next state input")
    (define accepting-state "will accept 'd','e','f' as a valid input.")
    (define error-state "error state an illegal key was entered by the user.")
;;   template
;;   state-fn
;;   (define (state-fn a)
;;   (cond
;;   ...
;;   [(or (key=? ke "a")(key=? k "b"))
;;               ...]
;;   [(key=? ke "c")
;;          ...]
;;   [(key=? ke "d")
;;          ...]
;;   [(or (key=? ke "a")(key=? k "b"))
;;               ...]
;;              ))

;;   initial-state :Number-> State
;;   Given: A number or a symbol.
;;   Returns:An initialised Finite state machine.

;;   Examples:
;;   (initial-state "a" => "q1: The Finite state machine is initialised."
;;   Strategy :Functional Composition.


    (define (initial-state h)   
      "q1: The Finite state machine is initialised.")

;;   next-state : State KeyEvent ->State.
;;   Given: The present state of the machine and a KeyEvent.
;;   Returns: The state which must be acheived after following the key event.
;;   Examples:
;;  (next-state "q1" "a")=> "q1")
;;  (next-state "q1" "d")=> "error-state")

;;  Strategy :Apply Cases on KeyEvent.


    (define (next-state sta ke)
      (cond
     [( >(string-length ke) 1)"error-state"]
           
    [(or (key=? ke "a")(key=? ke "b"))
     (state-q1 ke sta)]
    [(key=? ke "c")
     (state-q2 ke sta)]
    [(key=? ke "d")
     (state-q4 ke sta)]
    [(or (key=? ke "e" )(key=? ke "f"))
     (state-q5 ke sta)]
    (else "error-state")))

;;   state-a :String String ->String.
;;   Given :A keyevent and the current state of finite state machine.

    (define (state-q1 key sta)
      (cond
        [(string=? sta "q1")"q1"]
        [(string=? sta "q2")"q3"]
        [(string=? sta "q3")"q3"]
        [(string=? sta "q4")" error-state"]
        [(string=? sta "q5")" error-state"]
        [(string=? sta "accepting-state")"error-state"]))
   
;;   state-a :String String ->String.
;;   Given :A keyevent and the current state of finite state machine.

    (define (state-q2 key sta)
      (cond
        [(string=? sta "q1")"q2"]
        [(string=? sta "q2")"error-state"]
        [(string=? sta "q3")"error-state"]
        [(string=? sta "q4" )"error-state"]
        [(string=? sta "q5")"error-state"]
  
  ))

;;   state-a :String String ->String.
;;   Given :A keyevent and the current state of finite state machine.


    (define (state-q3 key sta)
     (cond
     [(string=? sta "q1")"error-state"]
     [(string=? sta "q2")"error-state"]
     [(string=? sta "q3")"error-state"]
     [(string=? sta "q4")"accepting-state"]
     [(string=? sta "q5")"error-state"]))
    
;;Tests:

    (begin-for-test
      (check-equal? (state-q3 "a" "q1")"error-state")
      (check-equal? (state-q3 "a" "q2")"error-state")
      (check-equal? (state-q3 "a" "q3")"error-state")
      (check-equal? (state-q3 "a" "q4")"accepting-state")
      (check-equal? (state-q3 "a" "q5")"error-state")
      (check-equal? (state-q1  "a" "accepting-state" )"error-state"))
    

;;    state-a :String String ->String.
;;    Given :A keyevent and the current state of finite state machine.


    (define (state-q4 key sta)
      (cond
        [(string=? sta "q1") "error-state"]
        [(string=? sta "q2") "accepting-state"]
        [(string=? sta "q3") "accepting-state"]
        [(string=? sta "q4") "error-state"]
        [(string=? sta "q5") "error-state"]
        ))

;;   state-a :String String ->String.
;;   Given :A keyevent and the current state of finite state machine.


    (define (state-q5 key sta)
      (cond
        [(string=? sta "q1")"error-state"]
        [(string=? sta "q2")"error-state"]
        [(string=? sta "q3")"error-state"]
        [(string=? sta "q4")"accepting-state"]
        [(string=? sta "q5")"accepting-state"]
        ))


;;    accepting-state? : State -> Boolean
;;    GIVEN: a state of the machine
;;    RETURNS:if the given state is a accepting state returns true.
;;    EXAMPLES:
;;    (accepting-state? accepting-state)=>true
;;    (accepting-state? q1)=>false



    (define (accepting-state? sta)
      (if 
       (string=? sta accepting-state) true
       false))
    
;;   error-state? : State -> Boolean
;;   GIVEN: a state of the machine
;;   RETURNS: if the given state is a error state returns true.

;;   EXAMPLES:
;;   (error-state? error-state)=>true
;;   (error-state? q1)=>false

    (define (error-state? sta)
      (if
       (string=? sta error-state) true
       false
       ))
    
;;   Tests:

    (begin-for-test
      (check-equal? (next-state "q1" "a") "q1" 
                    "Current state must stay at q1")
      (check-equal? (next-state "q1" "b") "q1" 
                    "Current state must stay at q1")
      (check-equal? (next-state "q1" "c") "q2" 
                    "Current state must move to q2")
      (check-equal? (next-state "q1" "d") "error-state" 
                    "Current state must move to an error state")
      (check-equal? (next-state "q1" "e") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q1" "f") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q1" "f") "error-state" 
                    "Current state must move to error-state")
      
      (check-equal? (next-state "q1" "g") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q2" "a") "q3" 
                    "Current state must move to q3")
      (check-equal? (next-state "q2" "b") "q3" 
                    "Current state must move to q3")
      (check-equal? (next-state "q2" "c") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q2" "d") "accepting-state" 
                    "Current state must move to an accepting state")
      (check-equal? (next-state "q2" "e") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q2" "f") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q2" "g") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q3" "a") "q3" 
                    "Current state should move to q3")
      (check-equal? (next-state "q3" "b") "q3" 
                    "Current state must move to q3")
      (check-equal? (next-state "q3" "c") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q3" "d") "accepting-state" 
                    "Current state must move to an accepting state")
      (check-equal? (next-state "q3" "e") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q3" "f") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q3" "g") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q4" "a") " error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q4" "b") " error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q4" "c") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q4" "d") "error-state" 
                    "Current state must move to an error state")
      (check-equal? (next-state "q4" "e") "accepting-state" 
                    "Current state must move to accepting-state")
      (check-equal? (next-state "q4" "f") "accepting-state" 
                    "Current state must move to accepting-state")
      (check-equal? (next-state "q4" "g") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q5" "a") " error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q5" "b") " error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q5" "c") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (next-state "q5" "d") "error-state" 
                    "Current state must move to an error state")
      (check-equal? (next-state "q5" "e") "accepting-state" 
                    "Current state must move to accepting-state")
      (check-equal? (next-state "q5" "f") "accepting-state" 
                    "Current state must move to accepting-state")
      (check-equal? (next-state "q5" "g") "error-state" 
                    "Current state must move to error-state")
      (check-equal? (error-state? error-state)true
                    "Must return true")
      (check-equal? (error-state? q1)false
                    "Must return false")
      (check-equal? (accepting-state? q5)false
                    "Must return false")
      (check-equal? (accepting-state? accepting-state)true
                    "Must return true")
      (check-equal?(initial-state 10)
                   "q1: The Finite state machine is initialised.")
      check-equal? (next-state "q4 ""asd")"error-state")
    




         