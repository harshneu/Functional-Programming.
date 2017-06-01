;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(check-location "02" "probe.rkt")
(require rackunit)
(require "extras.rkt")

(provide
  probe-at
  probe-left
  probe-right
  probe-forward
  probe-north?
  probe-south?
  probe-east?
  probe-west?)
 


;;   In this Code we assume a probe landing on the trap in pluto and a establish
;;   a relation between using functions such that the probe stays in the trap.
;;   A probe is a (make-probe Integer Integer String)
;;   Interp:
;;   x and y are the x and y coordinates of the trap used by the plutonians.

   (define-struct probe (x y direction))


;;  (define (probe-fb r)
;;  (...(probr-x r)
;;    (probe-y r)
;;     (probe-direction r))

;;   initial-probe : Number Number Real Real-> Probe
;;   GIVEN: a set of (x,y) coordinates
;;   RETURNS: a probe with its center at those coordinates, facing north (up)
;;          the starting state of this problem is in particular stated to
;;          be the origin i.e. the points (0,0) in the coordinate system.
;;   The probe here faces "North"


   (define (probe-at x y) (make-probe x y "North"))

;;   Examples:(probe-at 0 0) (make-probe 0 0 "North")
;;   Test

   (begin-for-test
     (check-equal? (probe-at 0 0) (make-probe 0 0 "North")))
                 
;;   The conditions here are used not explicitely but intrisicly in the code
;;   such that they are used to determine that whether or not a probe faces
;;   the specified direction.
;    probe-north? : probe -> Boolean
;    probe-south? : probe -> Boolean
;    probe-east? :  probe -> Boolean
;    probe-west? : probe -> Boolean
;    GIVEN: a probe
;    Returns: whether the probe is facing in the specified direction.

   (define (probe-north? a) (string=? (probe-direction a) "North"))
   (define (probe-west? a)  (string=? (probe-direction a) "West"))
   (define (probe-south? a) (string=? (probe-direction a) "South"))
   (define (probe-east? a)  (string=? (probe-direction a) "East"))

;;   Examples:(probe-north? (probe-at 50 49)) #t)
;;        (probe-south? (probe-at 11 -50)) #f)
;    Test
;;   The test case here verifies about the direction
;;   Returns:(true) if the specified direction is same as the assumed direction



   (begin-for-test
     (check-equal? (probe-north? (probe-at 50 49)) #t)
     (check-equal? (probe-south? (probe-at 11 -50)) #f)
     )
   
;   probe-left : Probe -> Probe
;   probe-right : Probe -> Probe
;   GIVEN: A Probe.
;   RETURNS: a Probe in the original state as it was ,
;;  but turned 90 degrees either left
;   or right as specified while calling the function.
;;  The direction that the probe returns are in accordance to
;;  the present state of the probe.

   (define (probe-left a) (cond
                            [(probe-north? a) (probe-turn a "West")]
                            [(probe-west? a)  (probe-turn a "South")]
                            [(probe-south? a) (probe-turn a "East")]
                            [(probe-east? a)  (probe-turn a "North")])) 
   
   
   (define (probe-right a) (cond
                             [(probe-north? a) (probe-turn a "East")]
                             [(probe-west? a)  (probe-turn a "North")]
                             [(probe-south? a) (probe-turn a "West")]
                             [(probe-east? a)  (probe-turn a "South")]))
   
;;   Examples: (probe-left (probe-at 0 0)) (make-probe 0 0 "West"))
;;         (probe-left (probe-left (probe-left(probe-at 20 25 ))))
;;                                        (make-probe 20 25 "East"))
;;         (probe-left (probe-right (probe-at 100 42 )))
;;                            (make-probe 100 42 "North"))
;;         (probe-left (probe-at 20 25 )) (make-probe 20 25 "West"))



;  Tests
;; These tests are about does the probe behaves in the desired way when an
;; operation of maove left and right are assigned to it  by the user.

   (begin-for-test
     (check-equal? (probe-left (probe-at 0 0)) (make-probe 0 0 "West"))
     (check-equal? (probe-left (probe-left (probe-left(probe-at 20 25 ))))
                                                (make-probe 20 25 "East"))
     (check-equal? (probe-left (probe-right (probe-at 100 42 )))
                                    (make-probe 100 42 "North"))
     (check-equal? (probe-left (probe-at 20 25 ))
                       (make-probe 20 25 "West"))
     
     (check-equal? (probe-right (probe-at 00 11)) (make-probe 00 11 "East"))
     (check-equal? (probe-right(probe-right (probe-at 00 11)))
                                   (make-probe 00 11 "South"))
     (check-equal? (probe-right(probe-left (probe-left (probe-at 1 4))))
                                                (make-probe 1 4 "West"))
     (check-equal? (probe-right (probe-left (probe-at 5 48)))
                                   (make-probe 5 48 "North"))
     )
;;  probe-forward :Probe PosInt -> Probe.
;;  GIVEN: a specified distance and the probe.
;;  RETURNS: a Probe as given in the question after it moves forward by the
;;         speified number of requested distance(multiple of 1cmin this case).
;;  Probe moves forward the specified
;;  requested  distance which causes the probe to move from being
;;  at the origin of the trap to being at some location in the trap.
;;  the probe stops at the end of the trap wall.





   (define (probe-forward a d) (cond
                                 [(probe-north? a) (move-north a d)]
                                 [(probe-south? a) (move-south a d)]
                                 [(probe-east? a)  (move-east a d)]
                                 [(probe-west? a)  (move-west a d)]))
   
   
   (define (move-north a d)
     (if (> (- (probe-y a) d) -153)
         (make-probe (probe-x a) (- (probe-y a) d) (probe-direction a))
         (make-probe (probe-x a) -153 (probe-direction a))))
   
   
   (define (move-south a d)
     (if (< (+ (probe-y a) d) 153)
         (make-probe (probe-x a) (+ (probe-y a) d) (probe-direction a))
         (make-probe (probe-x a) 153 (probe-direction a))))
   
   
   (define (move-east a d);;.
     (if (< (+ (probe-x a) d) 153)
         (make-probe (+ (probe-x a) d) (probe-y a) "East")
         (make-probe 153 (probe-y a) "East")))
   
   
   (define (move-west a d)
     (if (> (- (probe-x a) d) -153)
         (make-probe (- (probe-x a) d) (probe-y a) (probe-direction a))
         (make-probe -153 (probe-y a) (probe-direction a))))
   
;;  Examples: (probe-at 0 -153)(make-probe 0 -153 "North"))
;;         (probe-at 0 153)(make-probe 0 153 "North"))
;;         (probe-at 0 0)(make-probe 0 0 "North"))
;;         (probe-left (probe-forward r1 200))(make-probe 0 -153 "West"))
;;         (probe-left (probe-forward r1 153))(make-probe 0 -153 "West"))
;;         (probe-right (probe-forward r1 20))(make-probe 0 -20 "East"))
;;         (probe-forward (make-probe 0 0 "East")200)(make-probe 153 0 "East"))
;;         (probe-forward (make-probe 0 0 "West")200)(make-probe -153 0 "West"))
;;         (probe-forward (make-probe 0 0 "South")20)(make-probe 0 20 "South"))
;;         (probe-forward (make-probe 0 0 "North")2)(make-probe  0 -2 "North"))


;;  probe-turn : probe direction->probe.
;;  Given :a probe and direction where probe turn to.
;;  Returns : A probe at the same coordinates facing a specified direction.
;;  Examples: (probe-left (probe-forward r1 200))(make-probe 0 -153 "West")


   (define (probe-turn r direction) 
     (make-probe (probe-x r) (probe-y r) direction))
   
   
   (define r1 (make-probe 0 0 "North"))
   
   (begin-for-test
     (check-equal? (probe-at 0 -153)(make-probe 0 -153 "North"))
     (check-equal? (probe-at 0 153)(make-probe 0 153 "North"))
     (check-equal? (probe-at 0 0)(make-probe 0 0 "North"))
     (check-equal? (probe-left (probe-forward r1 200))
                           (make-probe 0 -153 "West"))
     (check-equal? (probe-left (probe-forward r1 153))
                           (make-probe 0 -153 "West"))
     (check-equal? (probe-right (probe-forward r1 20))
                            (make-probe 0 -20 "East"))
     (check-equal? (probe-forward (make-probe 0 0 "East")200)
                                   (make-probe 153 0 "East"))
     (check-equal? (probe-forward (make-probe 0 0 "West")200)
                                  (make-probe -153 0 "West"))
     (check-equal? (probe-forward (make-probe 0 0 "South")200)
                                  (make-probe  0 153 "South"))
     (check-equal? (probe-forward (make-probe 0 0 "North")200)
                                 (make-probe  0 -153 "North"))
     (check-equal? (move-west (probe-forward (make-probe 0 0 "East") 200)10)
                                             (make-probe 143 0 "East"))
     (check-equal? (move-east (probe-forward (make-probe 0 0 "East") 20)10)
                                             (make-probe 30 0 "East"))
     (check-equal? (move-south (probe-forward (make-probe 0 0 "South") 100)30)
                                              (make-probe 0 130 "South")))