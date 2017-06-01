#lang racket

(require rackunit)
(require "extras.rkt")
(provide
 (struct-out particle)
 (struct-out rect)
 particle-after-tick)

;; computing the geometry of a perfect bounce

;; a particle is bouncing inside a two-dimensional rectangle,
;; travelling at a constant velocity.

;; given its state at time t, compute its state at time t+1.

;; rules: if the particle would cross any side of the rectangle, at
;; time t+1 it appears on the side of the rectangle at the place it
;; would have hit the rectangle, and its velocity in the direction it
;; hits the wall is reversed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; data:

(define-struct particle (x y vx vy) #:transparent)  
(define-struct rect (xmin xmax ymin ymax) #:transparent)

;; all fields are Real.
;; We assume xmin < xmax, ymin < ymax.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; The particle is at (10 20 30 -40).  There is a wall at y = 0.
;; If there were no wall, the particle's next state would be
;; (50 -20 30 -30).  But there is a wall.  The particle reaches y=0
;; at t = 20/40 = 0.5.  So it hits the wall at x = 10 + 0.5*30 = 25,
;; and its y-velocity is reversed.  So the next state is (25 0 30 40),
;; with impact at t=0.5

;; If there were a wall at x=20, it would hit that wall first.
;; Similar arithmetic would apply, and the x-velocity would be
;; reversed. 

;; If there were a wall at x=25, the particle would hit exactly in the
;; corner, at (25,0), and both the x- and y-velocities would be
;; reversed.

;; How to manage multiple walls?  If the x-velocity is > 0, then the
;; ball is guaranteed to hit the xmax wall if it travels long enough.
;; If the x-velocity is < 0, the particle is guaranteed to hit the
;; xmin wall if it travels long enough (and it will never hit the xmax
;; wall). 

;; So we can figure out when the ball will hit an x-wall, and when it
;; will hit a y-wall.

;; Choose the wall it hits first.  If there's a tie, that means the
;; particle will hit in a corner, so it doesn't matter which one we
;; choose. 

;; Find the time of the first collision (or 1 if none), and return the
;; state of the ball at time t.  If the ball is at a wall, reverse the
;; velocity in the direction of the wall.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS:

;; particle-after-tick : Particle Rectangle -> Particle
;; GIVEN: a particle inside a rectangle
;; RETURNS: the state of the particle after the next tick.
;; STRATEGY: cases on whether the particle is at rest.  If it is not
;; at rest, it is guaranteed to collide with a wall eventually.
;; Calculate the time t of the next collision (or 1 if the particle
;; doesn't hit a wall within the next tick), and return its state at
;; time t.

(define (particle-after-tick p r)
  (if (at-rest? p) p
      (let ((t (min 1 (time-of-next-collision p r))))
        (particle-after-time p r t))))

;; time-of-next-collision : Particle Rectangle -> Time
;; WHERE: the particle is in the rectangle and has some non-zero velocity
;; RETURNS: the time at which the particle will first hit any wall.
;; Strategy:Use HOF filter on particle.  
(define (time-of-next-collision p r)
  (let ((px (particle-x p))
        (py (particle-y p))
        (vx (particle-vx p))
        (vy (particle-vy p))
        (xmin (rect-xmin r))
        (xmax (rect-xmax r))
        (ymin (rect-ymin r))
        (ymax (rect-ymax r)))
    ;; take the minimum of the positive times
    (list12-min
     (filter 
      (lambda (t) (> t 0))
      (list
       ;; find next collision with an x boundary. This is where we
       ;; use the assumption that xmin <= x <= xmax.  Checking the
       ;; sign of vx tells us which wall the particle is travelling
       ;; towards. 
       (cond
         [(> vx 0) (/ (- xmax px) vx)]
         [(< vx 0) (/ (- xmin px) vx)]
         ;; if vx=0 you can't have an x-collision, return a 0 which
         ;; will be filtered out
         [else 0])    
       ;; similarly find next collision with a y boundary.
       (cond
         [(> vy 0) (/ (- ymax py) vy)]
         [(< vy 0) (/ (- ymin py) vy)]
         ;; if vy=0 you can't have an y-collision, return a 0 which
         ;; will be filtered out
         [else 0]))))))

;; at-rest?:Particle ->Boolean.
;; Returns: Returns true iff the particle velocities are zero.
;;          else false.
;;Strategy: Use simpler functions.
;;Example:  We pass a particle as an argument and check whether the particle
;;          velocities are zero or not.

(define (at-rest? p)
  (and (zero? (particle-vx p)) (zero? (particle-vy p))))

;; listl2-in: list->list
;; GIVEN: A list of either 1 or 2 PosReals,
;; Returns: return the smallest of the posreals.
;; Strategy: Use template on lists.
;;Example: We pass arguments which must be positive ;the smallest amongst them
;;         is returned.

(define (list12-min lst)         
  (if (null? (rest lst))
      (first lst)
      (min (first lst) (second lst))))


;; particle-after-time : Particle Rectangle Time -> Particle
;; WHERE: t is either 1 of the time of next collision, whichever is
;; smaller. 
;; RETURNS: the state of the particle after time t.
;; If the particle is at a wall, it reverses its velocity in the
;; dimension of the wall.  (Or both, if it's at a corner).
;; Strategy:Combine simpler functions.

(define (particle-after-time p r t)
  (let ((px (particle-x p))
        (py (particle-y p))
        (vx (particle-vx p))
        (vy (particle-vy p))
        (xmin (rect-xmin r))
        (xmax (rect-xmax r))
        (ymin (rect-ymin r))
        (ymax (rect-ymax r)))
    (let ((next-x (+ px (* vx t)))
          (next-y (+ py (* vy t))))
      (make-particle
       next-x
       next-y
       (maybe-reverse-velocity next-x vx xmin xmax)
       (maybe-reverse-velocity next-y vy ymin ymax)))))

;; may-be-reverse-velocity :Int Int Int Int
;; Given: The velocity and the maximum and minimum coordinates of the system.
;; Effect: Will interchange the velocities if the maximum and minimum velocities
;;         are reached.

(define (maybe-reverse-velocity x vx xmin xmax)
  (if (or
       (= x xmin)
       (= x xmax))
      (- vx)
      vx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TESTS

(define particle1 (make-particle 10 20 30 -40))

(define rect1 (make-rect 0 100 0 150))
(define rect2 (make-rect 0  20 0 150))
(define rect3 (make-rect 0  25 0 150))
(begin-for-test
  ;; particle bounces off y = 0
  (check-equal?
   (particle-after-tick particle1 rect1) 
   (make-particle 25 0 30 40))
  
  ;; particle bounces off x = 20
  (check-equal?
   (particle-after-tick particle1 rect2)
   (make-particle 20 (- 20 40/3) -30 -40))
  
  ;; the particle bounces off y = 0 at the corner x = 25
  (check-equal?
   (particle-after-tick particle1 rect3)
   (make-particle 25 0 -30 40)))




