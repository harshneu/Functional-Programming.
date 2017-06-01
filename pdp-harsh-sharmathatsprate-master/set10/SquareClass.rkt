#lang racket                     ;;Used here as to determine language from
                                 ;;source.

(require rackunit)               ;;In built module for testing framework.
(require "World.rkt")
(require 2htdp/universe)         ;;A racket module for using big bang functions.
(require 2htdp/image)            ;;A racket module for using image functions.

(provide Square%)
;; CONSTANTS used in the program.: 

;;Constants for the World:
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define RIGHT-BOUNDARY 480)
(define BOTTOM-BOUNDARY 580)
(define EMPTY-SCENE (empty-scene 500 600))
(define TARGET-X 250)                      ;;initial x-coordinate of the target.
(define TARGET-Y 300)                      ;;initial y-coordinate of the target.

;;Constants for Key-event:
(define SQUARE-KE "s")
(define FOOTBALL-KE "f")
(define THROBBER-KE "t")
(define CLOCK-KE "w")
(define INVALID-KE " ") 

;;Constants for Mouse:
(define BUTTON-DOWN-MOUSE-EVENT "button-down")
(define BUTTON-UP-MOUSE-EVENT "button-up")
(define DRAG-MOUSE-EVENT "drag")
(define INVALID-MOUSE-EVENT "leave")

;;Constants for MousePosition of Toy:
(define MX-INITIAL -1)
(define MY-INITIAL -1)

;; Inital Value of Scale Count for Football
(define INITIAL-COUNT 1)

;;Constants for Colours:
(define SQUARE-COLOR "violet")
(define THROBBER-COLOR "green")

;;Constants for Direction:
(define RIGHT "right")
(define LEFT "left")

;;Constant for FootBallToy:
(define FOOTBALL "football.jpg")
(define FOOTBALL1 (bitmap "football.jpg"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-square-toy : PosInt PosInt PosInt -> Toy<%>
;;GIVEN: an x and a y position, and a speed
;;RETURNS: an object representing a square toy at the given position,
;;travelling right at the given speed.
;;EXAMPLE:
;; (make-square-toy 10 10 1)=>
;;(object:SquareToy% ...)
(define (make-square-toy tx ty speed)
  (new Square% [x tx] [y ty]
       [speed speed][direction RIGHT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-throbber: PosInt PosInt -> Toy<%>
;;GIVEN: an x and a y position
;;RETURNS: an object representing a throbber at the given position.
;;EXAMPLE:
;;(make-throbber 10 10 )
;;(object:Throbber% ...)
(define (make-throbber x y)
  (new Throbber% [x x][y y]
       [radius 5][flag 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;make-football : PosInt PostInt -> Toy<%>
;;;GIVEN: an x and a y position
;;;RETURNS: an object representing a clock at the given position.
;;;EXAMPLE:
;;;(make-football 10 10 )
;;;(object:Football% ...)
(define (make-football tx ty)
  (new Football% [x tx][y ty]
       [scale-count INITIAL-COUNT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;make-clock : PosInt PostInt -> Toy<%>
;;;GIVEN: an x and a y position
;;;RETURNS: an object representing a clock at the given position.
;;;EXAMPLE:
;;;(make-clock 10 10)
;;;(object:Clock% ...)
(define (make-clock tx ty)
  (new Clock% [x tx][y ty]
      [ticks 1]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SquareToy Class

;; A SquareToy is a (new SquareToy% [x Int] [y Int] [speed Int]
;;                                    [direction Direction])
;; INTERPRETATION:
;; x and y represents the centre co-ordinate of the toy
;; speed is the speed at which the square toy is moving
;; direction is the direction in which the square toy is moving
;; it can be "left" or "right"
(define Square%
  (class* object%(Toy<%>)
    (init-field x )      ;; interp : x-cordinate of
                         ;; the center of the toy                          
    (init-field y)       ;; interp : y-cordinate of
                         ;; the center of the toy
   
    
    (init-field speed)   ;; the speed at which the
                         ;; toy moves
    
    (init-field direction)  ;; the direction in which toy moves
                           ;; i.e either left or right

    
    ;;Private field associated with the square.
    (field [SQUARE-SIDE 40])
    (field [HALF-SQUARE-SIDE ( / SQUARE-SIDE 2)])
    (field [SQUARE-ICON (square SQUARE-SIDE "outline" "darkred")])
    (field [CANVAS-RIGHT-END (- CANVAS-WIDTH HALF-SQUARE-SIDE)])
    (field [CANVAS-LEFT-END HALF-SQUARE-SIDE])
    
    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;after-tick: ->Toy<%>
    ;;RETURNS: returns the toy that should follow this one after tick
    ;;DETAILS: if on the next tick the square touches the edge of the
    ;;         canvas, then it should start moving in the opposite direction
    ;;         otherwise it should keep on moving in the same direction
    ;;EXAMPLE: as covered in test cases
    ;;STRATEGY: cases on direction
    (define/public (after-tick)
      (cond
        [(string=? direction RIGHT)
         (send this right-transition)]
        [(string=? direction LEFT)
         (send this left-transition)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;right-transition: Int Int speed->Toy<%>
    ;;GIVEN: the x and y coordinates and the speed for transition.
    ;;RETURNS: the SquareToy that should follow this one after a tick
    ;;DETAILS: if on the next tick the square touches the edge of the
    ;;         canvas, then it should start moving in the opposite direction
    ;;         otherwise it should keep on moving in the same direction
    ;;EXAMPLE: as covered in test cases
    ;;Strategy:Communication via state.
    (define/public (right-transition)
       (cond
        [(< (+ x speed) CANVAS-RIGHT-END)
         (set! x (+ x speed))]
        [(>= (+ x speed) CANVAS-RIGHT-END)
         (set! x CANVAS-RIGHT-END)
         (set! direction LEFT)]))  
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;left-transition: Int Int speed-><Toy%>
    ;;GIVEN: the x and y coordinates and the speed for transition.
    ;;RETURNS: the Square that should follow this after a tick
    ;;DETAILS: if on the next tick the square touches the edge of the
    ;;         canvas, then it should start moving in the opposite direction
    ;;         otherwise it should keep on moving in the same direction
    ;;EXAMPLE: as covered in test cases
    ;;Strategy:Communication via state.
    (define/public (left-transition)
       (cond
        [(> (- x speed) CANVAS-LEFT-END)
         (set! x (- x speed))]
        [(<= (- x speed) CANVAS-LEFT-END)
         (set! x CANVAS-LEFT-END)
         (set! direction RIGHT)]))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after mouse and key event
    (define/public (after-button-down mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-key-event kev) this)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;add-to-scene: Scene->Scene
    ;;GIVEN: a scene
    ;;RETURNS: a scene like the given one, but with this toy drawn on it
    ;;EXAMPLES: as covered in test cases
    (define/public (add-to-scene s)
      (place-image SQUARE-ICON x y s))
        
    ;;toy-x: ->Integer
    ;;toy-y: ->Integer
    ;;RETURNS: x and y coordinates of the centre of the toy.
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;;toy-data: -> Integer
    ;;RETURNS: the speed of the SquareToy    
    (define/public (toy-data) speed)
      
        
    ;;for-test:get-toy-direction: -> String
    ;;RETURNS: the direction of the SquareToy 
    ;;EXAMPLES: as covered in test cases
    (define/public (for-test:get-toy-direction)
      direction)     
    
    ;;for-test:toy-equal?: -> Boolean
    ;;RETURNS: true iff the two SquareToys have the same x,y co-ordinates,speed
    ;;         and direction
    ;;EXAMPLES: as covered in test cases
    (define/public (for-test:toy-equal? sq1)
      (and
       (=
        (send sq1 toy-x)
        (send this toy-x))
       (=
        (send sq1 toy-y)
        (send this toy-y))
       (string=?
        (send sq1 for-test:get-toy-direction)
        (send this for-test:get-toy-direction))
       (=
        (send sq1 toy-data)
        (send this toy-data))))))     