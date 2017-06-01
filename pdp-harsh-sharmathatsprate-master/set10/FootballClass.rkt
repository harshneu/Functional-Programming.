#lang racket                     ;;Used here as to determine language from
                                 ;;source.

(require rackunit)               ;;In built module for testing framework.

(require 2htdp/universe)         ;;A racket module for using big bang functions.
(require 2htdp/image)            ;;A racket module for using image functions.
(check-location "10" "toys.rkt") ;;To check location of file.

(require "World.rkt")

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;FootballToy Class
;;;A football toy is a (new FootballToy% [x Int] [y Int] [scale-count PosReal])
;;;Interpretation:
;;;A football represents the football which deacreases in size with tick.
(define Football% 
  (class* object% (Toy<%>) 
    
    (init-field x)  ;; x-co-ordinate of the center of football
    (init-field y)  ;; y-co-ordinate of the center of football
    (init-field scale-count) ;; to scale the football image 
    
       
    (super-new)
    
    ;;after-tick: ->Toy<%>
    ;;RETURNS: the toy that should follow this one after a tick
    ;;EXAMPLES: as covered in test cases      
    (define/public (after-tick)
      (cond 
        [ (> (- scale-count 0.05) 0.01) 
          (set! scale-count (- scale-count  0.01))]
        [else (set! scale-count 0.001)]))
    
    ;; the mouse event and key event
    (define/public (after-button-down mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-key-event kev) this)
    
    ;;add-to-scene: Scene->Scene
    ;;GIVEN: a Scene
    ;;RETURNS: a scene like the given one, but with the given toy
    ;;         drawn on it
    ;;EXAMPLES: as covered in test cases
    (define/public (add-to-scene s)
      (place-image (scale  scale-count (bitmap "football.jpg")) x y s))   
    
    ;;(toy-x toy-y): ->Integer 
    ;;RETURNS: the x and y coordinates of the center of the toy.
    (define/public (toy-x)x)
    (define/public (toy-y)y)
    
    ;; toy-data: -> PosReal
    ;; RETURNS: the current size of the football toy.    
    (define/public (toy-data)scale-count)      
    
    ;;for-test:toy-equal?: -> Boolean
    ;;RETURNS: true iff the two FootballToys have the same x and y co-ordinates
    ;;         and the same size 
    ;;EXAMPLES: as covered in test cases
    (define/public (for-test:toy-equal? f)
      (and
       (=
        (send f toy-x)
        (send this toy-x))
       (=
        (send f toy-y)
        (send this toy-y))
       (=
        (send f toy-data)
        (send this toy-data))))
    ))       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; ClockToy Class

;;;A clock toy is a (new Clock% [x Int] [y Int] [ticks Int])
;;;INTERPRETATION:
;;; Clock represents a clock which measures the count after it is created.
(define Clock%
  (class* object%(Toy<%>)
    (init-field x)       ;; represents the x-cordinate of the clock
    (init-field y)       ;; represents the y-cordinate of the clock
    (init-field ticks)   ;; to increase the count of the number of
                         ;; ticks initially is one
   
    (field [CLOCK-COLOR "violetred"]) ;; colour of the clock
    (field [SIZE 15])  ;; represents the size of the clock 
    
    (super-new)
    
    ;;after-tick: ->Toy<%>
    ;;RETURNS: the toy that should follow this after a tick.
    ;;DETAILS: the tick field is continuously incremented to represent
    ;;         the number of ticks passed since it was created
    ;;EXAMPLES: as covered in test cases
    ;;Strategy:Communication via state.

    (define/public (after-tick)
      (set! ticks (+ ticks 1)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; after mouse and key event
    (define/public (after-button-down mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-key-event kev) this)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;add-to-scene: Scene->Scene
    ;;GIVEN: a scene
    ;;RETURNS: a scene like the given one but with this toy drawn on it.
    (define/public (add-to-scene s)
      (place-image (overlay(pulled-regular-polygon 20 8 1/4 10 "outline" "red") 
                       (text (number->string ticks) SIZE CLOCK-COLOR))  x y s))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;toy-x: ->Integer
    ;;toy-y: ->Integer
    ;;RETURNS:The x and y coordinates of the target.
    (define/public (toy-x)
      x)
    (define/public (toy-y)
      y)
    
    ;;toy-data: -> Integer
    ;;RETURNS: the number of ticks since the clock was created    
    (define/public (toy-data)ticks)
        
    ;;for-test:toy-equal?: -> Boolean
    ;;RETURNS: true iff the two Clocks have the same ticks
    ;;         and the same x and y co-ordinates
    ;;EXAMPLES: as covered in test cases
    (define/public (for-test:toy-equal? c)
      (and
       (=
        (send c toy-x)
        (send this toy-x))
       (=
        (send c toy-y)
        (send this toy-y))
       (=
        (send c toy-data)
        (send this toy-data))))
    ))      
