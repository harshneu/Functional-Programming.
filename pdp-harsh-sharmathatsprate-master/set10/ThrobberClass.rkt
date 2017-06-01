#lang racket                     ;;Used here as to determine language from
                                 ;;source.

(require rackunit)               ;;In built module for testing framework.
(require "extras.rkt")           ;;A file inported.
(require "WidgetWorks.rkt")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; ThrobberToy Class
;;;A throbber toy is a (new ThrobberToy% [x Int] [y Int] [count Int] [flag Int])
;;;A throbber represents a circle whose radius and colour changes
;;;whenever rendered with tick.
;;; A Throbber is a (new Throbber% [x Integer] [y Integer] [radius PosInt]
;;;                  [selected? Boolean] [saved-mx PosInt] [saved-my PosInt])
;;; A Throbber represents a throbber toy.
;;; in this version, the toy initially increased its size until radius 20 and
;;; then deccreases its radius till 5 and goes on.
;;; Throbber is also selectable and draggable
;
(define Throbber%
  (class* object%(Toy<%>)
    (init-field x)
    (init-field y)                  ;;Throbbers x position.
    (init-field radius)             ;;Throbbers y position.
    (init-field flag)               ;;flag event for changing the
                                    ;;orientation of throbbers expansion.
    (field [EXPAND 1])
    (field [CONTRACT -1])
    (field [CIRCLE-RADIUS 5])
    (field [MAX-RADIUS 20]) 
    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;after-tick: ->Toy<%>
    ;;RETURNS :A throbber like the given one but the radius increases and
    ;;deacreases with every tick. 
    ;;Strategy:Use cases on throbber
    
    ;;Strategy:Communication via state.
    (define/public (after-tick)
      (cond
        [(= radius CIRCLE-RADIUS)
         (set! flag EXPAND) 
         (set! radius (+ radius flag))]
        [(= radius MAX-RADIUS)
         (set! flag CONTRACT)
         (set! radius (+ radius flag))]
        [else
          (set! radius (+ radius flag))]))
         
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after mouse and key event
    (define/public (after-button-down mx my) this) 
    (define/public (after-drag mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-key-event kev) this)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;add-to-scene :Scene->Scene
    ;;Returns:A scene with the throbber rendered on it.
    (define/public (add-to-scene scene)
      (place-image (circle radius "solid" "cornflowerblue")
                   x y scene))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    ;;toy-x : ->PosInt
    ;;RETURNS :The x coordinate of the throbber.
    (define/public (toy-x)x)
    
    ;;toy-y: ->PosInt.
    ;;RETURNS:The y coordinate of the throbber.
    (define/public (toy-y) y)
    
    ;;toy-data ->PosInt
    ;;RETURNS:The radius of the throbber as stated in the problem set.
    (define/public (toy-data)radius)      
    
   
    ;;for-test:toy-equal?: -> Boolean
    ;;RETURNS: true iff the two FootballToys have the same x and y co-ordinates
    ;;         and the same size 
    ;;EXAMPLES: as covered in test cases
    (define/public (for-test:toy-equal? th)
      (and
       (=
        (send th toy-x)
        (send this toy-x))
       (=
        (send th toy-y)
        (send this toy-y))
       (=
        (send th toy-data)
        (send this toy-data))))
    ))
