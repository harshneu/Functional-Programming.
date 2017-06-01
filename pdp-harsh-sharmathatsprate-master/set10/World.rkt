#lang racket                     ;;Used here as to determine language from
                                 ;;source.

(require rackunit)               ;;In built module for testing framework.
(require "extras.rkt")           ;;A file inported.
(require "WidgetWorks.rkt")
(require 2htdp/universe)         ;;A racket module for using big bang functions.
(require 2htdp/image)            ;;A racket module for using image functions.
(check-location "10" "toys.rkt") ;;To check location of file.

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


(define WorldState% 
  (class* object% (PlaygroundState<%>)
    (init-field x) ;; The x-cordinate of target
    (init-field y)  ;; The y-coordinate of target
    (init-field toys)   ;;The list of toys on the scene.
    (init-field speed)  ;;The speed of our square toy.
    (init-field mouse-x) ;; x coordinate of mouse down event
    (init-field mouse-y) ;; y coordinate of mouse down event
    (init-field selected?) ;; determines wheter the target is selected?
    
    ;; private field for target
    (field [ICON(circle 10 "outline" "goldenrod")])
    (field [TARGET-RADIUS 10])
    
    
    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;after-tick: ->WorldState
    ;;RETURNS:A world after the tick.
    ;;EXAMPLES:as covered in test cases
    (define/public (after-tick)
      (begin
        (for-each
                  ;; Toy -> Toy
                  ;; GIVEN: a Toy object
                  ;; RETURNS: a Toy after a tick 
                  (lambda(toy)(send toy after-tick))
                  toys)))           
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     
    ;; after-button-down : Integer Integer -> World
    ;; GIVEN : the location of a mouse event
    ;; RETURNS : the WorldState that should follow this one after
    ;; the given button down at the given location
    ;; DETAILS:  If the event is inside the target, returns a worldstate just
    ;;           like original worldstate except that the target now is selected
    ;;           and the mx and my are updated to the position where button-down
    ;;           event happened. Otherwise returns the worldstate unchanged.
    ;; EXAMPLES : as covered in test cases    
    (define/public (after-button-down mx my)
       (if (send this in-target? mx my)
           (update-target-button-down mx my)
           this))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Integer Integer -> Void
    ;; EFFECTS: updates this world by changing the mouse-x and mouse-y to
    ;; new mouse positions and updating selected to true
    ;; GIVEN: Mouse co-ordinates
    ;; EXAMPLES: see tests
    ;; Strategy:Communication via state.
    
    (define/public (update-target-button-down mx my)
      (set! mouse-x mx)
      (set! mouse-y my)
      (set! selected? true))
           
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

    ;; after-drag :  Integer Integer -> World
    ;; GIVEN: the location of a mouse event
    ;; RETURNS: the WorldState that should follow this one after a drag at
    ;; the given location 
    ;; DETAILS: if the target in the worldstate is selected, move the target by 
    ;;          smooth dragging to a new location, otherwise ignore.
    ;; EXAMPLES: as covered in test cases
    (define/public (after-drag mx my)
      (if selected?
      (update-target-drag mx my)
      this))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; update-target-drag Integer Integer -> Void
    ;; EFFECTS: updates the target in this world to new positions calculated by
    ;; relative movement of the mouse positions
    ;; GIVEN: the mouse positions
    ;; EXAMPLES: see tests
    ;;Strategy:Communication via state.
    (define/public (update-target-drag mx my)
      (set! x (- mx (- mouse-x x)))
      (set! y (- my (- mouse-y y))) 
      (set! mouse-x mx)
      (set! mouse-y my))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; world-after-button-up : -> World
    ;; RETURNS: the world that should follow this one after a button-up
    ;; DETAILS: button-up unselects the target
    ;; EXAMPLES: as covered in test cases
    ;;Strategy:Communication via state.
    (define/public (after-button-up mx my)
      (set! selected? false)) 
      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

    ;;in-target?:Int Int->Boolean
    ;;GIVEN: the mouse coordinates.
    ;;RETURNS: a boolean  which shall be used for the dragging execution
    ;;Strategy :Use simple functions.
    
    (define/public (in-target? mx my)
      (<=
       (+ (sqr (- x mx))
          (sqr (- y my)))
       (sqr TARGET-RADIUS)))
     
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
    
    ;;after-key-event: KeyEvent-> WorldState
    ;;GIVEN:a keyevent to the world.
    ;;RETURNS:a world after the key event.
    ;;EXAMPLES:as covered in test cases
    ;;STRATEGY: cases on KeyEvent
    (define/public (after-key-event ke)
      (cond
        ;; The Square Key event.
        [(key=? ke "s")         
              (set! toys (cons (make-square-toy x y speed) toys))]              
        
        ;;The Throbber Key event.
        [(key=? ke "t")
             (set! toys (cons (make-throbber x y) toys))]             
             
        ;;The clock key event.
        [(key=? ke "w")
           (set! toys (cons (make-clock x y) toys))]            
              
        
        ;;The Football key event.
        [(key=? ke "f")
             (set! toys (cons (make-football x y) toys))] 
               
        [else this]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;add-to-scene: -> Scene
    ;;RETURNS:A scene with world rendered on it.
    ;;EXAMPLES: as covered in test cases        
   (define/public (add-to-scene scene)
      (local
        ;; first add the target to the scene
        ((define scene-with-target (place-image ICON x y EMPTY-SCENE))) 
        ;; then tell each toy to add itself to the scene
        (foldr
         ;; Toy Scene -> Scene
         ;; GIVEN: A Toy and an initial scene
         ;; RETURNS: Renders the toy onto the scene
         (lambda (toy scene)
          (send toy add-to-scene scene))
         scene-with-target
         toys)))
   
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; -> Integer
    ;; RETURNS:The x and y coordinates of the target.
    (define/public (target-x) x)
    (define/public (target-y) y)
        
    ;;target-selected?: ->Boolean.
    ;;RETURNS:A boolean result whether a target is selected or not.
    (define/public (target-selected?)selected?)
    
    ;;get-toys: ->ListOfToys.
    (define/public (get-toys) toys)
     ))
