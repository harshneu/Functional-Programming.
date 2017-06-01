;;                       "The Marvellous Toy"                                 ;;


;;The illustration deals with delivering a toy for a child who can play with it
;;by a few key strokes.
;;The key strokes are:
;;"s":makes a new square toy.
;;"t":creates a new throbber.
;;"f":draws an deflatable football.
;;"w":draws a clock for the system which counts the number of ticks that have
;;occured after the creation of the clock.
;;All the above toys are created at the center of the target.

;;The program can be executed by using
;;(run rate speed)
;;example :(run 0.1 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket                     ;;Used here as to determine language from
                                 ;;source.

(require rackunit)               ;;In built module for testing framework.
(require "extras.rkt")           ;;A file inported.
(require "WidgetWorks.rkt")
(require 2htdp/universe)         ;;A racket module for using big bang functions.
(require 2htdp/image)            ;;A racket module for using image functions.
(check-location "10" "toys.rkt") ;;To check location of file.
(require "Interfaces.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Functions provided for Automated Testing.

(provide
 make-playground
 run
 make-square-toy
 make-throbber
 make-clock
 make-football 
 PlaygroundState<%> 
 Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;Data Definitions:

;;A Direction is one of
;;--Left    interp:The square toy moves in left direction.
;;--Right   interp:The square toy moves in right direction.
;;template:
;;direction-fn :Direction -> ??
#;(define (direction-fn direction)
    (cond
      [(string=? dir LEFT)...]
      [(string=? dir RIGHT)...]))

;;ListofToy<%> is a list of interface toy.
;;A list ofToy<%> can be either of:
;;--empty
;;--(cons Toy<%> LOT)

;;Template:
#;(define (fn lot)
    (cond
      [(empty? lot)..]
      [else(...
            (first-lot)
            (fn (rest lot)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; setting up the world

;;run : PosNum PosInt -> Void 
;;GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;;creates and runs a world in which square toys travel at the given
;;speed.
;;RETURNS: the final state of the world.




(define (run frame-rate speed)
  (define w (make-world CANVAS-WIDTH CANVAS-HEIGHT)) 
  (define p (make-playground speed))
  (send w add-stateful-widget p)
  (send w run frame-rate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Every interface is implemented using classes. Here the WorldState interface
;;is implemented using WorldState class.

;;A WorldState is a

;;Constructor Template.
;;(new WorldState%
;;[target][target][toys toys][speed speed])

;;Interpretation:
;;The WorldState represents a world which contains our target,a sqaure toy,
;;a throbble,a clock and a football. 


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
        [(key=? ke SQUARE-KE)         
              (set! toys (cons (make-square-toy x y speed) toys))]              
        
        ;;The Throbber Key event.
        [(key=? ke THROBBER-KE)
             (set! toys (cons (make-throbber x y) toys))]             
             
        ;;The clock key event.
        [(key=? ke CLOCK-KE)
           (set! toys (cons (make-clock x y) toys))]            
              
        
        ;;The Football key event.
        [(key=? ke FOOTBALL-KE)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; SquareToy Class

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;; End of Class Definition;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Function Definitions

;;make-playground : PosInt -> PlaygroundState<%>
;;RETURNS: a world with a target, but no toys, and in which any
;;square toys created in the future will travel at the given speed (in
;;pixels/tick)
;;EXAMPLE:
;; (make-playground 5 5) =>
;;(object:WorldState% ...)
(define (make-playground speed)
  (new WorldState%
                [x TARGET-X][y TARGET-Y]
                [toys empty]
                [mouse-x MX-INITIAL]
                [mouse-y MY-INITIAL]
                [speed speed]
                [selected? false]))
      

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

;;FUNCTION FOR TESTING

;; world-equal?: WorldState WorldState -> Boolean
;; GIVEN: the two worlds
;; RETURNS: true iff the observable properties of the two world are same
;; STRATEGY: combining simpler functions
(define (world-equal? w1 w2)
  (and  
   (=
    (send w1 target-x)
    (send w2 target-x))
   (=
    (send w1 target-y)
    (send w2 target-y))
   (equal?
    (send w1 target-selected?)
    (send w2 target-selected?))
   (andmap
    (lambda (t1 t2) (send t1 for-test:toy-equal? t2)) 
    (send w1 get-toys)
    (send w2 get-toys))))

;; TESTS
(begin-for-test
  (local
    ((define square-toy-moving-right
       (new Square% [x 50] [y 40][speed 10]
            [direction RIGHT])))
      (send square-toy-moving-right after-tick)
    (check-equal?
     (send square-toy-moving-right toy-x) 
     60
      "The x-position returned after tick is'nt as expected"))      
  (local
    ((define square-toy-move-left-after-tick
       (new Square% [x 580] [y 40] [speed 20]
            [direction RIGHT])))
      (send square-toy-move-left-after-tick after-tick)
      (check-equal?
      (send square-toy-move-left-after-tick  for-test:get-toy-direction)
      "left"    
     "The direction of toy returned after tick is not as expected"))
   (local
    ((define square-toy-moving-left
       (new Square% [x 50] [y 40] [speed 10]
            [direction LEFT])))
     (send square-toy-moving-left after-tick) 
      (check-equal?
       (send square-toy-moving-left toy-x)
       40
     "The x-position returned after tick is not as expected"))
   (local
    ((define square-toy-moving-left
       (new Square% [x 50] [y 40] [speed 10]
            [direction LEFT])))
     (send square-toy-moving-left after-tick) 
      (check-equal?
       (send square-toy-moving-left toy-y)
       40
     "The y-position returned after tick is not as expected"))
   (local
    ((define square-toy-move-right-after-tick
       (new Square% [x 40] [y 40] [speed 20]
            [direction LEFT])))
     (send square-toy-move-right-after-tick after-tick)
     (check-equal?
      (send square-toy-move-right-after-tick  for-test:get-toy-direction)
      "right"    
     "The direction of toy returned after tick is not as expected"))
   (local
    ((define square-toy-emtpy-scene 
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-toy-emtpy-scene add-to-scene EMPTY-SCENE)
     (place-image (square 40 "outline" "darkred") 40 60 EMPTY-SCENE)
     "The scene displayed is not as expected."))
    (local
    ((define square-toy
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-toy toy-x)
     40
     "The value displayed is not as expected."))
  (local
    ((define square-toy
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-toy toy-y)
     60
     "The value displayed is not as expected."))
  (local
    ((define square-direction
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-direction for-test:get-toy-direction)
     RIGHT
     "The value displayed is not as expected."))
  (local
    ((define square-equal
       (new Square% [x 40] [y 60] [speed 20]  
            [direction RIGHT])))
    (check-true
     (send square-equal for-test:toy-equal? 
           (make-square-toy 40 60 20))
     "The two squares are not equal"))
    (local
    ((define square-button-down
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-button-down after-button-down 100 100)
      square-button-down
     "The button down does not have any effect on square")) 
    (local
    ((define square-button-up
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-button-up after-button-up 100 100)
      square-button-up
     "The button up does not have any effect on square"))
    (local
    ((define square-drag
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-drag after-drag 100 100)
      square-drag
     "The drag does not have any effect on square"))
    (local
    ((define square-key-event
       (new Square% [x 40] [y 60] [speed 20] 
            [direction RIGHT])))
    (check-equal?
     (send square-key-event after-key-event SQUARE-KE)
      square-key-event
     "The key event does not have any effect on square"))
   (local
    ((define football-toy 
       (new Football% [x 40] [y 60]
            [scale-count 1]))) 
    (check-equal?
     (send football-toy toy-data)
     1
     "The value displayed is not as expected.")) 
   (local
    ((define football-toy
       (new Football% [x 40] [y 60]
            [scale-count 1]))) 
    (check-equal?
     (send football-toy toy-x)
     40
     "The value displayed is not as expected."))
   (local
    ((define football-toy
       (new Football% [x 40] [y 60]
            [scale-count 1]))) 
    (check-equal?
     (send football-toy toy-y) 
     60
     "The value displayed is not as expected."))
   (local
    ((define football-toy
       (new Football% [x 40] [y 60]
            [scale-count 1]))) 
    (check-equal?
     (send football-toy after-tick) 
    (check-equal?
     (send football-toy toy-data)
     0.99
     "The value displayed is not as expected.")))
   (local
    ((define football-toy
       (new Football% [x 40] [y 60]
            [scale-count 0.06]))) 
    (check-equal?
     (send football-toy after-tick) 
    (check-equal?
     (send football-toy toy-data)
     0.001
     "The value displayed is not as expected.")))
   (local
    ((define football-toy 
       (new Football% [x 40] [y 60]
            [scale-count 1]))) 
    (check-equal?
     (send football-toy for-test:toy-equal? football-toy)
     true
     "The value of the two football should be equal")) 
    (local
    ((define football-button-down
       (new Football% [x 40] [y 60] [scale-count 1])))
    (check-equal?
     (send football-button-down after-button-down 100 100)
      football-button-down
     "The button down does not have any effect on football"))
    (local
    ((define football-button-up
       (new Football% [x 40] [y 60] [scale-count 1])))
    (check-equal?
     (send football-button-up after-button-up 100 100)
      football-button-up
     "The button up does not have any effect on football"))
    (local
    ((define football-drag
       (new Football% [x 40] [y 60] [scale-count 1])))
    (check-equal?
     (send football-drag after-drag 100 100)
      football-drag
     "The drag does not have any effect on football"))
    (local
    ((define football-key-event
       (new Football% [x 40] [y 60] [scale-count 1])))
    (check-equal?
     (send football-key-event after-key-event FOOTBALL-KE)
      football-key-event
     "The key event does not have any effect on football"))
    (local
    ((define clock-toy
       (new Clock% [x 40] [y 60] [ticks 1]))) 
    (check-equal?
     (send clock-toy after-tick) 
    (check-equal?
     (send clock-toy toy-data)
     2
     "The value displayed is not as expected.")))
    (local
    ((define clock-toy
       (new Clock% [x 40] [y 60] [ticks 1]))) 
    (check-equal?
     (send clock-toy after-tick) 
    (check-equal?
     (send clock-toy toy-x)
     40
     "The value displayed is not as expected.")))
    (local
    ((define clock-toy
       (new Clock% [x 40] [y 60] [ticks 1]))) 
    (check-equal?
     (send clock-toy after-tick) 
    (check-equal?
     (send clock-toy toy-y)
     60
     "The value displayed is not as expected.")))
    (local
    ((define clock-toy
       (new Clock% [x 40] [y 60] [ticks 1]))) 
    (check-equal?
     (send clock-toy after-tick) 
    (check-equal?
     (send clock-toy for-test:toy-equal? clock-toy)
     true
     "The value displayed is not as expected.")))
    (local
    ((define clock-button-down
       (new Clock% [x 40] [y 60] [ticks 1])))
    (check-equal?
     (send clock-button-down after-button-down 100 100)
      clock-button-down
     "The button down does not have any effect on clock"))
    (local
    ((define clock-button-up
       (new Clock% [x 40] [y 60] [ticks 1])))
    (check-equal?
     (send clock-button-up after-button-up 100 100)
      clock-button-up
     "The button up does not have any effect on clock"))
    (local
    ((define clock-drag
       (new Clock% [x 40] [y 60] [ticks 1])))
    (check-equal?
     (send clock-drag after-drag 100 100)
      clock-drag
     "The drag does not have any effect on clock"))
    (local
    ((define clock-key-event
       (new Clock% [x 40] [y 60] [ticks 1])))
    (check-equal?
     (send clock-key-event after-key-event CLOCK-KE)
      clock-key-event
     "The key event does not have any effect on clock"))
    (local
    ((define throbber-toy
       (new Throbber% [x 40] [y 60]
            [radius 5][flag 1]))) 
    (check-equal?
     (send throbber-toy after-tick) 
    (check-equal?
     (send throbber-toy toy-data)
     6
     "The value displayed is not as expected.")))
    (local
    ((define throbber-toy
       (new Throbber% [x 40] [y 60]
            [radius 15][flag 1]))) 
    (check-equal?
     (send throbber-toy after-tick) 
    (check-equal?
     (send throbber-toy toy-data)
     16
     "The value displayed is not as expected.")))
    (local
    ((define throbber-toy
       (new Throbber% [x 40] [y 60]
            [radius 20][flag 1]))) 
    (check-equal?
     (send throbber-toy after-tick) 
    (check-equal?
     (send throbber-toy toy-data)
     19
     "The value displayed is not as expected.")))
    (local
    ((define throbber-toy
       (new Throbber% [x 40] [y 60]
            [radius 5] [flag 1]))) 
    (check-equal?
     (send throbber-toy after-tick) 
    (check-equal?
     (send throbber-toy toy-x)
     40
     "The value displayed is not as expected.")))
    (local
    ((define throbber-toy
       (new Throbber% [x 40] [y 60] [radius 5]
            [flag 1]))) 
    (check-equal?
     (send throbber-toy after-tick) 
    (check-equal?
     (send throbber-toy toy-y)
     60
     "The value displayed is not as expected.")))
    (local
    ((define throbber-equal
       (new Throbber% [x 40] [y 60] [radius 5]  
            [flag 1])))
    (check-true
     (send throbber-equal for-test:toy-equal? 
           (make-throbber 40 60))
     "The two throbbers are not equal"))
    (local
    ((define throbber-button-down
       (new Throbber% [x 40] [y 60] [radius 20] 
            [flag 1])))
    (check-equal?
     (send throbber-button-down after-button-down 100 100)
      throbber-button-down
     "The button down does not have any effect on throbber")) 
    (local
    ((define throbber-button-up
       (new Throbber% [x 40] [y 60] [radius 20] 
            [flag 1])))
    (check-equal?
     (send throbber-button-up after-button-up 100 100)
      throbber-button-up
     "The button up does not have any effect on throbber"))
    (local
    ((define throbber-drag
       (new Throbber% [x 40] [y 60] [radius 20] 
            [flag 1])))
    (check-equal?
     (send throbber-drag after-drag 100 100)
      throbber-drag
     "The drag does not have any effect on throbber"))
    (local
    ((define throbber-key-event
       (new Throbber% [x 40] [y 60] [radius 20] 
            [flag 1])))
    (check-equal?
     (send throbber-key-event after-key-event THROBBER-KE)
      throbber-key-event
     "The key event does not have any effect on throbber")) 
   (local
    ((define world1
       (new WorldState%
                [x TARGET-X][y TARGET-Y]
                [toys empty]
                [mouse-x MX-INITIAL]
                [mouse-y MY-INITIAL]
                [speed 10]
                [selected? false])))
    (check world-equal? (make-playground 10) world1
           "The new world is not as expected."))
   (local
    ((define world1
       (new WorldState%
                   [x TARGET-X] [y TARGET-Y] 
                   [mouse-x MX-INITIAL] [mouse-y MY-INITIAL]
                   [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))
          (check world-equal? world1 world1)           
          "The world's should have been equal")
   (local
    ((define world1
       (new WorldState%
                         [x TARGET-X] [y TARGET-Y]
                         [mouse-x MX-INITIAL] [mouse-y MY-INITIAL]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1])
                   (new Football% [x 100] [y 120] [scale-count 1])
                   (new Throbber% [x 120] [y 150] [radius 5]
                        [flag 1]))] 
            [speed 10])))
     (send world1 after-tick)
     (check-equal?
      (send world1 target-x)
      250
      "The x-cordinate of target in world after tick is not as expected")
     (check-equal?
      (send world1 target-y)
      300
      "The y-cordinate of target in world after tick is not as expected")
     (check-equal?
      (send world1 target-selected?)
      false
      "The target should have been not selected")
     (check-equal?
      (length (send world1 get-toys))
      4
      "The list of toys returned are not correct"))
   (local
    ((define world1
       (new WorldState%
                         [x TARGET-X] [y TARGET-Y]
                         [mouse-x MX-INITIAL] [mouse-y MY-INITIAL]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
         (send world1 after-button-down  
               TARGET-X TARGET-Y )
         (check-equal?
          (send world1 target-selected?)
          true
           "The target remained unselected even after button down was in the
           range"))
     (local
        ((define world1
       (new WorldState%
                         [x TARGET-X] [y TARGET-Y]
                         [mouse-x MX-INITIAL] [mouse-y MY-INITIAL]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
        (send world1 after-button-down 100 100)
          (check-equal?
          (send world1 target-selected?)
          false
           "The target got selected even after button down was in the
           range"))
   (local
    ((define world1
       (new WorldState%
                         [x 40] [y 60]
                         [mouse-x 45] [mouse-y 55]
                         [selected? true]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
      (send world1 after-drag 60 60)
         (check-equal?
          (send world1 target-x)
          55
           "The target was not dragged as expected")
         (check-equal?
          (send world1 target-y)
          65
           "The target was not dragged as expected"))
    (local
    ((define world1
       (new WorldState%
                         [x 40] [y 60]
                         [mouse-x 45] [mouse-y 55]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
       (send world1 after-drag 60 60 )
        (check-equal?
          (send world1 target-x)
          40
           "The target should not have been dragged")
         (check-equal?
          (send world1 target-y)
          60
           "The target should not have been dragged"))
     (local
    ((define world1
       (new WorldState%
                         [x TARGET-X] [y TARGET-Y]
                         [mouse-x TARGET-X] [mouse-y TARGET-Y]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
        (send world1 after-button-up
                 TARGET-X TARGET-Y )
         (check-equal?
          (send world1 target-selected?)
          false
           "The target remained selected even after button up was in the
           range"))
   (local
    ((define world1
       (new WorldState%
                         [x TARGET-X] [y TARGET-Y]
                         [mouse-x TARGET-X] [mouse-y TARGET-Y]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
        (send world1 after-button-up TARGET-X TARGET-Y )
         (check-equal?
          (send world1 target-x)
          TARGET-X
           "The target should not have been affected by other mouse event")
         (check-equal?
          (send world1 target-y)
          TARGET-Y
           "The target should not have been affected by other mouse event"))
    (local
    ((define world1
       (new WorldState%
                         [x TARGET-X] [y TARGET-Y]
                         [mouse-x TARGET-X] [mouse-y TARGET-Y]
                         [selected? false]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))           
     (send world1 after-button-down TARGET-X TARGET-Y)
         (check-equal?
          (send world1 target-x)
          TARGET-X
           "The target should not have been affected by other mouse event")
         (check-equal?
          (send world1 target-y)
          TARGET-Y
           "The target should not have been affected by other mouse event"))    
  (local
    ((define world1
       (new WorldState%
                         [x 50] [y 100]
                         [mouse-x 50] [mouse-y 100]
                         [selected? true]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))
           (send world1 after-key-event SQUARE-KE)
           (check-equal?
            (length (send world1 get-toys))
            3
            "The number of toys in the world is not as expected"))
    (local
      ((define world1
       (new WorldState%
                         [x 50] [y 100]
                         [mouse-x 50] [mouse-y 100]
                         [selected? true]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))           
        (send world1 after-key-event CLOCK-KE)
        (check-equal?
            (length (send world1 get-toys))
            3
            "The number of toys in the world is not as expected"))
   (local
    ((define world1
       (new WorldState%
                         [x 50] [y 100]
                         [mouse-x 50] [mouse-y 100]
                         [selected? true]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
        (send world1 after-key-event FOOTBALL-KE)
           (check-equal?
            (length (send world1 get-toys))
            3
            "The number of toys in the world is not as expected"))   
  (local
      ((define world1
         (new WorldState%
                           [x 50] [y 100]
                           [mouse-x 50] [mouse-y 100]
                           [selected? true]
              [toys (list
                     (new Square% [x 100] [y 120] [speed 10]
                          [direction RIGHT])
                     (new Clock% [x 200] [y 150] [ticks 1]))] 
              [speed 10])))              
         (send world1 after-key-event THROBBER-KE)
            (check-equal?
            (length (send world1 get-toys))
            3
            "The number of toys in the world is not as expected"))  
  (local
    ((define world1
       (new WorldState%
                         [x 50] [y 100]
                         [mouse-x 50] [mouse-y 100]
                         [selected? true]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT])
                   (new Clock% [x 200] [y 150] [ticks 1]))] 
            [speed 10])))            
      (send world1 after-key-event INVALID-KE)
           (check-equal?
            (length (send world1 get-toys))
            2
            "The number of toys in the world is not as expected"))  
  (local
    ((define world1
       (new WorldState%
                         [x 50] [y 100]
                         [mouse-x 50] [mouse-y 100]
                         [selected? true]
            [toys (list
                   (new Square% [x 100] [y 120] [speed 10]
                        [direction RIGHT]))] 
            [speed 10])))            
    (check-equal?
     (send world1 add-to-scene EMPTY-SCENE)
     (place-image (square 40 "outline" "darkred") 100 120
                  (place-image (circle 10 "outline" "goldenrod")
                               50 100 EMPTY-SCENE))
             "The images are not properly placed"))  
  (local
    ((define football1
       (new Football% [x 100] [y 100] [scale-count 1])))
    (check-equal?
     (send football1 add-to-scene EMPTY-SCENE)
     (place-image FOOTBALL1 100 100 EMPTY-SCENE)
     "The football image is not properly placed"))  
  (local
    ((define clock1
       (new Clock% [x 100] [y 100]
                   [ticks 1])))
    (check-equal?
     (send clock1 add-to-scene EMPTY-SCENE)
     (place-image (overlay(pulled-regular-polygon 20 8 1/4 10 "outline" "red") 
                  (text "1" 15 "violetred"))
                  100 100 EMPTY-SCENE)
     "The clock image is not properly placed"))   
  (local
    ((define throbber1
       (new Throbber% [x 100] [y 100]
                   [radius 5] [flag 1])))
    (check-equal?
     (send throbber1 add-to-scene EMPTY-SCENE)
     (place-image (circle 5 "solid" "cornflowerblue") 100 100 EMPTY-SCENE)
     "The throbber image is not properly placed")))
