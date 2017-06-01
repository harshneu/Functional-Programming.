;;This file contains all the controllers and is used by our mvc file to execute
;;the illustration using the run command.The concept of Inheritance has been used here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket                           ;; Language Specifier.
(require 2htdp/image)                  ;; Module used for image functions.
(require "extras.rkt")                 ;; File provided with inherent functions    
(require "Interfaces.rkt")             ;; Contains The Interfaces.
(require "PerfectBounce.rkt")          ;; A file which provides the function to
;                                      ;; execute perfect bounce.
(require "ParticleWorld.rkt")
(require 2htdp/universe)
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Provided Functions:
(provide
 UniversalController%
 ControllerFactory%
 XYController%
 XController%
 YController%
 Model%
 PositionController%
 VelocityController%)


;;Constants Used In the code.

;;Key Events Constants.
(define XCONTKEY "x")
(define XYCONTKEY "z")
(define YCONTKEY "y")
(define VELCONTKEY "v")
(define POSCONTKEY "p")
(define UP "up")
(define DOWN "down")
(define RIGHT "right")
(define LEFT "left")
(define INVALID-KEY-EVENT " ")

;;Image Constants.
(define HANDLE 10)
(define FONT-SIZE 10)
(define CHANGE 5)
(define BLUE "blue")
(define RED "red")
(define OUTLINE "outline")
(define SOLID "solid")
(define BALL-RADII 2)
(define BALL-RADIUS 7)
(define TOP "top")
(define BLACK "black")
(define EMPTY-CANVAS (empty-scene 600 500))

;;Dimension Constants:
(define PARTICLE-Y 25)
(define PARTICLE-X 25)
(define HEIGHT2 150)
(define BASE 0)
(define HALF 2)
(define CANVAS-WIDTH2` 300)
(define CANVAS-HEIGHT2 250)
(define WIDTH 180)
(define HEIGHT 130)
(define BOUNDARY 150)
(define BOUNDRY 100)
(define END 15)
(define HEIGHT1 50)
(define WIDTH1 50)
(define ENDX 11.5)
(define ENDY 14)
(define WIDTHS 160)
(define WIDTH3 75)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Data Definitions:
;; A ColorString is one of:
;; --"green"
;; --"red"

;;Template:
;;colorstring-fn: ColorString -> ??

#;(define (colorstring-fn colors)
    (cond
      [(String=? colors "black")..]
      [(String=? colors "red")..]))


;;####Universal Controller.

;; a UniversalController% is a (new UniversalController% [model Model<%>])

(define UniversalController%
  (class* object% (Controller<%>)
    (init-field model)                          ;; the model
    (init-field [x CANVAS-WIDTH2]               ;; To define The coordiantes
                [y CANVAS-HEIGHT2])             ;; of the canvas
    (init-field [p particle])                   ;;The particle on which all the
    ;                                           ;;transitions occur.
    (init-field [width WIDTHS][height HEIGHT1])
    (init-field [half-width (/ width HALF)])
    (init-field [half-height (/ height HALF)])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    
    (init-field [selected? false])
    (init-field [block-selected? false])
    (init-field [saved-mx BASE])
    (init-field [saved-my BASE])
    (super-new)
    (send model register this)
    
    ;;receive-signal:A signal->Void.
    ;;Given:A signal.
    ;;Returns: decodes signal and updates local data.
    ;;Strategy: Communicate via states.
    (define/public (receive-signal sig)
      (set! p sig))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; STRATEGY: Cases on whether the event is in this object
    ; Examples: Used in test Cases.
    (define/public (after-button-down mx my)
      (cond
        [(and (not(in-this? mx my))(in-box? mx my))
         (begin
           (set! selected? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y)))]
        [(in-this? mx my)
         (begin
           (set! block-selected? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y)))]))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    ;;Strategy:Communicate via state.
    ; Examples: Used in test Cases.
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! block-selected? false)))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    ; Examples: Used in test Cases.
    (define/public (after-drag mx my)
      (if block-selected? 
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          void))
    
    
    ;; abstract function to be supplied by subclass
    (abstract in-box?)
    
    ;; abstract function to be supplied by subclass
    (abstract in-this?)
    
    ;; abstract function to be supplied by subclass
    (abstract add-to-scene)  
    (define/public (after-tick) 'viewer2-after-tick-trap)
    
    ;; abstract function to be supplied by subclass
    (abstract after-key-event)
    
    ;; abstract functions to be provided by subclass
    (abstract current-color)
    (abstract box-color)
    
    
    ;; abstract functions to be supplied by subclass to be used for
    ;; placing the image
    (abstract viewer-initial-image)
    (abstract viewer-image)))

;;#########################################################################


;; ModelClass:
;;The Model Class has the particle and rectangle the initial position of the
;;particle and rectangle are established in this class.

(define Model% 
  (class* object% (Model<%>)
    (init-field [p particle])
    (init-field [rect (make-rect
                       BASE BOUNDARY BASE 100)]) ;;represents coordnates.
    
    ; ListOfController<%>
    (init-field [controllers empty])             ;;No controllers initially.
    (field [stop-tick? false] )
    
    (super-new)
    (set! p (make-particle
             WIDTH3 HEIGHT1 BASE BASE))          ;;Initial particle Conditions.
    
    ;;#########################################################################
    
    ;; stop-tick:->Boolean
    ;; Effects:A boolean results to manage the ball movement in the canvas.
    ;; Strategy: Communicate via state.
    ;; Examples: Used in test Cases.
    (define/public (stop-tick value)
      (set! stop-tick? value))
    
    ;; after-tick:-> Void
    ;; Given :the world.
    ;; Effects:Moves the object by v.if the resulting x is >= 20BASE or <= BASE 
    ;;             reports x at ever tick reports velocity only when it changes
    ;; Strategy: Communicate via state.
    ;; Examples: Used in test Cases.
    (define/public (after-tick)
      
      (if stop-tick?
          this
          (begin
            (set! p (particle-after-tick p rect))
            (publish-particle))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Controller -> Void
    ;; Given: A controller.
    ;; Returns:register the new controller and send it some data 
    ;; Strategy: Communicate via state.
    ;; Examples: Used in test Cases.
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal p)))
    
    ;; execute-command -> Void
    ;; Given :A command.
    ;; Returns : decodes the command, executes it, and sends updates to the
    ;; controllers.
    ;; Strategy:Communicate via state.
    ;; Examples: Used in test Cases.
    (define/public (execute-command cmd)
      (begin
        (set! p cmd)
        (publish-particle)))
    
    
    ;; publish-particle: Paricle->Particle.
    ;; Given :A particle.
    ;; Returns: report position or velocity to each controllers.
    ;; Examples: Used in test Cases. 
    (define (publish-particle)
      (for-each
       (lambda (obs) (send obs receive-signal p))
       controllers))
    
    ;; Testing functions:
    (define/public (for-test:get-particle)p)
    (define/public (for-test:get-controllers)controllers)))

;;#########################################################################

;;#########################################################################

;;Controller Factory.


(define ControllerFactory%
  (class* object% (SWidget<%>)
    ; the world in which the controllers will live
    (init-field w)   ; World<%>
    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>
    
    (super-new)
    
    ;;after-key-event:keyevent->Void
    ;;Given: A key event.
    ;;Effect:Adds the controller to the scene.
    ;;Details:Adds a new controller to the scene.
    ;;Strategy:Use casess on key events.
    ;;Example:Used in tests.
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev VELCONTKEY) (add-viewer VelocityController%)]
        [(key=? kev POSCONTKEY) (add-viewer PositionController%)]
        [(key=? kev XYCONTKEY) (add-viewer XYController%)]
        [(key=? kev XCONTKEY) (add-viewer XController%)]
        [(key=? kev YCONTKEY) (add-viewer YController%)]))
    
    
    ;;add-viewer: Class% ->Class%
    ;;Given: Any class.
    ;;Returns :Adds a viewer to any class on the keyevent.
    ;;Strategy: Use simple function.
    ;;Example: When an add-viewer is used and a class is given in accordance the
    ;;         viewer gets added to that respective class.
    
    (define/public (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))
    
    ;; add-to-scene :Scene -> Scene
    ;; Given: A scene.
    ;; Returns: A scene with the scene rendered on it.
    ;; Examples: Used in test Cases.
    
    (define/public (add-to-scene s) s)
    
    ;;after-tick :Scene->void
    ;;EFFECT: updates the controller factory to a state where should be in
    ;;after a tick.
    ;; Examples: Used in test Cases.
    (define/public (after-tick) 'controller-factory-after-tick-trap)
    
    ;; after-button-down : -> Void
    ;; RETURNS: the world that should follow this one after a button-down
    ;; DETAILS: button-up unselects the controler.
    ;; EXAMPLES: as covered in test cases
    ;;Strategy: Use cases on button down
    
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)))


;;;####### Position Controller.


;; a PositionController% is a (new PositionController% [model Model<%>])

(define PositionController%
  (class* UniversalController% (Controller<%>)
    
    ; Nats -- the position of the center of the controller
    (inherit-field model x y p width height half-width half-height
                   selected? block-selected? saved-mx saved-my)
    
    
    (super-new)
    (send model register this)
    
    ;; in-box? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the controller or not.
    (define/override (in-box? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ x half-width))
       (<= (- y half-height) other-y (+ y half-height))))
    
    
    ;; in-this? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the handle or not.
    ;; Examples: Used in test Cases.
    (define/override (in-this? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ HANDLE (- x half-width)))
       (<= (- y half-height) other-y (+ HANDLE (- y half-height)))))   
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    ;; Examples: Used in test Cases.
    
    (define/override (add-to-scene scene)
      (place-image (viewer-image) x y scene))
    
    ;;after-key-event: KeyEvent-> World
    ;;GIVEN:a keyevent to the world.
    ;;RETURNS:a world after the key event.
    ;;EXAMPLES:as covered in test cases
    ;;STRATEGY: cases on KeyEvent
    (define/override (after-key-event kev)  
      (if selected?
          (cond            
            [(key=? UP kev)
             (send model execute-command
                   (make-particle
                    (particle-x p)
                    (- (particle-y p) CHANGE)
                    (particle-vx p)
                    (particle-vy p)))]            
            
            [(key=? DOWN kev)
             (send model execute-command
                   (make-particle
                    (particle-x p)
                    (+ (particle-y p) CHANGE)
                    (particle-vx p)
                    (particle-vy p)))]            
            
            [(key=? RIGHT kev)
             (send model execute-command
                   (make-particle
                    (+ CHANGE (particle-x p))
                    (particle-y p)
                    (particle-vx p)
                    (particle-vy p)))]
            
            [(key=? LEFT kev)
             (send model execute-command
                   (make-particle
                    (- (particle-x p) CHANGE)
                    (particle-y p)
                    (particle-vx p)
                    (particle-vy p)))])
          void))
    
    ;; for Testing
    (define/public (for-test:get-particle) p)
    
    ;; For Testing
    (define/public (for-test:get-selected?) selected?)
    (define/public (for-test:get-block-selected?) block-selected?)
    
    
    ;;current-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    ;;Example:Used in tests.
    (define/override (current-color)
      (if selected? RED BLACK))
    
    ;;box-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object
    ;;Example:Used in tests.
    (define/override (box-color)
      (if block-selected? RED BLACK))
    
    ;;#####################################################################
    
    ;; Image Definitions:
    
    ;; Viewer-initial-image->Image.
    ;; Given: An Image.
    ;; Returns:The image.
    ;; Details:Assemble the image of the viewer initially.
    ;; Strategy:Combine Simpler Functions.
    ;; Example:Used in tests.
    
    (define/override (viewer-initial-image)
      (let ((the-data-image (data-image)))
        (overlay  
         the-data-image
         (rectangle 
          (max width (+ (image-width the-data-image) HANDLE))
          (max height (+ (image-height the-data-image) HANDLE))
          OUTLINE 
          BLACK))))
    
    ;; assemble the image of the viewer with the handler
    
    ;;Viewer-image->Image.
    ;;Given:Image.
    ;;Returns:Image
    ;;Details:Assemble the image of the viewer with the handler
    ;;Strategy:Use simpler functions.
    ;;Example:Used in tests.
    
    (define/override (viewer-image) 
      (let ((initial-image (viewer-initial-image)))
        (overlay/align LEFT TOP
                       initial-image
                       (square HANDLE OUTLINE (box-color)))))
    
    
    ;;data-image->Image.
    ;;Given : Image.
    ;;Returns: An image.
    ;;Strategy: Use simple functions
    (define (data-image)
      (above
       (text "Arrow-Keys : Change Position" HANDLE (current-color))
       (text (string-append
              "X = "
              (number->string (particle-x p))
              " Y = "
              (number->string (particle-y p))
              " VX = "
              (number->string (particle-vx p))
              " VY = "
              (number->string (particle-vy p)))
             HANDLE
             (current-color))))
    ))



;;;#################################################################

;;Velocity Controller.

;; a VelocityController% is a (new VelocityController% [model Model<%>])

(define VelocityController%
  (class* UniversalController% (Controller<%>)
    ; Nats -- the position of the center of the controller
    (inherit-field model x y p width height half-width half-height
                   selected? block-selected? saved-mx saved-my)
    
    (super-new)
    (send model register this)
    
    ;; in-box? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the controller or not.
    ;; Example:Used in tests. 
    
    (define/override (in-box? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ x half-width))
       (<= (- y half-height) other-y (+ y half-height))))
    
    
    ;; in-this? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the handle or not.
    ;; Example:Used in tests.
    
    (define/override (in-this? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ HANDLE (- x half-width)))
       (<= (- y half-height) other-y (+ HANDLE (- y half-height))))) 
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    
    (define/override (add-to-scene scene)
      (place-image (viewer-image) x y scene))
    
    ;;after-key-event: KeyEvent-> World
    ;;GIVEN:a keyevent to the world.
    ;;RETURNS:a world after the key event.
    ;;EXAMPLES:as covered in test cases
    ;;STRATEGY: cases on KeyEvent
    
    (define/override (after-key-event kev)
      (if selected?
          (cond
            [(key=? UP kev)
             (send model execute-command
                   (make-particle
                    (particle-x p)
                    (particle-y p) 
                    (particle-vx p)
                    (+ (particle-vy p) CHANGE)))]
            
            [(key=? DOWN kev)
             (send model execute-command
                   (make-particle
                    (particle-x p)
                    (particle-y p) 
                    (particle-vx p)
                    (- (particle-vy p) CHANGE)))]
            
            [(key=? RIGHT kev)
             (send model execute-command
                   (make-particle
                    (particle-x p)
                    (particle-y p)
                    (+ (particle-vx p) CHANGE)
                    (particle-vy p)))]
            
            [(key=? LEFT kev)
             (send model execute-command
                   (make-particle
                    (particle-x p)
                    (particle-y p)
                    (- (particle-vx p) CHANGE)
                    (particle-vy p)))])
          void)) 
    
    ;; for testing
    
    (define/public (for-test:get-particle) p)
    
    ;; For testing
    (define/public (for-test:get-selected?) selected?)
    (define/public (for-test:get-block-selected?) block-selected?)
    
    ;;current-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    (define/override (current-color)
      (if selected? RED BLACK))
    
    ;;box-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    ;;Example:Used in tests.
    
    (define/override (box-color)
      (if block-selected? RED BLACK))
    
    
    ;;#####################################################################
    
    ;;Image Definitions:
    
    ;; Viewer-initial-image->Image.
    ;; Given: An Image.
    ;; Returns:The image.
    ;; Details:Assemble the image of the viewer initially.
    ;; Strategy:Combine Simpler Functions.
    ;;Example:Used in tests.
    
    (define/override (viewer-initial-image)
      (let ((the-data-image (data-image)))
        (overlay  
         the-data-image
         (rectangle 
          (max width (+ (image-width the-data-image) HANDLE))
          (max height (+ (image-height the-data-image) HANDLE))
          OUTLINE 
          BLACK))))
    
    ;; assemble the image of the viewer with the handler
    
    ;;Viewer-image->Image.
    ;;Given:Image.
    ;;Returns:Image
    ;;Details:Assemble the image of the viewer with the handler
    ;;Strategy:Use simpler functions.
    ;;Example:Used in tests.
    
    (define/override (viewer-image) 
      (let ((initial-image (viewer-initial-image)))
        (overlay/align LEFT TOP
                       initial-image
                       (square HANDLE OUTLINE (box-color)))))
    
    
    ;;data-image->Image.
    ;;Given : Image.
    ;;Returns: An image.
    ;;Strategy: Use simple functions
    ;;Example:Used in tests.
    
    (define (data-image)
      (above
       (text "Arrow Keys : Change velocity" HANDLE (current-color))
       (text (string-append
              "X = "
              (number->string (particle-x p))
              " Y = "
              (number->string (particle-y p))
              " VX = "
              (number->string (particle-vx p))
              " VY = "
              (number->string (particle-vy p)))
             HANDLE
             (current-color))))))

;;#########################################################################

;;A Y Controller
;; a YController% is a (new YController% [model Model<%>])

(define YController%
  (class* UniversalController% (Controller<%>)
    
    
    ; Nats -- the position of the center of the controller
    (init-field [x1 CANVAS-HEIGHT2] [y1 CANVAS-WIDTH2])
    (inherit-field model p)
    (init-field [width1 WIDTH1][height1 HEIGHT])
    
    ;; half width for perfect bounce        
    (field [half-width1  (/ WIDTH1  HALF)])
    (field [half-height1 (/ HEIGHT HALF)])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    
    (init-field [selected1? false])
    (init-field [box-selected1? false])
    (field [saved-mx1 BASE])             ;; used for smooth dtagging.
    (field [saved-my1 BASE])     
    (field [saved-mx2 BASE])             ;; used for stopping and passing the 
    (field [saved-my2 BASE])             ;; ball coordinates after this event.    
    (field [new-p-x BASE])       
    (field [new-p-y BASE])
    
    (super-new)
    (send model register this)
    (set! new-p-x (particle-x p))        ;;represents the new particle y posn.
    (set! new-p-y (particle-y p))        ;; represents the new particle x posn.
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; Details :Check For the coordinates where the button down actually is
    ; STRATEGY: Cases on whether the event is in this object
    ; Example:Used in tests.
    (define/override (after-button-down mx my)
      (cond
        [(in-this? mx my)
         (begin
           (set! selected1? true)
           (set! saved-mx1 (- mx x1))
           (set! saved-my1 (- my y1)))]
        
        [(in-box? mx my)
         (begin
           (set! box-selected1? true)
           (set! saved-mx2 (- mx (particle-x p)))
           (set! saved-my2 (- my (particle-y p)))
           (send model stop-tick true))]))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    ; Strategy: Communicate via state.
    ; Example:Used in tests.
    
    (define/override (after-button-up mx my)
      (set! selected1? false)
      (set! box-selected1? false)
      (send model stop-tick false))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    ; Strategy: Communicate via state.
    
    (define/override (after-drag mx my)
      (begin
        (if selected1?
            (begin
              (set! x1 (- mx saved-mx1))
              (set! y1 (- my saved-my1)))
            void)
        (if box-selected1?
            (begin
              (set! new-p-x (- mx saved-mx2))
              (set! new-p-y (- my saved-my2))
              (send
               model execute-command
               (make-particle
                new-p-x new-p-y (particle-vx p) (particle-vy p))))
            void))) 
    
    ;; in-this? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the handle or not.
    ;; Example:Used in tests.
    
    (define/override (in-this? other-x other-y)
      (and
       (<= (- x1 half-width1) other-x (+ HANDLE (- x1 half-width1)))
       (<= (- y1 half-height1) other-y (+ HANDLE (- y1 half-height1)))))
    
    ;; in-box? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the controller or not.
    ;;Example:Used in tests.
    
    (define/override (in-box? other-x other-y) 
      (and
       (<= (- x1 half-width1) other-x (+ x1 half-width1))
       (<= (- y1 half-height1) other-y (+ y1 half-height1))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    ;; Example:Used in tests.
    
    (define/override (add-to-scene scene)
      (place-image (viewer-image) x1 y1 scene))
    
    ;;after-key-event: KeyEvent-> World
    ;;GIVEN:a keyevent to the world.
    ;;RETURNS:a world after the key event.
    ;;EXAMPLES:as covered in test cases
    ;;STRATEGY: cases on KeyEvent
    
    (define/override (after-key-event kev)
      this)
    
    ;;current-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    ;;Strategy: Use simple functions.
    ;;Example:Used in tests.
    
    (define/override (current-color)
      (if selected1? RED BLACK))
    
    ;;box-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    ;;Strategy :Use simpler functions.
    ;;Example:Used in tests.
    
    (define/override (box-color)
      void)
    
    ;; For testing
    (define/public (for-test:get-particle) p)
    (define/public (for-test:get-selected?) selected1?)
    (define/public (for-test:get-box-selected?) box-selected1?)
    ;; Testing functions
    (define/public (for-test:get-x) x1)
    (define/public (for-test:get-y) y1)
    (define/public (for-test:get-new-x) new-p-x)
    (define/public (for-test:get-new-y) new-p-y)
    
    
    ;;#########################################################################
    
    ;;Image Definition:
    
    ;;Ball and the particle definition
    (define (BALL)
      (overlay (circle BALL-RADII SOLID BLACK) (circle BALL-RADIUS SOLID RED)))
    
    ;;Assemble the image of the viewer initially
    (define/override (viewer-initial-image)
      (place-image
       (BALL) PARTICLE-X (particle-y p) 
       (rectangle WIDTH1 BOUNDRY OUTLINE BLUE))) 
    
    ;;Assembly of the viewer
    (define (viewer-initial-image2)
      (overlay
       (viewer-initial-image)
       (rectangle WIDTH1 BOUNDRY OUTLINE BLUE)
       (rectangle WIDTH1 HEIGHT OUTLINE BLACK)))
    
    ;; assemble the image with the handler
    (define/override (viewer-image)
      (let ((initial-image (viewer-initial-image2)))
        (overlay/align LEFT TOP
                       initial-image
                       (square HANDLE OUTLINE (current-color)))))))

;;##########################################################################
;;;; XController.

;; a XController% is a (new XController% [model Model<%>])

(define XController%
  (class* UniversalController% (Controller<%>)
    
    (inherit-field model p)
    (init-field [x1 CANVAS-HEIGHT2] [y1 CANVAS-WIDTH2])  
    (init-field [width1 WIDTH][height1 HEIGHT1]) 
    
    ;; half-width with perfect bounce           
    (field [half-width1  (/ width1  HALF)])
    (field [half-height1 (/ HEIGHT1 HALF)])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to 
    ;; center of viewer; else any value
    (init-field [selected1? false])
    (init-field [box-selected1?
                 false])
    
    (field [saved-mx1 BASE])     ;; used for smooth dtagging.
    (field [saved-my1 BASE])     
    (field [saved-mx2 BASE])     ;; used for stopping the ball and passing the 
    
    (field [saved-my2 BASE])     ;; coordinates after this event.    
    (field [new-p-x BASE])       
    (field [new-p-y BASE])
    
    (super-new)
    (send model register this)
    (set! new-p-x (particle-x p)) ;;represents the new particle y position.
    (set! new-p-y (particle-y p)) ;; represents the new particle x position.
    
    
    ;;#####################################################################
    
    ;;Mouse Functions.
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; Details :Check For the coordinates where the button down actually is
    ; STRATEGY: Cases on whether the event is in this object
    (define/override (after-button-down mx my)
      (cond
        [(in-this? mx my)
         (begin
           (set! selected1? true)
           (set! saved-mx1 (- mx x1))
           (set! saved-my1 (- my y1)))]
        
        [(in-box? mx my)
         (begin
           (set! box-selected1? true)
           (set! saved-mx2 (- mx (particle-x p)))
           (set! saved-my2 (- my (particle-y p)))
           (send model stop-tick true))]))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    ; Strategy: Communicate via state.
    ; Example:Used in tests.
    (define/override (after-button-up mx my)
      (set! selected1? false)
      (set! box-selected1? false)
      (send model stop-tick false))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    ; Strategy: Communicate via state.
    (define/override (after-drag mx my)
      (begin
        (if selected1?
            (begin
              (set! x1 (- mx saved-mx1))
              (set! y1 (- my saved-my1)))
            void)
        (if box-selected1?
            (begin
              (set! new-p-x (- mx saved-mx2))
              (set! new-p-y (- my saved-my2))
              (send
               model execute-command
               (make-particle
                new-p-x new-p-y (particle-vx p) (particle-vy p))))
            void))) 
    
    ;; Testing functions
    (define/public (for-test:get-x) x1)
    (define/public (for-test:get-y) y1)
    (define/public (for-test:get-new-x) new-p-x)
    (define/public (for-test:get-new-y) new-p-y)
    
    ;;#####################################################################
    
    
    ;; in-this? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the handle or not.
    (define/override (in-this? other-x other-y)
      (and
       (<= (- x1 half-width1) other-x (+ ENDX (- x1 half-width1)))
       (<= (- y1 half-height1) other-y (+ ENDY (- y1 half-height1)))))
    
    ;; in-box? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the controller or not.
    
    (define/override (in-box? other-x other-y)
      (and
       (<= (- x1 half-width1) other-x (+ x1 half-width1))
       (<= (- y1 half-height1) other-y (+ y1 half-height1))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/override (add-to-scene scene)
      (place-image (viewer-image) x1 y1 scene))
    
    
    ;;after-key-event: KeyEvent-> World
    ;;GIVEN:a keyevent to the world.
    ;;RETURNS:a world after the key event.
    ;;EXAMPLES:as covered in test cases
    ;;STRATEGY: cases on KeyEvent
    (define/override (after-key-event kev)this)
    
    ;;current-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    (define/override (current-color)
      (if selected1? RED BLACK))
    
    ;;box-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    
    (define/override (box-color)
      void)
    
    ;; For testing
    (define/public (for-test:get-particle) p)
    (define/public (for-test:get-selected?) selected1?)
    (define/public (for-test:get-box-selected?) box-selected1?)
    
    
    
    ;;#####################################################################
    
    ;;Image Definitions:
    
    
    ;;Ball and the particle definition
    (define (BALL)
      (overlay (circle BALL-RADII SOLID BLACK) (circle BALL-RADIUS SOLID RED)))
    
    ;;Assemble the image of the viewer initially
    (define/override (viewer-initial-image)
      (place-image
       (BALL) (particle-x p) PARTICLE-Y
       (rectangle BOUNDARY HEIGHT1 OUTLINE BLUE)))
    
    ;;Assembly of the viewer
    (define (viewer-initial-image2)
      (overlay
       (viewer-initial-image)
       (rectangle BOUNDARY HEIGHT1 OUTLINE BLUE)
       (rectangle WIDTH HEIGHT1 OUTLINE BLACK)))
    
    ;; assemble the image with the handler
    (define/override (viewer-image)
      (let ((initial-image (viewer-initial-image2)))
        (overlay/align LEFT TOP
                       initial-image
                       (square HANDLE OUTLINE (current-color)))))))


;;#########################################################################
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;XY Controller.

;; a XYController% is a (new XYController% [model Model<%>])
(define XYController%
  (class* UniversalController% (Controller<%>)
    
    ; Nats -- the position of the center of the controller
    (inherit-field model p)
    (init-field [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2])  
    (init-field [width1 WIDTH][height1 HEIGHT])
    
    ;; half-width for perfect bounce    
    (field [half-width1  (/ width1  HALF)])
    (field [half-height1 (/ height1 HALF)])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    
    (init-field [selected1? false])
    (init-field [box-selected1? false])
    (field [saved-mx1 BASE])     ;; used for smooth dtagging.
    (field [saved-my1 BASE])     
    (field [saved-mx2 BASE])     ;; used for stopping the ball and passing the 
    (field [saved-my2 BASE])     ;; coordinates after this event.    
    (field [new-p-x BASE])       
    (field [new-p-y BASE])
    
    (super-new)
    (send model register this)
    (set! new-p-x (particle-x p)) ;;represents the new particle y position.
    (set! new-p-y (particle-y p)) ;; represents the new particle x position.
    
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; Details :Check For the coordinates where the button down actually is
    ; STRATEGY: Cases on whether the event is in this object
    ;;Example:Used in tests.
    
    (define/override (after-button-down mx my)
      (cond
        [(in-this? mx my)
         (begin
           (set! selected1? true)
           (set! saved-mx1 (- mx x1))
           (set! saved-my1 (- my y1)))]
        
        [(in-box? mx my)
         (begin
           (set! box-selected1? true)
           (set! saved-mx2 (- mx (particle-x p)))
           (set! saved-my2 (- my (particle-y p)))
           (send model stop-tick true))]))
    
    
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    ; Strategy: Communicate via state.
    ; Example:Used in tests.
    (define/override (after-button-up mx my)
      (set! selected1? false)
      (set! box-selected1? false)
      (send model stop-tick false))
    
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    ; Strategy: Communicate via state.
    (define/override (after-drag mx my)
      (begin
        (if selected1?
            (begin
              (set! x1 (- mx saved-mx1))
              (set! y1 (- my saved-my1)))
            void)
        (if box-selected1?
            (begin
              (set! new-p-x (- mx saved-mx2))
              (set! new-p-y (- my saved-my2))
              (send
               model execute-command
               (make-particle
                new-p-x new-p-y (particle-vx p) (particle-vy p))))
            void)))     
    
    
    ;; in-box? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the canvas or not.
    
    (define/override (in-box? other-x other-y)
      (and
       (<= (+ END (- x1 half-width1)) other-x (- (+ x1 half-width1) END))
       (<= (+ END (- y1 half-height1)) other-y (-  (+ y1 half-height1) END))))
    
    
    
    ;; in-this? :Integer Integer
    ;; Given: The mouse coordinates.
    ;; Returns:The x and y coordinates for comparison with canvas coordinates.
    ;; Strategy: Combine simpler functions.
    ;; Details: It is used to check whether the coordiantes of button-down is
    ;;          inside the handle or not.
    
    (define/override (in-this? other-x other-y) 
      (and
       (<= (- x1 half-width1) other-x (+ HANDLE (- x1 half-width1)))
       (<= (- y1 half-height1) other-y (+ HANDLE (- y1 half-height1)))))
    
    ;; Testing functions
    (define/public (for-test:get-x) x1)
    (define/public (for-test:get-y) y1)
    (define/public (for-test:get-new-x) new-p-x)
    (define/public (for-test:get-new-y) new-p-y)
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/override (add-to-scene scene)
      (place-image (viewer-image) x1 y1 scene))
    
    
    ;;after-key-event: KeyEvent-> World
    ;;GIVEN:a keyevent to the world.
    ;;RETURNS:a world after the key event.
    ;;EXAMPLES:as covered in test cases
    ;;STRATEGY: cases on KeyEvent
    (define/override (after-key-event kev)this)
    
    ;;current-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;object.
    
    (define/override (current-color)
      (if selected1? RED BLACK))
    
    
    ;;box-color:A color.
    ;;Given:Color
    ;;Returns:The color.
    ;;Details: Should return the color which is based on the selection of the
    ;;box.
    (define/override (box-color) 
      void)
    
    ;; For testing
    (define/public (for-test:get-particle) p)
    (define/public (for-test:get-selected?) selected1?)
    (define/public (for-test:get-box-selected?) box-selected1?)
    
    ;;##########################################################################
    
    ;; IMAGE DEFINITIONS.
    
    ;;Ball and the particle definition.
    (define (BALL)
      (overlay (circle BALL-RADII SOLID BLACK) (circle BALL-RADIUS SOLID RED)))
    
    ;;Assemble the image of the viewer initially with the ball.
    (define/override (viewer-initial-image)
      (place-image
       (BALL) (particle-x p) (particle-y p)
       (rectangle BOUNDARY BOUNDRY OUTLINE BLUE)))
    
    ;; Assembly of the viewer initial. 
    (define (viewer-initial-image2)
      (overlay
       (viewer-initial-image)
       (rectangle BOUNDARY BOUNDRY OUTLINE BLUE)
       (rectangle WIDTH HEIGHT OUTLINE BLACK)))
    
    ;; Assemble the image with the handler.
    (define/override (viewer-image)
      (let ((initial-image (viewer-initial-image2)))
        (overlay/align LEFT TOP
                       initial-image
                       (square HANDLE OUTLINE (current-color)))))))

;;############################################################################


;; TESTS
(define particle1 (make-particle WIDTH3 HEIGHT1 BASE BASE))
(define particle-up-position (make-particle 75 45 0 0))
(define particle-down-position (make-particle 75 50 0 0))
(define particle-right-position (make-particle 80 50 0 0))
(define particle-left-position (make-particle 75 50 0 0))
(define particle-up-velocity (make-particle 75 50 0 5))
(define particle-down-velocity (make-particle 75 50 0 0))
(define particle-right-velocity (make-particle 75 50 5 0))
(define particle-left-velocity (make-particle 75 50 0 0))
(define rect1 (make-rect BASE BOUNDRY BASE BOUNDARY))
(define model1 (new Model%))
(send model1 after-tick)
(define model2 (new Model% [p particle1][rect rect1]))


(define controller1 (new PositionController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? false] [block-selected? false]
                         [saved-mx BASE] [saved-my BASE]))


(define controller2 (new VelocityController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? false] [block-selected? false]
                         [saved-mx BASE] [saved-my BASE]))

(define controller3 (new PositionController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? true] [block-selected? true]
                         [saved-mx BASE] [saved-my BASE]))

(define controller4 (new VelocityController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? true] [block-selected? false]
                         [saved-mx BASE] [saved-my BASE]))

(define controller5 (new PositionController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? false] [block-selected? false]
                         [saved-mx BASE] [saved-my BASE]))

(define controller6 (new PositionController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? true] [block-selected? false]
                         [saved-mx BASE] [saved-my BASE]))

(define controller7 (new PositionController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? false] [block-selected? true]
                         [saved-mx BASE] [saved-my BASE]))


(define controller8 (new VelocityController% [model model1]
                         [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                         [width WIDTHS] [height HEIGHT1]
                         [half-width 80] [half-height 25]
                         [selected? true] [block-selected? false]
                         [saved-mx BASE] [saved-my BASE]))

(define controller9  (new VelocityController% [model model1]
                          [x CANVAS-WIDTH2] [y CANVAS-HEIGHT2] [p particle1]
                          [width WIDTHS] [height HEIGHT1]
                          [half-width 80] [half-height 25]
                          [selected? false] [block-selected? true]
                          [saved-mx BASE] [saved-my BASE]))

(define controller10 (new XController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? false]
                          [box-selected1? false]))


(define controller11 (new YController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 50] [height1 130] [selected1? false]
                          [box-selected1? false]))

(define controller12 (new XYController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 130][selected1? false]
                          [box-selected1? false]))

(define controller13 (new XController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? true]
                          [box-selected1? false]))

(define controller14 (new YController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 50] [height1 130] [selected1? true]
                          [box-selected1? false]))

(define controller15 (new XYController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 130] [selected1? true]
                          [box-selected1? false]))

(define controller16     (new XController% [model model1]
                              [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                              [width1 180] [height1 50] [selected1? true]
                              [box-selected1? false]))
(define controller17 (new XController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? false]
                          [box-selected1? true]))
(define controller18 (new XController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? false]
                          [box-selected1? false]))
(define controller20 (new YController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? true]
                          [box-selected1? false]))
(define controller21 (new YController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? false]
                          [box-selected1? true]))
(define controller22 (new XYController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? true]
                          [box-selected1? false]))
(define controller23 (new XYController% [model model1]
                          [x1 CANVAS-WIDTH2] [y1 CANVAS-HEIGHT2] [p particle1]
                          [width1 180] [height1 50] [selected1? false]
                          [box-selected1? true]))



;;Testing: 
(begin-for-test
  (begin
    (check-equal?
     (is-a?
      model1
      Model%)
     true
     "The Value Given must be true")
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    (send model2 after-tick)
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    (send model2 stop-tick true)
    (send model2 after-tick)
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    (send model2 register controller1)
    (check-equal?
     (length (send model2 for-test:get-controllers))
     1  
     "The value is not same")
    (send model2 execute-command particle1)
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    
    ))
(define (data-image-test)
  (above
   (text "Arrow-Keys : Change Position" FONT-SIZE BLACK)
   (text (string-append
          "X = "
          (number->string (particle-x particle1))
          " Y = "
          (number->string (particle-y particle1))
          " VX = "
          (number->string (particle-vx particle1))
          " VY = "
          (number->string (particle-vy particle1)))
         FONT-SIZE
         BLACK))) 

(define (viewer-initial-image-test)
  (let ((the-data-image (data-image-test)))
    (overlay  
     the-data-image
     (rectangle  
      WIDTHS
      HEIGHT1 
      OUTLINE 
      BLACK))))

(define (viewer-image-test) 
  (let ((initial-image (viewer-initial-image-test)))
    (overlay/align LEFT TOP
                   initial-image
                   (square FONT-SIZE OUTLINE BLACK))))
(define (final-image)
  (place-image (viewer-image-test) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (data-image-test2)
  (above
   (text "Arrow-Keys : Change Position" FONT-SIZE RED) 
   (text (string-append
          "X = "
          (number->string (particle-x particle1))
          " Y = "
          (number->string (particle-y particle1))
          " VX = "
          (number->string (particle-vx particle1))
          " VY = "
          (number->string (particle-vy particle1)))
         FONT-SIZE
         RED))) 

(define (viewer-initial-image-test2)
  (let ((the-data-image (data-image-test2)))
    (overlay  
     the-data-image
     (rectangle  
      WIDTHS
      HEIGHT1
      OUTLINE 
      BLACK))))

(define (viewer-image-test2) 
  (let ((initial-image (viewer-initial-image-test2)))
    (overlay/align LEFT TOP
                   initial-image
                   (square FONT-SIZE OUTLINE BLACK))))
(define (final-image2)
  (place-image (viewer-image-test2) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (data-image-test3)
  (above
   (text "Arrow-Keys : Change Position" FONT-SIZE BLACK) 
   (text (string-append
          "X = "
          (number->string (particle-x particle1))
          " Y = "
          (number->string (particle-y particle1))
          " VX = "
          (number->string (particle-vx particle1))
          " VY = "
          (number->string (particle-vy particle1)))
         FONT-SIZE
         BLACK))) 

(define (viewer-initial-image-test3)
  (let ((the-data-image (data-image-test3)))
    (overlay  
     the-data-image
     (rectangle  
      WIDTHS
      HEIGHT1
      OUTLINE 
      BLACK))))

(define (viewer-image-test3) 
  (let ((initial-image (viewer-initial-image-test3)))
    (overlay/align LEFT TOP
                   initial-image
                   (square FONT-SIZE OUTLINE RED))))
(define (final-image3)
  (place-image (viewer-image-test3) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (data-image-test1)
  (above
   (text "Arrow Keys : Change velocity" FONT-SIZE BLACK)
   (text (string-append
          "X = "
          (number->string (particle-x particle1))
          " Y = "
          (number->string (particle-y particle1))
          " VX = "
          (number->string (particle-vx particle1))
          " VY = "
          (number->string (particle-vy particle1)))
         FONT-SIZE
         BLACK)))

(define (viewer-initial-image-test1)
  (let ((the-data-image (data-image-test1)))
    (overlay  
     the-data-image
     (rectangle 
      WIDTHS
      HEIGHT1
      OUTLINE 
      BLACK))))

(define (viewer-image-test1) 
  (let ((initial-image (viewer-initial-image-test1)))
    (overlay/align LEFT TOP
                   initial-image
                   (square FONT-SIZE OUTLINE BLACK))))  
(define (final-image1)
  (place-image (viewer-image-test1) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (data-image-test4)
  (above
   (text "Arrow Keys : Change velocity" FONT-SIZE RED)
   (text (string-append
          "X = "
          (number->string (particle-x particle1))
          " Y = "
          (number->string (particle-y particle1))
          " VX = "
          (number->string (particle-vx particle1))
          " VY = "
          (number->string (particle-vy particle1)))
         FONT-SIZE
         RED)))

(define (viewer-initial-image-test4)
  (let ((the-data-image (data-image-test4)))
    (overlay  
     the-data-image
     (rectangle 
      WIDTHS
      HEIGHT1
      OUTLINE 
      BLACK))))

(define (viewer-image-test4) 
  (let ((initial-image (viewer-initial-image-test4)))
    (overlay/align LEFT TOP
                   initial-image
                   (square FONT-SIZE OUTLINE BLACK))))  
(define (final-image4)
  (place-image (viewer-image-test4) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))


(define (data-image-test5)
  (above
   (text "Arrow Keys : Change velocity" FONT-SIZE BLACK)
   (text (string-append
          "X = "
          (number->string (particle-x particle1))
          " Y = "
          (number->string (particle-y particle1))
          " VX = "
          (number->string (particle-vx particle1))
          " VY = "
          (number->string (particle-vy particle1)))
         FONT-SIZE
         BLACK)))

(define (viewer-initial-image-test5)
  (let ((the-data-image (data-image-test5)))
    (overlay  
     the-data-image
     (rectangle 
      WIDTHS
      HEIGHT1
      OUTLINE 
      BLACK))))

(define (viewer-image-test5) 
  (let ((initial-image (viewer-initial-image-test5)))
    (overlay/align LEFT TOP
                   initial-image
                   (square FONT-SIZE OUTLINE RED))))  
(define (final-image5)
  (place-image (viewer-image-test5) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))


(define (BALL)
  (overlay (circle BALL-RADII SOLID BLACK) (circle BALL-RADIUS SOLID RED)))

(define (viewer-initial-image6)
  (place-image
   (BALL) (particle-x particle1) (particle-y particle1)
   (rectangle BOUNDARY BOUNDRY OUTLINE BLUE)))


(define (viewer-initial-image61)
  (overlay
   (viewer-initial-image6)
   (rectangle BOUNDARY BOUNDRY OUTLINE BLUE)
   (rectangle WIDTH HEIGHT OUTLINE BLACK)))


(define (viewer-image6)
  (let ((initial-image (viewer-initial-image61)))
    (overlay/align LEFT TOP
                   initial-image
                   (square HANDLE OUTLINE BLACK))))



(define (final-image6)
  (place-image (viewer-image6) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))



(define (viewer-initial-image9)
  (place-image
   (BALL) (particle-x particle1) (particle-y particle1)
   (rectangle BOUNDARY BOUNDRY OUTLINE BLUE)))


(define (viewer-initial-image91)
  (overlay
   (viewer-initial-image9)
   (rectangle BOUNDARY BOUNDRY OUTLINE BLUE)
   (rectangle WIDTH HEIGHT OUTLINE BLACK)))


(define (viewer-image9)
  (let ((initial-image (viewer-initial-image91)))
    (overlay/align LEFT TOP
                   initial-image
                   (square HANDLE OUTLINE RED))))

(define (final-image9)
  (place-image (viewer-image9) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (viewer-initial-image7)
  (place-image
   (BALL) (particle-x particle1) PARTICLE-Y
   (rectangle BOUNDARY HEIGHT1 OUTLINE BLUE)))

;;Assembly of the viewer
(define (viewer-initial-image71)
  (overlay
   (viewer-initial-image7)
   (rectangle BOUNDARY HEIGHT1 OUTLINE BLUE)
   (rectangle WIDTH HEIGHT1 OUTLINE BLACK)))

;; assemble the image with the handler
(define (viewer-image7)
  (let ((initial-image (viewer-initial-image71)))
    (overlay/align LEFT TOP
                   initial-image
                   (square HANDLE OUTLINE BLACK))))

(define (final-image7)
  (place-image (viewer-image7) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (viewer-initial-image10)
  (place-image
   (BALL) (particle-x particle1) PARTICLE-Y
   (rectangle BOUNDARY HEIGHT1 OUTLINE BLUE)))

;;Assembly of the viewer
(define (viewer-initial-image101)
  (overlay
   (viewer-initial-image10)
   (rectangle BOUNDARY HEIGHT1 OUTLINE BLUE)
   (rectangle WIDTH HEIGHT1 OUTLINE BLACK)))

;; assemble the image with the handler
(define (viewer-image10)
  (let ((initial-image (viewer-initial-image101)))
    (overlay/align LEFT TOP
                   initial-image
                   (square HANDLE OUTLINE RED))))

(define (final-image10)
  (place-image (viewer-image10) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))


(define (viewer-initial-image8)
  (place-image
   (BALL) PARTICLE-X (particle-y particle1) 
   (rectangle WIDTH1 BOUNDRY OUTLINE BLUE))) 

;;Assembly of the viewer
(define (viewer-initial-image81)
  (overlay
   (viewer-initial-image8)
   (rectangle WIDTH1 BOUNDRY OUTLINE BLUE)
   (rectangle WIDTH1 HEIGHT OUTLINE BLACK)))

;; assemble the image with the handler
(define (viewer-image8)
  (let ((initial-image (viewer-initial-image81)))
    (overlay/align LEFT TOP
                   initial-image
                   (square HANDLE OUTLINE BLACK))))

(define (final-image8)
  (place-image (viewer-image8) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define (viewer-initial-image11)
  (place-image
   (BALL) PARTICLE-X (particle-y particle1) 
   (rectangle WIDTH1 BOUNDRY OUTLINE BLUE))) 

;;Assembly of the viewer
(define (viewer-initial-image111)
  (overlay
   (viewer-initial-image11)
   (rectangle WIDTH1 BOUNDRY OUTLINE BLUE)
   (rectangle WIDTH1 HEIGHT OUTLINE BLACK)))

;; assemble the image with the handler
(define (viewer-image11)
  (let ((initial-image (viewer-initial-image111)))
    (overlay/align LEFT TOP
                   initial-image
                   (square HANDLE OUTLINE RED))))

(define (final-image11)
  (place-image (viewer-image11) CANVAS-WIDTH2 CANVAS-HEIGHT2 EMPTY-CANVAS))

(define w (new World% [canvas-width 600] [canvas-height 500]
               [model model1]))

(define ControllerFactory1 (new ControllerFactory% [w w] [m model1])) 




;;Testing: 
(begin-for-test
  (begin
    (check-equal? 
     (send controller1 add-to-scene EMPTY-CANVAS) 
     (final-image)
     "The image displayed is not proper")
    (send ControllerFactory1 after-key-event "v")
    (check-equal? 
     (send controller6 add-to-scene EMPTY-CANVAS) 
     (final-image2)
     "The image displayed is not proper")
    (check-equal? 
     (send controller7 add-to-scene EMPTY-CANVAS) 
     (final-image3)
     "The image displayed is not proper")
    (check-equal? 
     (send ControllerFactory1 add-to-scene EMPTY-CANVAS) 
     EMPTY-CANVAS
     "The image displayed is not proper")
    (check-equal? 
     (send controller2 add-to-scene EMPTY-CANVAS) 
     (final-image1)
     "The image displayed is not proper")
    (check-equal? 
     (send controller8 add-to-scene EMPTY-CANVAS) 
     (final-image4)
     "The image displayed is not proper")
    (check-equal? 
     (send controller9 add-to-scene EMPTY-CANVAS) 
     (final-image5)
     "The image displayed is not proper")
    (check-equal? 
     (send controller10 add-to-scene EMPTY-CANVAS) 
     (final-image7)
     "The image displayed is not proper")
    (check-equal? 
     (send controller11 add-to-scene EMPTY-CANVAS) 
     (final-image8)
     "The image displayed is not proper")
    (send ControllerFactory1 after-key-event "p")
    (check-equal? 
     (send controller12 add-to-scene EMPTY-CANVAS) 
     (final-image6)
     "The image displayed is not proper")
    (check-equal? 
     (send controller15 add-to-scene EMPTY-CANVAS) 
     (final-image9)
     "The image displayed is not proper")
    (send ControllerFactory1 after-tick)
    (check-equal? 
     (send controller13 add-to-scene EMPTY-CANVAS) 
     (final-image10)
     "The image displayed is not proper")
    (check-equal? 
     (send controller14 add-to-scene EMPTY-CANVAS) 
     (final-image11)
     "The image displayed is not proper")
    
    (send ControllerFactory1 after-button-down 10 20)
    (send controller3 after-key-event UP)
    (check-equal?
     (send controller3 for-test:get-particle)
     particle-up-position
     "The key event did not work properly")
    (send controller3 after-key-event DOWN)
    (check-equal?
     (send controller3 for-test:get-particle)
     particle-down-position
     "The key event did not work properly")
    (send ControllerFactory1 after-button-up 10 20)
    (send controller3 after-key-event RIGHT)
    (check-equal?
     (send controller3 for-test:get-particle)
     particle-right-position
     "The key event did not work properly")
    (send controller3 after-key-event LEFT)
    (check-equal?
     (send controller3 for-test:get-particle)
     particle-left-position
     "The key event did not work properly")
    (send controller1 after-key-event INVALID-KEY-EVENT)
    (check-equal?
     (send controller1 for-test:get-particle)
     particle1 
     "The key event did not work properly")
    
    (send ControllerFactory1 after-drag 10 20)
    (send controller4 after-key-event UP)
    (check-equal?
     (send controller4 for-test:get-particle)
     particle-up-velocity
     "The key event did not work properly")
    (send ControllerFactory1 after-key-event "z")
    (send controller4 after-key-event DOWN)
    (check-equal?
     (send controller4 for-test:get-particle)
     particle-down-velocity
     "The key event did not work properly")
    (send controller4 after-key-event RIGHT)
    (check-equal?
     (send controller4 for-test:get-particle)
     particle-right-velocity
     "The key event did not work properly")
    (send controller4 after-key-event LEFT)
    (check-equal?
     (send controller2 for-test:get-particle)
     particle-left-velocity
     "The key event did not work properly")
    (send controller2 after-key-event INVALID-KEY-EVENT)
    (check-equal?
     (send controller4 for-test:get-particle)
     particle1
     "The key event did not work properly")
    (send controller5 after-button-down 230 230)
    (check-equal?
     (send controller5 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    (send controller5 after-button-down 230 225)
    (check-equal?
     (send controller5 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    (send controller5 after-button-down 10 10)
    (check-equal?
     (send controller5 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    (send ControllerFactory1 after-key-event "x")
    (send controller5 after-button-down 230 230)
    (check-equal?
     (send controller5 for-test:get-block-selected?)
     true
     "after-button-down function did not work properly")
    (send controller5 after-button-down 10 10)
    (send controller11 box-color)
    (check-equal?
     (send controller5 for-test:get-block-selected?)
     true
     "after-button-down function did not work properly")
    (send controller5 after-button-down 230 245) 
    (send controller2 after-button-down 230 230)
    (check-equal?
     (send controller2 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    (check-equal?
     (send controller2 for-test:get-block-selected?)
     true
     "after-button-down function did not work properly")
    (send controller12 box-color)
    (send controller2 after-button-down 230 245)
    (check-equal?
     (send controller2 for-test:get-block-selected?)
     true
     "after-button-down function did not work properly")
    (send controller10 box-color)
    (check-equal?
     (send controller2 for-test:get-selected?)
     true
     "after-button-down function did not work properly")
    (send controller10 after-key-event UP)
    (check-equal?
     (send controller10 for-test:get-particle)
     particle1
     "The after-key-event did not work properly")
    (send ControllerFactory1 after-key-event "y")
    (send controller11 after-key-event UP)
    (check-equal?
     (send controller11 for-test:get-particle)
     particle1
     "The after-key-event did not work properly")
    (send controller12 after-key-event UP)
    (check-equal?
     (send controller12 for-test:get-particle)
     particle1
     "The after-key-event did not work properly")
    (send controller10 after-button-down 10 10) 
    (check-equal?
     (send controller10 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    
    (send controller16 after-button-down 170 280)
    (check-equal?
     (send controller16 for-test:get-selected?)
     true
     "after-button-down function did not work properly")
    (send controller17 after-button-down 220 310)
    (check-equal?
     (send controller17 for-test:get-box-selected?)
     true
     "after-button-down function did not work properly")
    (send controller10 after-button-down 269 259)
    (check-equal?
     (send controller10 for-test:get-selected?)
     false
     "Returns true for button down")
    
    (send controller10 after-button-down 220 310)
    (check-equal?
     (send controller10 for-test:get-box-selected?)
     true
     "Returns true for button down")
    (send controller10 after-button-down 220 310)
    (check-equal?
     (send controller10 for-test:get-box-selected?)
     true
     "the function did not work properly")
    (send controller10 after-button-down 269 259)
    (check-equal?
     (send controller10 for-test:get-selected?)
     false
     "the function did not worked properly")
    
    (send controller16 after-button-up 170 280)
    (check-equal?
     (send controller16 for-test:get-selected? )
     false
     "Not selected after button up")
    (send controller17 after-drag 220 310)
    (check-equal?
     (send controller17 for-test:get-x)
     300
     "after-drag function did not work properly")
    (check-equal?
     (send controller17 for-test:get-y)
     250
     "after-drag function did not work properly")
    (check-equal?
     (send controller17 for-test:get-new-x)
     220
     "after-drag function did not work properly")
    (check-equal?
     (send controller17 for-test:get-new-y)
     310
     "after-drag function did not work properly")
    (send controller16 after-drag 170 280)
    (check-equal?
     (send controller16 for-test:get-x)
     300
     "after-drag function did not work properly")
    (check-equal?
     (send controller16 for-test:get-y)
     250
     "after-drag function did not work properly")
    
    
    
    
    
    
    (send controller21 after-drag 220 310)
    (check-equal?
     (send controller21 for-test:get-x)
     300
     "after-drag function did not work properly")
    (check-equal?
     (send controller21 for-test:get-y)
     250
     "after-drag function did not work properly")
    (check-equal?
     (send controller21 for-test:get-new-x)
     220
     "after-drag function did not work properly")
    (check-equal?
     (send controller21 for-test:get-new-y)
     310
     "after-drag function did not work properly")
    (send controller20 after-drag 170 280)
    (check-equal?
     (send controller20 for-test:get-x)
     170
     "after-drag function did not work properly")
    (check-equal?
     (send controller20 for-test:get-y)
     280
     "after-drag function did not work properly")
    
    
    
    (send controller23 after-drag 220 310)
    (check-equal?
     (send controller23 for-test:get-x)
     300
     "after-drag function did not work properly")
    (check-equal?
     (send controller23 for-test:get-y)
     250
     "after-drag function did not work properly")
    (check-equal?
     (send controller23 for-test:get-new-x)
     220
     "after-drag function did not work properly")
    (check-equal?
     (send controller23 for-test:get-new-y)
     310
     "after-drag function did not work properly")
    (send controller22 after-drag 170 280)
    (check-equal?
     (send controller22 for-test:get-x)
     170
     "after-drag function did not work properly")
    (check-equal?
     (send controller22 for-test:get-y)
     280
     "after-drag function did not work properly")
    
    
    
    (send controller11 after-button-down 10 10) 
    (check-equal?
     (send controller11 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    
    (send controller20 after-button-down 170 280)
    (check-equal?
     (send controller20 for-test:get-selected?)
     true
     "after-button-down function did not work properly")
    (send controller21 after-button-down 220 310)
    (check-equal?
     (send controller21 for-test:get-box-selected?)
     true
     "after-button-down function did not work properly")
    (send controller11 after-button-down 269 259)
    (check-equal?
     (send controller11 for-test:get-selected?)
     false
     "Returns true for button down")
    
    
    (send controller11 after-button-down 220 310)
    (check-equal?
     (send controller11 for-test:get-box-selected?)
     false
     "Returns true for button down")
    (send controller11 after-button-down 220 310)
    (check-equal?
     (send controller11 for-test:get-box-selected?)
     false
     "the function did not work properly")
    (send controller11 after-button-down 269 259)
    (check-equal?
     (send controller11 for-test:get-selected?)
     false
     "the function did not worked properly")
    (send controller20 after-button-up 170 280)
    (check-equal?
     (send controller20 for-test:get-selected? )
     false
     "Not selected after button up")
    
    
    
    
    (send controller12 after-button-down 10 10) 
    (check-equal?
     (send controller12 for-test:get-selected?)
     false
     "after-button-down function did not work properly")
    
    (send controller22 after-button-down 170 280)
    (check-equal?
     (send controller22 for-test:get-selected?)
     true
     "after-button-down function did not work properly")
    (send controller23 after-button-down 220 310)
    (check-equal?
     (send controller23 for-test:get-box-selected?)
     true
     "after-button-down function did not work properly")
    (send controller12 after-button-down 269 259)
    (check-equal?
     (send controller12 for-test:get-selected?)
     false
     "Returns true for button down")
    
    (send controller12 after-button-down 220 310)
    (check-equal?
     (send controller12 for-test:get-box-selected?)
     true
     "Returns true for button down")
    (send controller12 after-button-down 220 310)
    (check-equal?
     (send controller11 for-test:get-box-selected?)
     false
     "the function did not work properly")
    (send controller12 after-button-down 269 259)
    (check-equal?
     (send controller12 for-test:get-selected?)
     false
     "the function did not worked properly")
    (send controller22 after-button-up 170 280)
    (check-equal?
     (send controller22 for-test:get-selected? )
     false
     "Not selected after button up")))
