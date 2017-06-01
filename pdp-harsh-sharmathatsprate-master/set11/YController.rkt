#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocity of the particle.

;; the rectangle is draggable

;; Arrow Keys increments or decrements location of the particle by 5

;;Required Files.

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "UniversalController.rkt")
(require rackunit)

;;Provided Functions.
(provide YController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Constants:
(define HANDLE 10)
(define LEFT "left")
(define Right "right")
(define TOP "top")
(define BLACK "black")
(define BLUE "blue")
(define RED "red")
(define OUTLINE "outline")
(define SOLID "solid")
(define BALL-RADII 2)
(define BALL-RADIUS 7)
(define PARTICLE-X 25)
(define HEIGHT2 150)
(define BASE 0)
(define HALF 2)
(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 250)
(define WIDTH 180)
(define HEIGHT 130)
(define BOUNDARY 100)
(define WIDTH1 50)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a PositionController% is a (new PositionController% [model Model<%>])

(define YController%
  (class* UniversalController% (Controller<%>)
    
    
    ; Nats -- the position of the center of the controller
    (init-field [x1 CANVAS-HEIGHT] [y1 CANVAS-WIDTH])
    (inherit-field model p)
    (init-field [width1 WIDTH1][height1 HEIGHT]) 
    ;; half width for perfect bounce        
    (field [half-width1  (/ width1  HALF)])
    (field [half-height1 (/ height1 HALF)])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (field [selected1? false])
    (field [box-selected1? false])
    (field [saved-mx1 BASE])             ;;used For Smooth Dragging.(x)
    (field [saved-my1 BASE])             ;;used For Smooth Dragging.(y)


    (super-new)

    (send model register this)
    
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; STRATEGY: Cases on whether the event is in this object
   
    (define/override (after-button-down mx my)
      (if (in-this? mx my)
          (begin
            (set! selected1? true)
            (set! saved-mx1 (- mx x1))
            (set! saved-my1 (- my y1)))void))

         
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    ; Strategy: Communicate via state.
    (define/override (after-button-up mx my)
      (set! selected1? false)
      (set! box-selected1? false))
    
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    ; Strategy: Communicate via state.

    (define/override (after-drag mx my)
      (if selected1?
          (begin
            (set! x1 (- mx saved-mx1))
            (set! y1 (- my saved-my1)))
          void))
    
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
    (define/override (after-key-event kev)
      this)
    
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
     (if box-selected1? RED BLACK))


;;#########################################################################
    
    ;;Image Definition:
    
    ;;Ball and the particle definition
    (define (BALL)
      (overlay (circle BALL-RADII SOLID BLACK) (circle BALL-RADIUS SOLID RED)))

    ;;Assemble the image of the viewer initially
    (define/override (viewer-initial-image)
      (place-image
        (BALL) PARTICLE-X (particle-y p) 
        (rectangle WIDTH1 BOUNDARY OUTLINE BLUE))) 

    ;;Assembly of the viewer
    (define (viewer-initial-image2)
      (overlay
       (viewer-initial-image)
       (rectangle WIDTH1 BOUNDARY OUTLINE BLUE)
       (rectangle WIDTH1 HEIGHT OUTLINE BLACK)))

    ;; assemble the image with the handler
    (define/override (viewer-image)
      (let ((initial-image (viewer-initial-image2)))
      (overlay/align LEFT TOP
                     initial-image
                     (square HANDLE OUTLINE (current-color)))))))

;;##########################################################################



