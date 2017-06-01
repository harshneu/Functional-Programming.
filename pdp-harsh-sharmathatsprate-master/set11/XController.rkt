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

;;Provided Functions.
(provide XController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define PARTICLE-Y 25)
(define WIDTH2 150)
(define BASE 0)
(define HALF 2)
(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 250)
(define WIDTH 180)
(define HEIGHT 50)
(define ENDX 11.5)
(define ENDY 14)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; a PositionController% is a (new PositionController% [model Model<%>])

(define XController%
  (class* UniversalController% (Controller<%>)
    ; Nats -- the position of the center of the controller
    (inherit-field model p)
    (init-field [x1 CANVAS-HEIGHT] [y1 CANVAS-WIDTH]) 
    (init-field [width1 WIDTH][height1 HEIGHT]) 
    ;; half-width with perfect bounce           
    (field [half-width1  (/ width1  HALF)])
    (field [half-height1 (/ height1 HALF)])
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (field [selected1? false])         ;;will work for the handle seelection. 
    (field [box-selected1? false])     ;;will work for the entire block
    (field [saved-mx1 BASE])           
    (field [saved-my1 BASE])
    (super-new)
    
    (send model register this)
    
    ;;#####################################################################

    ;;Mouse Functions.
    
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
      (if box-selected1? RED BLACK))
    
 ;;#####################################################################

    ;;Image Definitions:
    
    
    ;;Ball and the particle definition
    (define (BALL)
      (overlay (circle BALL-RADII SOLID BLACK) (circle BALL-RADIUS SOLID RED)))

    ;;Assemble the image of the viewer initially
    (define/override (viewer-initial-image)
      (place-image
       (BALL) (particle-x p) PARTICLE-Y
       (rectangle WIDTH2 HEIGHT OUTLINE BLUE)))

    ;;Assembly of the viewer
    (define (viewer-initial-image2)
      (overlay
       (viewer-initial-image)
       (rectangle WIDTH2 HEIGHT OUTLINE BLUE)
       (rectangle WIDTH HEIGHT OUTLINE BLACK)))

    ;; assemble the image with the handler
    (define/override (viewer-image)
      (let ((initial-image (viewer-initial-image2)))
        (overlay/align LEFT TOP
                       initial-image
                       (square HANDLE OUTLINE (current-color)))))))


;;#########################################################################




