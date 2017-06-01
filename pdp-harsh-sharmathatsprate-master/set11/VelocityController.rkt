#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocity of the particle.

;; the rectangle is draggable

;; "up","down","right","left" increments or decrements the speed of the particle

;;Required Functions:
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "UniversalController.rkt")

;;Provided Function:
(provide VelocityController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Constants:
(define BASE 5)
(define HANDLE 10)
(define RIGHT "right")
(define LEFT "left")
(define UP "up")
(define DOWN "down")
(define TOP "top")
(define OUTLINE "outline")
(define BLACK "black")
(define RED "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    
    (define/override (in-this? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ 10 (- x half-width)))
       (<= (- y half-height) other-y (+ 10 (- y half-height))))) 

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
                   (+ (particle-vy p) BASE)))]

          [(key=? DOWN kev)
           (send model execute-command
                 (make-particle
                   (particle-x p)
                   (particle-y p) 
                   (particle-vx p)
                   (- (particle-vy p) BASE)))]

          [(key=? RIGHT kev)
           (send model execute-command
                 (make-particle
                   (particle-x p)
                   (particle-y p)
                   (+ (particle-vx p) BASE)
                   (particle-vy p)))]

          [(key=? LEFT kev)
           (send model execute-command
                 (make-particle
                   (particle-x p)
                   (particle-y p)
                   (- (particle-vx p) BASE)
                   (particle-vy p)))])
        void))


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

    (define/override (box-color)
     (if block-selected? RED BLACK))


    ;;#####################################################################

    ;;Image Definitions:
    
    ;; Viewer-initial-image->Image.
    ;; Given: An Image.
    ;; Returns:The image.
    ;; Details:Assemble the image of the viewer initially.
    ;; Strategy:Combine Simpler Functions.
    
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