#lang racket

;; A super class for all the sub classes controller
;;Required Functions.
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require rackunit)
(require "extras.rkt")

;;The provided functions:

(provide UniversalController%)

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

;; a UniversalController% is a (new UniversalController% [model Model<%>])

(define UniversalController%
  (class* object% (Controller<%>)
    (init-field model)  ; the model
    ; Nats -- the position of the center of the controller
    (init-field [x 300] [y 250])           ;;To define The coordiantes of the canvas
    (init-field [p particle])              ;;The particle on which all the
    ;                                      ;;transitions occur.
    (init-field [width 160][height 50])
    (init-field [half-width (/ width 2)])
    (init-field [half-height (/ height 2)])
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (init-field [selected? false])
    (init-field [block-selected? false])
    (init-field [saved-mx 0])
    (init-field [saved-my 0])
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

