;;The entire system works on a 600 x 500 canvas.
;;Hitting one of the following keys causes a new controller to appear in the
;;center of the canvas:
;;"p" : Position controller
;;"v" : velocity controller
;;"x" : X controller
;;"y" : Y controller
;;"z" : XY controller
;;Each controller has a 10x10 handle.Dragging on the handle moves the controller
;;around the canvas.
;;A button-down inside a controller selects the controller for input.
;;In the position or velocity controller, the arrow keys are used for input.
;;The arrow keys alter the position or velocity of the particle in the indicated
;;direction. Each press of an arrow key alters the appropriate quantity by 5.
;;In the X, Y, or XY controller, the mouse drags the particle via smooth drag.
;;The mouse need not be in the representation of the particle
;;it need only be in the controller.

#lang racket
;;Inherited Files.
(require "mvctester.rkt")       
(require "World.rkt")


;#######################################################################  

;;run : PosReal -> Void
;;GIVEN: a frame rate, in sec/tick
;;EFFECT: Creates and runs the MVC simulation with the given frame rate. 

(define (run frame-rate)
  (let* ((m (new Model%))
         (w (make-world m 600 500))) 
    (begin
      (send w add-widget 
            (new ControllerFactory% [m m][w w]))
      (send w run frame-rate)))) 
;########################################################################




