#lang racket
;;Required Files.
(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XYController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require 2htdp/universe)
(provide ControllerFactory%)


;; The Controller factory impletemts the swidget interface.

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
        [(key=? kev "v") (add-viewer VelocityController%)]
        [(key=? kev "p") (add-viewer PositionController%)]
        [(key=? kev "z") (add-viewer XYController%)]
        [(key=? kev "x") (add-viewer XController%)]
        [(key=? kev "y") (add-viewer YController%)]))
    
    ;;add-viewer: Class% ->Class%
    ;;Given: Any class.
    ;;Returns :Adds a viewer to any class on the keyevent.
    ;;Strategy: Use simple function.
    ;;Example: When an add-viewer is used and a class is given in accordance the
    ;;         viewer gets added to that respective class.

    (define/public (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))

    ;;add-to-scene :Scene -> Scene
    ;;Given: A scene.
    ;;Returns: A scene with the scene rendered on it.
   
    (define/public (add-to-scene s) s)

     ;;after-tick :Scene->void
     ;;EFFECT: updates the controller factory to a state where should be in
     ;;after a tick.
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



