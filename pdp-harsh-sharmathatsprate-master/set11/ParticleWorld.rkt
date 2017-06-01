#lang racket

;;Required iles:
(require 2htdp/image)
(require 2htdp/universe)


;;Provides:
(provide World% make-world)

(define World<%>
  (interface ()
    ; SWidget<%> -> Void
    add-widget                          ; all the widgets are stateful
    ; PosReal -> Void
    run
    ))

;; A World is a (make-world model canvas-width canvas-height) 

(define (make-world m w h)
  (new World%
    [model m]
    [canvas-width w]
    [canvas-height h]))

;; A World% consists of a model and some stateful widgets.  
;; it distributes after-tick to the model and to the controllers
;; it distributes mouse events and keyboard events to the controllers
;; with for-each.
;; it distributes add-to-scene to the controllers, and collects the
;; results with foldr

(define World% 
  (class* object%
    (World<%>)

    (init-field canvas-width)
    (init-field canvas-height)
    ;; the model, initialized to a garbage value
    (init-field model) ; [model false])     ; MaybeModel     
    (init-field [widgets empty])   ; ListOfSWidget

    ;; (Widget -> Void) -> Void
    (define (for-each-widget fn)
      (for-each fn widgets))

    ;; (Widget Y -> Y) Y ListOfWidget -> Y
    (define (foldr-widgets fn base)
      (foldr fn base widgets))

    (super-new)

    ;;empty-canvas represents the canvas with nothing drawn on it.
    ;;Given:Canvas width and Canvas Height.
    ;;Returns:The world with an empty canvas.
    ;;Strategy:Use simpler dunctions.
    
    (define empty-canvas (empty-scene canvas-width canvas-height))

    ;;run : PosReal -> Void
    ;;GIVEN: a frame rate, in sec/tick
    ;;EFFECT: Creates and runs the MVC simulation with the given frame rate.
    
    ;; BIG BANG EXPRESSION.
    
    ;; (big-bang (state-expr clause ...)
    ;; a big-bang expression returns the last world when the
    ;; stop condition is satisfied.
    ;; big-bang is responsible to run the animation.
    ;; on-draw calls world to scene when we need to draw the scene on  screen.
    ;; after-key-event responses to the keyevent whenever the keyevent occurs.

    (define/public (run rate)
      (big-bang this
        (on-tick
          (lambda (w) (begin (after-tick) w))
          rate)
        (on-draw
          (lambda (w) (to-scene)))
        (on-key
          (lambda (w kev)
            (begin
              (after-key-event kev)
              w)))
        (on-mouse
          (lambda (w mx my mev)
            (begin
              (after-mouse-event mx my mev)
              w)))))

    ;;after-tick: ->World%
    ;;RETURNS:A world after the tick.
    ;;EXAMPLES:as covered in test cases

    (define (after-tick)
      (send model after-tick)
      (for-each-widget
        (lambda (c) (send c after-tick))))
    

    ;;after-key-event: Keyevent->World%
    ;;RETURNS:A world after the key.
    ;;EXAMPLES:as covered in test cases
    
    (define (after-key-event kev)
      (for-each-widget
        (lambda (c) (send c after-key-event kev))))

    
    ;;to-scene: ->World%
    ;;RETURNS:A world with the scene rendered on it..
    ;;EXAMPLES:as covered in test cases

    (define (to-scene)
      (foldr-widgets
       (lambda (widget scene) (send widget add-to-scene scene))
       empty-canvas))

    
    ;;after-mouse-event:Int Int Mev ->World%
    ;;RETURNS:A world after the mouse event.
    ;;EXAMPLES:as covered in test cases
    ;;Details: demux the mouse event and send button-down/drag/button-up
    ;; events to each widget
    (define (after-mouse-event mx my mev)
      (for-each-widget 
        (mouse-event->message mx my mev)))

    ;; Nat Nat MouseEvent -> (Widget -> Void)
    ;;after-mouse-event:Int Int MouseEvent -> WorldState
    ;;GIVEN :Mouse co-ordinates and a mouse event.
    ;;RETURNS: World after mouse event.
    ;;EXAMPLES: as covered in test cases
    ;;Strategy:Use cases  on mouse event.
    (define (mouse-event->message mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (lambda (obj) (send obj after-button-down mx my))]
        [(mouse=? mev "drag")
         (lambda (obj) (send obj after-drag mx my))]
        [(mouse=? mev "button-up")
         (lambda (obj) (send obj after-button-up mx my))]
        [else (lambda (obj) 'dont-look-at-this-126)]))


    ;;add-widget: widget ->Widgets
    ;;Given A widget
    ;;Returns :The widget added to list of widgets.
    ;;Strategy:Combinesimpler functions.
    ;;Examples:As covered in test cases.
    (define/public (add-widget c)
      (set! widgets (cons c widgets)))))

;;#########################################################################