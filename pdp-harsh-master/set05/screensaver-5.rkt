;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname screensaver-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)          ;;testing framework.
(require "extras.rkt")

(require 2htdp/universe)    ;;module to use various inbuilt library functions.
;;such as big-bang,key-events?.

(require 2htdp/image)       ;;module with inbuilt library functions
;;like building shapes and structures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide
 screensaver
 initial-world
 world-after-tick          
 world-after-key-event
 world-rects
 rect-after-key-event
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 rect-selected?
 world-after-mouse-event
 rect-after-mouse-event
 rect-pen-down?)

;;Data Definition:

;;World:
(define-struct world (mx my rects locus paused?))
;;Constructor template:
;;A world is a (make-world Int Int ListOfRectangles ListOfPoints Boolean) 

;;Interpretation:
;;Interp: mx is the x-coordinte for the mouseevent.
;;Interp: my is the y-coordinte for the mouseevent.
;;Interp: Rects is the rectangle list that are created in the canvas.
;;Interp: Locus is the list of rectangle that are made in the canvas.
;;Interp: paused? gives a boolean result to check
;;whether the world is paused or not.

;;Template:

;;world-fn:World->??.
;;(define (world-fn n)
;  (... (world-rects n)(world-locus n)(world n)(world-paused? n))



;;Point
(define-struct point (x y))

;;Constructor Template:
;;A point is a (make-point int int) 

;;Interpretation:
;;Interp:x is the vertex we should have our circle placed at x coordinate.
;;Interp:y is the vertex we should have our circle placed at  y coordinate.

;;Template:
;;point-fn:point->??
;;define (point fn n)
;;(...
;;  (...(x)(y)) 


;;Rect:
(define-struct rect (x y vx vy selected? pen-down?))
;;A rect is a (make-struct Int Int Int Int Boolean) 
;;Interp:x is the x coordinate of the rectangle.
;;Interp:y is the y coordinate of the rectangle.
;;Interp:vx is the x coordinate velocity of rectangle
;;Interp:vy is the x coordinate velocity of rectangle
;;selected? is the condition that shall return a boolean result of whether
;;       a rectangles are selected or not.

;;Template:

;;rect-fn:rect->??.
;;(define (rect-fn n)
;;(...
;  (... (x)(y)(vx) (vy) (boolean?) (boolean?))



;;CONSTANTS:
(define HALF-WORLD-WIDTH 200)
(define HALF-WORLD-HEIGHT 150)
(define TEXT-FONT 10)
(define LEFT-BOUNDARY 32)
(define RIGHT-BOUNDARY 368)
(define BOTTOM-BOUNDARY 273)
(define TOP-BOUNDARY 27)
(define VEL-X -12)
(define VEL-Y 20)
(define VEL-X1 23)
(define VEL-Y1 -14)
(define RECT-HEIGHT 60)
(define RECT-WIDTH 50)
(define HALF-RECT-HEIGHT 30)
(define HALF-RECT-WIDTH 25)
(define VELO-CHANGE 2)
(define RADIUS 5)
(define RECTS-1 (make-rect  100 200 20 40 false false))
(define RECTS-4 (make-rect  100 200 20 40 false true))
(define LOCUS-1 (make-point 10 10))
(define RECTS-2 (make-rect 106 200 20 40 true true))
(define WORLD-BEFORE-BUTTON-UP (make-world 0 0 RECTS-1 LOCUS-1 false))
(define WORLD-AFTER-BUTTON-UP (make-world 0 0 (make-rect  100 200 20 40
                                                          false true)
                                          LOCUS-1 false))
(define WORLD-BEFORE-BUTTON-DOWN (make-world 0 0 LOCUS-1 RECTS-2 true))
(define WORLD-AFTER-BUTTON-DOWN
  (make-world 0 0 (make-rect   100 200 20 40 true false)
              LOCUS-1 false ))
(define RECT-1 (cons (make-rect 10 10 10 10 #true #false)  empty))
(define LOCUS-2 (cons (make-point 10 10) empty))
(define RECT-4 (cons (make-rect 10 10 10 10 #true #true)  empty))
(define RECT-2 (cons(make-rect 10 10 10 10 #false #false) empty))
(define RECT-3 (cons(make-rect   200 100 20 30 true true) empty))
(define WORLD-1 (make-world 300 300 RECT-1 LOCUS-1 true))
(define WORLD-2 (make-world 300 300 RECT-1 LOCUS-1 false))
(define ICON-RECTANGLE (make-rect 100 100 -12 -15 true true))
(define WORLD-ICON (make-world 100 100 (cons ICON-RECTANGLE empty) empty true))
(define RECT-UNSELECTED (make-rect 100 100 -14 -15 false true))
(define WORLD-UNSELECTED
  (make-world 100 100 (cons RECT-UNSELECTED empty) empty true))
(define CIRCLE1 (circle 2 "solid" "yellow"))

;;dimensions of the canvas.

;;we determine the dimensions of the canvas(height and width) which shall
;;act as the boundary for our screensaver.

;; dimensions should comprise of:
;;;;Canvas Height
;;;;Canvas Width
;;;;Interp:Self Evident.

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;; initial-world : Any->WorldState.
;; GIVEN:          any input
;; RETURNS: the initial world specified in the problem set.
;;          here we start as the rectangles in the positions(0,0)
;;          both in a paused state.
;;          true boolean in initial-world function signifies that the
;;          rectangle is paused.
;;Interp:   Empty Corresponds to the empty list that must be returned
;;if there is no element in the list.
;;Strategy:Use template on world w.
;;Examples: 
;;(initial-world 8)
;;(make-world 200 150 '() #true)
;;(initial-world 10)
;;(make-world 200 150 '() #true)

(define (initial-world n)
  (make-world HALF-WORLD-WIDTH HALF-WORLD-HEIGHT empty empty true))

;;Tests:
(begin-for-test
  (check-equal? (initial-world 8)
                (make-world 200 150 '() '() #true)
                "returns a world"))

;; initial-rect : Any->WorldState.
;; GIVEN:          any input
;; RETURNS: the initial rect specified in the problem set.
;;          here we start as the rectangles in the positions(0,0)
;;          both in a paused state.
;;          true boolean in initial-world function signifies that the
;;          rectangle is paused.
;;Strategy: Use template on rect r
;;Examples:
; initial-rect (make-rect 200 150  0  0 false false)
;(make-rect 200 150 0 0 #false #false)
;(make-rect 200 150 0 0 #false #false)

(define initial-rect (make-rect 200 150  0  0 false false))


;; SCREENSAVER FUNCTION.

;; screensaver : PosReal -> WorldState.
;; GIVEN: the speed of the simulation, in seconds/tick.
;; EFFECT: runs the simulation, starting with the initial state as
;;         specified in the problem set.
;; RETURNS: the final state of the world
;; "speed" used with screensaver function is used to assign the speed
;; to the simulation in seconds/tick.  

;; BIG BANG EXPRESSION.

;; (big-bang (state-expr clause ...)
;; a big-bang expression returns the last world when the
;; stop condition is satisfied.
;; big-bang is responsible to run the animation.
;; on-draw calls world to scene when we need to draw the scene on the screen.
;; on-key responses to the keyevent whenever the keyevent occurs.

;;Big-Bang Expression.
;;Main Function.
;;Call the program using this function.
;;(screensaver 0.5).

(define (screensaver speed)
  (big-bang (make-world HALF-WORLD-WIDTH HALF-WORLD-HEIGHT
                        empty empty true)
            (on-tick world-after-tick speed)
            (on-key  world-after-key-event)
            (on-mouse  world-after-mouse-event)
            (on-draw world-to-scene)))



;; examples for testing
;;(define pause-key-event " ")
;;(define non-pause-key-event "p")
;;Tests:


;;END DATA DEFINITIONS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; helper function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent i.e. spacebar (" ").
;; RETURNS: true iff the KeyEvent represents a pause instruction.
;;Strategy:Use casees on key events.
;;Template:
;;key-event : KeyEvent -> ??
;;(define (key-event k)

;;Examples:
;;is-pause-key-event? " ")
;;true
;;is-pause-key-event? "p")
;;false


(define(is-pause-key-event? ke)
  (key=? ke " ")) 
(define pause-key-event " ")


;;Tests:
(begin-for-test
  (check-equal? (is-pause-key-event? " ")
                true
                "Should return a true value if paused"))




;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Strategy:Combine Simpler functions.
;;Example
;;(new-rectangle 10 10 20 20)
;;(make-rect 10 10 20 20 #false #false)

(define (new-rectangle x y vx vy )
  (make-rect x y vx vy false false))


;;Tests:
(begin-for-test
  (check-equal? (new-rectangle 10 10 10 10)
                (make-rect 10 10 10 10 #false #false)
                "Makes a rectangle and returns it"))


;;draw-rect: Rectangle String-> Rectangle
;;Given: The coordinates dimensions and otline of the rectangle.
;;Returns: the rectangle feature according to the string provided.
;;Strategy: Use a more generalfunction.
;;Examples:
;;(draw-rect RECT-1 "red")
;;(draw-rect RECT-2 "blue")

(define (draw-rect r str)
  (overlay (rectangle RECT-HEIGHT RECT-WIDTH "outline" str)
           (text (string-append (number->string (rect-vx r))
                                "," (number->string (rect-vy r)))
                 TEXT-FONT str)))

;;draw-normal-rect: Rectangle -> Rectangle
;;Given: The coordinates dimensions and outline of the rectangle.
;;Returns: the rectangle feature when in unselected state.
;;Strategy: Generalization.

(define (draw-normal-rect r)
  (draw-rect r "blue"))

;;draw-selected-rect: Rectangle -> Rectangle
;;Given: The coordinates dimensions and otline of the rectangle.
;;Returns: the rectanglefeature when in selected state.
;;Strategy: generalization.
;;Examples: Used in Test cases.

(define (draw-selected-rect r)
  (draw-rect r "red"))



;; draw-rectangle : NonNegInt NonNegInt Rect Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers mx and my
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Strategy:Combine Simpler function.


(define (draw-rectangle mx my r)
  (if(rect-selected? r)
     (draw-selected-rect r)
     (draw-normal-rect r))) 

;;Example
;;(draw-rectangle 10 10 RECT-1)
;;Tests:Used in test cases.

;;any-rect-selected: World ->Boolean
;;Given: A list of rectangle.
;;Returns:Whether a rectangle is selected or not in the given world.
;;Strategy:Use template on rect.
;;Example:(any-rect-selected? RECT-1)
;;         #true

(define(any-rect-selected? rl)
  (if (empty? rl)
      false
      (or (rect-selected? (first rl)) (any-rect-selected? (rest rl)))))

;;print-locus :ListOfPoints Point->Point
;;Given: A list of the points in the given canvas.
;;Returns:A circle in accordance to the position and the
;;events on the world-scene.
;;Strategy:Use templatr for list.
;;Example:
;;(print-locus LOCUS-2 CC)
;;             CC)

(define (print-locus locus ps)
  (cond
    [(empty? locus)
     ps]
    [else
     (draw-circle ps locus)]))

;;Tests:
(begin-for-test
  (check-equal? (print-locus LOCUS-2 CC)
                CC)
  "Returns the point for the canvas")

;;make-canvas : RectangleList->World.
;;Given: canvas and its coordinates.
;;Returns: A canvas with the world scene.
;;Strategy: Combine Simpler Function..
;;Interp: rl is a list of rectangles rl.
;;Interp: mx is the mouse event x coordinate.
;;Interp: my is the mouse event y coordinate.
;;Examples:See Tests below.
;;Strategy:Use template for list.


(define (make-canvas rl mx my )
  (if (empty? rl)
      EMPTY-CANVAS
      (place-image (draw-rectangle mx my (first rl))
                   (rect-x (first rl))
                   (rect-y (first rl))
                   (make-canvas  (rest rl)mx my))))


;;world-to-scene :World->World.
;;Given:A world.
;;Returns:Returns a world with the rectangles and the designated
;;        images.
;;Strategy: Use cases on world.
;;Examples:
;;(world-to-scene WORLD-1)
;;(world-to-scene WORLD-2)
;;See tests below.

(define(world-to-scene a)
  (if (any-rect-selected? (world-rects a))
      (print-locus (world-locus a)
                   (place-image 
                    (circle RADIUS "outline" "red")
                    (world-mx a) (world-my a)
                    (make-canvas (world-rects a) (world-mx a) (world-my a))))
      (print-locus (world-locus a) (make-canvas
                                    (world-rects a)(world-mx a)(world-my a)))))
;;Tests:
(begin-for-test
  (check-equal? (world-to-scene WORLD-ICON)
                (place-image
                 (circle RADIUS "outline" "red")
                 100 100
                 (make-canvas
                  (world-rects WORLD-ICON) 100 100))
                "Returns a world after applying world to
scene operations on selected rects"))
(check-equal? (world-to-scene WORLD-UNSELECTED)
              (make-canvas
               (world-rects WORLD-UNSELECTED) 100 100)
              "Returns world after applying worldtoscene operations on rects")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event : WorldState Integer
;;Integer MouseEvent -> WorldState
;; GIVEN: a world, the x- and y-positions of the mouse, and a mouse
;; event. 
;; RETURNS: the world that should follow the given mouse event and supports
;;          smooth dragging.
;; STRATEGY: Cases on MouseEvent
;; Example:
;;(world-after-mouse-event WORLD-1 10 10  "drag")
;;(make-world
;;10 10(list (make-rect -280 -280 10 10 #true))
;; #true)

(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

;; rect-after-mouse-event : rect Integer Integer MouseEvent -> World
;; GIVEN: a rect, the x- and y-positions of the mouse, and a mouse
;; event. 
;; RETURNS: the world that should follow the given mouse event and supports
;;          smoothh dragging.
;; STRATEGY: Cases on MouseEvent
;; Example:
;;(world-after-mouse-event RECT-1 10 10 10 "drag")
;;(make-world 10 10 (list (make-rect -280 -280 10 10 #true #false))
;;(make-point 10 10)#true).

(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down r mx my)]
    [(mouse=? mev "drag") (rect-after-drag r mx my mx my)]
    [(mouse=? mev "button-up")(rect-after-button-up r mx my)]
    [else r]))

;;Tests
(begin-for-test
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "drag")
                (make-world 10 10 (list
                                   (make-rect -280 -280 10 10 #true #false))
                            (make-point 10 10)#true)
                "Returns a world after drag event")
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "button-down")
                (make-world 10 10 (list (make-rect 10 10 10 10 #true #false))
                            (make-point 10 10)#true)
                "Returns a world after button-down event")
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "button-up")
                (make-world 10 10 (list (make-rect 10 10 10 10 #false #false))
                            (make-point 10 10)#true)
                "Returns a world after button-up event")
  (check-equal? (rect-after-mouse-event RECTS-1 10  10 "drag")
                (make-rect 100 200 20 40 #false #false)
                "Returns a world after button-down event")
  
  (check-equal?  (world-after-mouse-event WORLD-1 10 10 "leave")
                 (make-world 300 300(list (make-rect 10 10 10 10 #true #false))
                             (make-point 10 10)  #true)
                 "Returns a world after button-down event")       
  
  (check-equal? (rect-after-mouse-event RECTS-1 10  10 "button-up")
                (make-rect 100 200 20 40 #false #false)
                "Returns a world after button-down event")
  (check-equal? (rect-after-mouse-event RECTS-1 10  10 "button-down")
                (make-rect 100 200 20 40 #false #false)
                "Returns the rectangle as the given one")
  (check-equal?  (rect-after-mouse-event RECTS-1 10  10 "leave")
                 (make-rect 100 200 20 40 #false #false)
                 "Returns the rectangle as the given one"))


;; world-after-button-event : World Integer Integer String -> World
;; GIVEN: a world ,the mouse coordinates and the operations to be performed
;;which are specified in the string.
;; RETURNS: the world following a button-event  at the given location.
;; if the button-devent is inside the rectangle,returns a rectangle just
;;like the given one, except that it is selected or
;;unselected according to the operation defined.
;; STRATEGY: Use a more general function.

;;Examples:
;;(world-after-button-down World-1 10 10)
;;(make-world 10 10 (list (make-rect 10 10 10 10 #true #false))
;;   (make-point 10 10)
;;   #true)

(define (world-after-button-event w mx my str)
  (make-world
   mx
   my
   (str(world-rects w) mx my)
   (world-locus w)
   (world-paused? w)))

;; world-after-button-down : World Integer Integer -> World
;; GIVEN: a world and the location of the button-down
;; RETURNS: the world following a button-down at the given location.
;; if the button-down is inside the rectangle,returns a rectangle just like the
;; given one, except that it is selected.
;; STRATEGY: Generalization.

;;Examples:
;;(world-after-button-down World-1 10 10)
;;(make-world 10 10 (list (make-rect 10 10 10 10 #true #false))
;;   (make-point 10 10)
;;   #true)

(define (world-after-button-down w mx my)
  (world-after-button-event w mx my rects-after-button-down))
;;Tests

(begin-for-test
  (check-equal? (world-after-button-down WORLD-1 10 10)
                (make-world
                 10 10 (list (make-rect 10 10 10 10 #true #false))
                 (make-point 10 10)
                 #true)
                "Should Return a world after button-down event"))

;; world-after-button-up : World Integer Integer World-> World
;; RETURNS: the world following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected.
;; STRATEGY: Genearalization
;;Examples:
;(world-after-button-up WORLD-1 10 10)
;                (make-world
;                 10 10(list (make-rect 10 10 10 10 #false #false))
;                 #true)
;                "Should return a rectangle world after button up event")

(define (world-after-button-up w mx my)
  (world-after-button-event w mx my rects-after-button-up))
;Tests
(begin-for-test
  (check-equal? (world-after-button-up WORLD-1 10 10)
                (make-world 10 10
                            (list (make-rect 10 10 10 10 #false #false))
                            (make-point 10 10)
                            #true)
                "Should return a rectangle world after button up event"))


;; rect-after-button-event : Rectangle Integer Integer String
;; String Rectangle-> Rectangle
;; Given: A rectangle for applying the function.
;; RETURNS: the rectangle following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected or unseleceted according
;;to the given condition.
;; STRATEGY: Call a more general function.
;;;Examples:
;;(rect-after-button-event RECTS-1 10 10  (rect-selected? RECTS-1) false)
;;(make-rect 100 200 20 40 #false #false)


(define (rect-after-button-event r mx my str str1)
  (if str
      (make-rect (rect-x r)
                 (rect-y r)
                 (rect-vx r)
                 (rect-vy r)
                 str1 (rect-pen-down? r))
      r))

;;Tests:Used in test cases.

;; rect-after-button-down : Rectangle Integer Integer -> World
;; GIVEN: a rectangle in the world and the location of the button-down
;; RETURNS: the rectangle following a button-down at the given location.
;; if the button-down is inside the rectangle,returns a rectangle just like the
;; given one, except that it is selected.
;; STRATEGY: Generalization.
;;Example:
;; (rect-after-button-down RECTS-1 10 10)
;;(make-rect 100 200 20 40 #false #false)

(define (rect-after-button-down r mx my)
  (rect-after-button-event r mx my (in-rect? r mx my) true))


;; rect-after-button-up : Rectangle Integer Integer  Rectangle-> Rectangle
;; Given: A rectangle for applying the function.
;; RETURNS: the rectangle following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected.
;; STRATEGY:Generalization.
;;;Examples:
;;(rect-after-button-up RECTS-1)
;;(make-rect 100 200 20 40 #false #false)
;(rect-after-button-up RECTS-1)
;                (make-rect 100 200 20 40 #false)
;                "Returns a rectangle after button-up event")

(define (rect-after-button-up r mx my)
  (rect-after-button-event r mx my  (rect-selected? r) false))



;; in-rect? : World Integer Integer -> World
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the rectangle.
;; EXAMPLES:
;;(in-rect? RECTS-1 10 10)
;;#false
;; strategy: Use simple functions.

(define (in-rect? r mx my)
  (and
   (<= 
    (- mx HALF-RECT-HEIGHT)
    (rect-x r)
    (+ mx HALF-RECT-HEIGHT))
   (<= 
    (- my HALF-RECT-WIDTH)
    (rect-y r)
    (+ my HALF-RECT-WIDTH))))


;; world-after-drag : World Integer Integer  World-> World
;; GIVEN: a world and the location of the drag event
;; RETURNS: the world following a drag at the given location.
;; if the world is selected, then return a world just like the given
;; one, except that it is now centered on the mouse position.
;; STRATEGY: Combine simpler function.
;; Examples:
;;(world-after-drag WORLD-1 10 10)
;;(make-world 10 10
;;(list (make-rect -280 -280 10 10 #true #false))
;; (make-point 10 10)
;; #true)

(define (world-after-drag w mx my)
  (make-world mx my
              (rects-after-drag (world-rects w) (world-mx w)(world-my w) mx my)
              (world-locus w)
              (world-paused? w) ))

;;Tests:
(begin-for-test
  (check-equal?
   (world-after-drag WORLD-1 10 10)
   (make-world 10 10
               (list (make-rect -280 -280 10 10 #true #false))
               (make-point 10 10)
               #true)
   "Returns the world after drag"))

;; rect-after-drag : Rectangle Integer Integer Integer Integer
;;Rectangle-> Rectangle.
;; GIVEN: a rectangle and the location of the drag event
;; RETURNS: The rectangle following a drag at the given location.
;; if the world is selected, then return a rectangle just like the given
;; one, except that it is now centered on the mouse position.
;; STRATEGY: Combine simpler function.
;; Interp: r is the rectangle.
;;Interp: wmx is the world mx coordinate.
;;Interp: wmy is the world my coordinate.
;;EXAMPLES:
;;(rect-after-drag RECTS-1 10 10 10 10)
;;                (make-rect 100 200 20 40 #false #false)
;;                "Returns a rectangle after drag event"))


(define (rect-after-drag r wmx wmy mx my)
  (if (rect-selected? r)
      (make-rect  (+ (rect-x r) (- mx wmx)) 
                  (+ (rect-y r) (- my wmy))
                  (rect-vx r) (rect-vy r) true (rect-pen-down? r))
      r))

;;Tests
(begin-for-test
  (check-equal? (rect-after-drag RECTS-1 10 10 10 10)
                (make-rect 100 200 20 40 #false #false)
                "Returns a rectangle after drag event"))

;;rects-after-drag :RectangleList Int Int PosInt PosInt
;;RectangleList-> RectangleList.
;;Given: A rectanglelist and the location for the drag event.
;;Returns :A list of rectangle  that is returned after the "drag" mouse event.
;; if the world is selected, then return a rectangle just like the given
;; one, except that it is now centered on the mouse position.
;; STRATEGY: Use template for list.
;; Interp: rl is the rectanglelist.
;;Interp: wmx is the world mx coordinate.
;;Interp: wmy is the world my coordinate.
;;Interp: mx is the  mx coordinate.
;;Interp: my is the  my coordinate.
;;Examples:
;;(rects-after-drag RECT-1 10 10 10 10)
;;(list (make-rect 10 10 10 10 #true #false))

(define (rects-after-drag rl wmx wmy mx my)
  (cond [(empty? rl) empty]
        [else (cons (rect-after-drag (first rl) wmx wmy mx my)
                    (rects-after-drag (rest rl) wmx wmy mx my))]))
;;Tests:
(begin-for-test
  (check-equal?  (rects-after-drag RECT-1 10 10 10 10)
                 (list (make-rect 10 10 10 10 #true #false))
                 "Returns the list of rectangles after key-event"))



;; rects-after-button-event : Rectanglelist POsInt PosInt
;;String String -> Rectanglelist.
;; Given: A rectanglelist for applying the functions.
;; RETURNS: the rectanglelist following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected.
;; STRATEGY: Call a more general function.
;;Examples:(rects-after-button-event RECT-1 10 10
;;rect-after-button-down rects-after-button-down  )
;;(list (make-rect 10 10 10 10 #true #false))


(define (rects-after-button-event rl mx my str str1)
  (cond [(empty? rl) empty]
        [else (cons (str (first rl) mx my)
                    (str1 (rest rl) mx my))]))

;; rects-after-button-up :Rectanglelist Integer Integer->Rectanglelist.
;; Given :A rectangle list in the list and the coordinates for mouse events.
;; Returns: The rectangle following the button down at the given location
;;  returns all the rectangle fittingly in the 
;;Strategy :Generalization.
;;Examples:
;;(rects-after-button-up RECT-1 10 10)
;;(list (make-rect 10 10 10 10 #false #false))

(define (rects-after-button-up rl mx my)
  (rects-after-button-event rl mx my rect-after-button-up
                            rects-after-button-up ))

;; rects-after-button-down :Rectanglelist Integer Integer->Rectanglelist.
;; Given :A rectangle list in the list and the coordinates for mouse events.
;; Returns: The rectangle following the button down at the given location
;;  returns all the rectangle fittingly in the 
;;Strategy :Generalization.
;;Examples:
;;(rects-after-button-down RECTS-1 10 10)
;;(list (make-rect 10 10 10 10 #true #false))
(define (rects-after-button-down rl mx my)
  (rects-after-button-event rl mx my rect-after-button-down
                            rects-after-button-down ))


;;Tests
(begin-for-test
  (check-equal? (any-rect-selected? RECT-2)
                #false
                "Should Return false if no rectangle is selected")
  (check-equal? (any-rect-selected? RECT-1)
                #true
                "Should Return true if no rectangle is selected"))



;;World-after-key-event: world keyevent  world->world
;;Given: A world and a key event.
;;RETURNS: the WorldState that should follow the given worldstate
;;after the given keyevent
;;STRATEGY: Cases on whether the kev is a pause event.
;;Example:
;(world-after-key-event WORLD-1 "p")
;                (make-world
;                 300 300
;                 (list (make-rect 10 10 10 10 #true))
;                 #true)
;                "Returns the world after key event")

(define (world-after-key-event a kev)
  (cond[(is-pause-key-event? kev)
        (world-with-paused-toggled a)]
       [(key=? kev "n") (add-new-rect a)]
       [else (make-world (world-mx a)(world-my a)
                         (rects-after-key-event(world-rects a) kev)
                         (world-locus a)
                         (world-paused? a) )])) 

;;Tests:
(begin-for-test
  (check-equal? (world-after-key-event WORLD-1 "p")
                (make-world
                 300 300
                 (list (make-rect 10 10 10 10 #true #false))
                 (make-point 10 10)
                 #true)
                "Returns the world after key event")
  (check-equal? (world-after-key-event WORLD-1 "n")
                (make-world
                 300 300
                 (list
                  (make-rect 200 150 0 0 #false #false)
                  (make-rect 10 10 10 10 #true #false))
                 (make-point 10 10)
                 #true)
                "Returns the world after key event")
  
  (check-equal? (world-after-key-event WORLD-1 " ")
                (make-world
                 300
                 300
                 (list (make-rect 10 10 10 10 #true #false))
                 (make-point 10 10)
                 #false)
                
                "Returns the world after key event"))

;;rects-after-key-event: rectlist->rectlist
;;Given: A rectangle and a key event.
;;RETURNS: the WorldState that should follow the given worldstate
;;after the given keyevent
;;STRATEGY: Use template for list.
;;Examples:
;;(rects-after-key-event RECT-1 "d")
;;(list (make-rect 10 10 10 10 #true #true))


(define (rects-after-key-event rl kev)
  (cond [(empty? rl) empty]
        
        [else (cons (rect-after-key-event (first rl) kev)
                    (rects-after-key-event (rest rl) kev))]))

;;Tests:
(begin-for-test
  (check-equal? (rects-after-key-event RECT-1 "d")
                (list (make-rect 10 10 10 10 #true #true))
                "Returns a list of rectangles after keyevent"))


;;rect-after-key-event: rectange keyevent. Rectangle->Rectangle.
;;Given: A rectanglelist and a key event.
;;RETURNS: the rect State that should follow the given worldstate
;;here the speed of rectangle increases by 2 ticks per second.
;;after the given keyevent.
;;STRATEGY: Use Cases on keyevents.
;;Examples:See tests below.  




(define (rect-after-key-event r kev)
  (cond
    [ (not (rect-selected? r)) r]
    [(key=? kev "up") (make-rect (rect-x r) (rect-y r)
                                 (rect-vx r)(- (rect-vy r) VELO-CHANGE)
                                 (rect-selected? r) (rect-pen-down? r))]
    [(key=? kev "down")(make-rect (rect-x r) (rect-y r)
                                  (rect-vx r)(+ (rect-vy r) VELO-CHANGE)
                                  (rect-selected? r) (rect-pen-down? r))]
    [(key=? kev "left")(make-rect (rect-x r) (rect-y r)
                                  (- (rect-vx r) VELO-CHANGE) (rect-vy r)
                                  (rect-selected? r) (rect-pen-down? r))]
    [(key=? kev "right")(make-rect (rect-x r) (rect-y r)
                                   (+ (rect-vx r) VELO-CHANGE)(rect-vy r)
                                   (rect-selected? r) (rect-pen-down? r))]
    [(key=? kev "d")(make-rect (rect-x r) (rect-y r)
                               (rect-vx r)(rect-vy r) (rect-selected? r) true)]
    [(key=? kev "u")(make-rect (rect-x r) (rect-y r)
                               (rect-vx r)(rect-vy r)(rect-selected? r) false)]
    [else r]
    ) )

;;Tests:
(begin-for-test
  (check-equal? (rect-after-key-event RECTS-1 "up")
                (make-rect 100 200 20 40 #false #false)
                "Returns a rectangle after key-event")
  (check-equal? (rect-after-key-event RECTS-1 "down")
                (make-rect 100 200 20 40 #false #false)
                "Returns a rectangle after key-event")
  (check-equal? (rect-after-key-event RECTS-2 "up")
                (make-rect 106 200 20 38 #true #true)
                "Returns a rectangle after key-event")
  (check-equal? (rect-after-key-event RECTS-2 "down")
                (make-rect 106 200 20 42 #true #true)
                "Returns a rectangle after key-event")
  
  (check-equal? (rect-after-key-event RECTS-2 "d")
                (make-rect 106 200 20 40 #true #true)
                "Returns a rectangle after key-event")
  (check-equal? (rect-after-key-event RECTS-2 "u")
                (make-rect 106 200 20 40 #true #false)
                "Returns a rectangle after key-event")
  
  (check-equal? (rect-after-key-event RECTS-2 "u")
                (make-rect 106 200 20 40 #true #false)
                "Returns a rectangle after key-event")
  
  (check-equal? (rect-after-key-event RECTS-2 "right")
                (make-rect 106 200 22 40 #true #true)
                "Returns a rectangle after key-event")
  (check-equal? (rect-after-key-event RECTS-2 "left")
                (make-rect 106 200 18 40 #true #true)
                "Returns a rectangle after key-event"))


;;add-new-rect : World  World->World.
;;Given :A world a
;;Returns :A new rectangle on the keyevent "n")
;;Strategy: Use template on world.
;;Examples:
; (add-new-rect WORLD-1)
;(make-world 300 300
; (list
;  (make-rect 200 150 0 0 #false #false)
;  (make-rect 10 10 10 10 #true #false))
; (make-point 10 10)
; #true)


(define (add-new-rect a)
  (make-world (world-mx a) (world-my a) (cons initial-rect
                                              (world-rects a))
              (world-locus a)(world-paused? a) ))
;;Tests:
(begin-for-test
  (check-equal?(add-new-rect WORLD-1)
               (make-world 300 300
                           (list
                            (make-rect 200 150 0 0 #false #false)
                            (make-rect 10 10 10 10 #true #false))
                           (make-point 10 10)
                           #true)
               "Add new rectangle to the world and to the rectangle list"))




;; world-with-paused-toggled :World  World -> World
;; Given: A world.
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template on world  a.
;; Example:
;(make-world 300 300
; (list (make-rect 10 10 10 10 #true #false))
; (make-point 10 10)
; #false)

(define (world-with-paused-toggled a)
  (make-world
   (world-mx a)
   (world-my a)
   (world-rects a)
   (world-locus a)
   (not (world-paused? a))))

;;Tests:
(begin-for-test
  (check-equal?(world-with-paused-toggled WORLD-1)
               (make-world 300 300
                           (list (make-rect 10 10 10 10 #true #false))
                           (make-point 10 10)
                           #false)
               "Returns a world with pause toggled"))

;; world-after-tick : WorldState -> WorldState
;; Given :A world w
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; STRATEGY: Use template on world w.
;;Examples:Used in tests.

(define (world-after-tick w)
  (if  (world-paused? w)
       w
       (make-world 
        (world-mx w)
        (world-my w)
        (move-rects (world-rects w))
        (add-points (world-locus w) (world-rects w))
        false ))) 

;;Tests:
(begin-for-test
  (check-equal?(world-after-tick WORLD-1)
               (make-world 300 300 (list (make-rect 10 10 10 10 #true #false))
                           (make-point 10 10) #true)
               "Returns the world after tick")
  (check-equal? (world-after-tick WORLD-2)
                (make-world 300 300 (list (make-rect 10 10 10 10 #true #false))
                            (make-point 10 10) #false)
                "Returns the world after tick"))

;;move-rects: ListOfRectangles Rectanglelist ->Rectanglelist.
;;Given: The rectangle in the world current state.
;;Returns:The world state and moves the rectangle as specified
;;in the problem set.
;;Strategy:Use template on lists.
;;Example:
;; (move-rects RECT-1)
;;(list (make-rect 10 10 10 10 #true #false)).

(define (move-rects rl)
  (cond [(empty? rl) empty]
        [else (cons (move-rect (first rl)) (move-rects (rest rl)))]))

;;add-points:ListOf Points ListOf Rectangles-> ListOfPoints.
;;Given: A list of point and a rectangle lsit.
;;Returns:The world with the points stated on the scene.
;;Strategy: Use template for lists.
;;Example:
;;(add-points LOCUS-2 RECT-4)
;;                (list (make-point 10 10) (make-point 10 10))

(define (add-points pl rects)
  (cond [(empty? rects) pl]
        [(rect-pen-down? (first rects))
         (cons (make-point (rect-x (first rects)) (rect-y (first rects)))
               (add-points pl (rest rects)))]
        [else (add-points pl (rest rects) )]))
;;Tests:
(begin-for-test
  (check-equal? (add-points LOCUS-2 RECT-4)
                (list (make-point 10 10) (make-point 10 10))
                "Returns the points after passing the lists"))

;;draw-circle: Image ListOfPoints Circle->Circle
;;Given:AListof points and an image.
;;Returns:an image added to the list of points.
;;Strategy:Combine simpler functions.
;;Examples:
;;(check-equal? (draw-circle CC LOCUS-2)
;;              CC))
;;

(define (draw-circle  ps locus)
  (place-image (circle 1 "solid" "black")
               (point-x (first locus))
               (point-y (first locus))
               (print-locus (rest locus)  ps)))
(define CC (circle 1 "solid" "yellow"))
;Tests:
(begin-for-test
  (check-equal? (draw-circle CC LOCUS-2)
                CC
                "Returns a circle added to the list of circles"))

;;move-rect: Rectangle ->Rectangle.
;;Given: The rectangle in the world current state.
;;Returns:The world state and moves the rectangle as specified
;;in the problem set.
;;sTRATEGY:Use template on list.
;;Examples:
;(move-rect RECTS-1)
;                (make-rect 120 240 20 40 #false)
;                "Makes a rectangle after moving")
(define (move-rect r)
  (if (rect-selected? r) 
      r
      (make-rect (update-x (rect-x r) (rect-vx r))
                 (update-y (rect-y r) (rect-vy r))
                 (update-xv (rect-x  r) (rect-vx r))
                 (update-yv (rect-y  r) (rect-vy r))
                 (rect-selected? r)
                 (rect-pen-down? r))))
;;Tests:
(begin-for-test
  (check-equal?
   (move-rect RECTS-1)
   (make-rect 120 240 20 40 #false #false)
   "Returns the rectangle inthe unselected state"))


;;add-point Rectangle->Rectangle
;;Given:A rectangle
;;Returns:A point added if the pen is down.
;;Strategy:Use simpler functions.
;;Example:
;;(add-point initial-rect)
;;                (make-point 0 0)

(define (add-point r)
  (if (rect-pen-down? r)
      (make-point 
       (rect-x r) (rect-y r))
      (make-point 0 0)))


;;Tests:
(begin-for-test
  
  (check-equal? (add-point initial-rect)
                (make-point 0 0)
                "Returns The point for initial-rect as pen is up given (0,0)")
  (check-equal? (add-point RECTS-4)
                (make-point 100 200)))


;;update-x : WorldState->WorldSate.
;;Given: The boundary co-ordinate condition.
;;Returns: The updated condition of the left or right boundaries.
;;Strategy:Combine Simpler Function.
;;Example:
;(check-equal?
;   (update-x -20 388)
;   368
;   "Returns an updated value of the velocity")

(define (update-x x vx)
  (if (left-right-boundary? x vx)
      (if (< vx 0) LEFT-BOUNDARY RIGHT-BOUNDARY)
      (+ vx x)))

;;Tests:
(begin-for-test
  (check-equal?
   (update-x -20 388)
   368
   "Returns an updated value of the velocity")
  (check-equal?
   (update-x 20 360)
   368
   "Returns an updated value of the velocity")
  
  (check-equal?
   (update-x 20 -27)
   32
   "Returns an updated value of the velocity"))


;;update-y : WorldState->WorldSate.
;;Given: The boundary co-ordinate condition.
;;Returns: The updated condition of the -top-bottom boundaries.
;;Strategy:Combine Simpler Function.
;;Examples:
;(update-y 20 270)
;   273
;   "Returns an updated value of the velocity")

(define (update-y y vy)
  (if (top-bot-boundary? y vy)
      (if (< vy 0) TOP-BOUNDARY BOTTOM-BOUNDARY)
      (+ vy y)))

;;Tests:
(begin-for-test
  (check-equal?
   (update-y 20 270)
   273
   "Returns an updated value of the velocity")
  (check-equal?
   (update-y 20 27)
   47
   "Returns an updated value of the velocity")
  (check-equal?
   (update-y -20 -25)
   27
   "Returns an updated value of the velocity" ))

;;update-xv : WorldState->WorldSate.
;;Given: The boundary co-ordinate condition.
;;Returns: The updated condition of thex velocity.
;;Strategy:Combine Simpler Function.
;;Examples:
;(update-xv 20 25)
;   25
;   "Returns an updated value of the velocity")


(define (update-xv x vx)
  (if (left-right-boundary? x vx)
      (- vx)
      vx))

;;Tests:
(begin-for-test
  (check-equal?
   (update-xv 20 25)
   25
   "Returns an updated value of the velocity")
  (check-equal?
   (update-xv 20 -25)
   25
   "Returns an updated value of the velocity" ))

;;update-yv : WorldState->WorldSate.
;;Given: The boundary co-ordinate condition.
;;Returns: The updated condition of the y velocity.
;;Strategy:Combine Simpler Function.
;;Examples:
;(update-yv 20 25)
;   25
;   "Returns an updated value of the velocity")

(define (update-yv y vy)
  (if (top-bot-boundary? y vy)
      (- vy)
      vy))
;;Tests:
(begin-for-test
  (check-equal?
   (update-yv 20 25)
   25
   "Returns an updated value of the velocity")
  (check-equal?
   (update-yv -20 25)
   -25
   "Returns an updated value of the velocity"))


;; top-bot-boundary? WorldState->Boolean
;;Given: The boundary co-ordinate condition.
;;Returns: The updated condition of the top and bottom boundaries.
;;Strategy:Combine Simpler Function.
;;Example:
;(top-bot-boundary? 20 233)
;   false
;   "Should return a boolean value for the boundary cases")

(define (top-bot-boundary? y vy)
  (or (> (+ vy y) BOTTOM-BOUNDARY) (< (+ vy y) TOP-BOUNDARY)))

;;Tests:
(begin-for-test
  (check-equal?
   (top-bot-boundary? 20 233)
   false
   "Should return a boolean value for the boundary cases"))

;;left-right-boundary? WorldState->Boolean
;;Given: The boundary co-ordinate condition.
;;Returns: The updated condition of the left and right boundaries.
;;Strategy:Combine Simpler Function.
;;Examples:
;;   (top-bot-boundary? 20 233)
;; false
;;   "Should return a boolean value for the boundary cases"))

(define (left-right-boundary? x vx)
  (or (> (+ vx x) RIGHT-BOUNDARY) (< (+ vx x) LEFT-BOUNDARY)))

;;Tests:

(begin-for-test
  (check-equal?
   (left-right-boundary? 20 233)
   false
   "Should return a boolean value for the boundary cases"))