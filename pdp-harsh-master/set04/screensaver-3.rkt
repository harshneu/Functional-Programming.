;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver.
;; A list of rectangles in an canvas which perform certain task and
;;create a world scene performing different tasks on different key events.

;; space bar pauses and unpauses the movement of the rectangles.
;; the rectangles can be selected and smothly dragged around the
;; canvas;if selected the rectangle are paused and the color changes to red.
;; a red circle acts as a pointer.
;;On key-event "n" a new rectangle is created.
;; start with (screensaver speed)
;; start with a screensaver and a speed
;; (screensaver speed)

(require rackunit)          ;;testing framework.
(require "extras.rkt")

(require 2htdp/universe)    ;;module to use various inbuilt library functions.
;;such as big-bang,key-events?.

(require 2htdp/image)       ;;module with inbuilt library functions
;;like building shapes and structures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide
 world-rects
 rect-after-key-event
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 rect-selected?
 world-after-mouse-event
 rect-after-mouse-event)

;;Data Definition:

;;A world is a (make-world Int Int rectanglelist Boolean) 
;;Interp: rects is the rectanglelist with its coordinates and velocities.
;;Interp:paused? is the condition that shall return a boolean result of whether
;;       a rectangles are moving or paused.

;;Template:

;;world-fn:World->??.
;;(define (world-fn n)
;;(...
;;   (...is-pause-keyevent...)
;; world-fn : World -> ??
;(define (world-fn n)
;  (... (world-rect1 n)(world-rect2 n)(world-paused? n))

;;;;key-event : KeyEvent -> ??
;;(define (key-event k)
;; helper function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent i.e. spacebar (" ").
;; RETURNS: true iff the KeyEvent represents a pause instruction.


(define-struct world (mx my rects paused?))
(define(is-pause-key-event? ke)
  (key=? ke " ")) 
(define pause-key-event " ")

;;Examples:
;;is-pause-key-event? " ")
;;true
;;is-pause-key-event? "p")
;;false

;;Tests:
(begin-for-test
  (check-equal? (is-pause-key-event? " ")
                true
                "Should return a true value if paused"))


;;A rect is a (make-struct Int Int Int Int Boolean) 
;;Interp:x is the x vertice of the rectangle.
;;Interp:y is the y vertice of the rectangle.
;;Interp:vx is the x coordinate velocity of rectangle
;;Interp:vy is the x coordinate velocity of rectangle
;;selected? is the condition that shall return a boolean result of whether
;;       a rectangles are selected or not.

;;Template:

;;rect-fn:World->??.
;;(define (rect-fn n)
;;(...

;  (... (x)(y)(vx) (vy) (boolean?)
(define-struct rect (x y vx vy selected?))


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
(define RECTS-1 (make-rect  100 200 20 40 false))
(define RECTS-2 (make-rect 106 200 20 40 true))
(define WORLD-BEFORE-BUTTON-UP (make-world 0 0 RECTS-1 false))
(define WORLD-AFTER-BUTTON-UP (make-world 0 0 (make-rect  100 200 20 40 false)
                                          false))
(define WORLD-BEFORE-BUTTON-DOWN (make-world 0 0 RECTS-2 true))
(define WORLD-AFTER-BUTTON-DOWN(make-world 0 0 (make-rect   100 200 20 40 true)
                                            false))
(define RECT-1 (cons (make-rect 10 10 10 10 #true) empty))
(define RECT-2 (cons(make-rect 10 10 10 10 #false) empty))
(define RECT-3 (cons(make-rect   200 100 20 30 true) empty))
(define WORLD-1 (make-world 300 300 RECT-1  true))
(define WORLD-2 (make-world 300 300 RECT-1  false))
(define ICON-RECTANGLE (make-rect 100 100 -12 -15 true))
(define WORLD-ICON (make-world 100 100 (cons ICON-RECTANGLE empty) true))
(define RECT-NOTSELECTED (make-rect 100 100 -14 -15 false))
(define WORLD-NOTSELECTED
  (make-world 100 100 (cons RECT-NOTSELECTED empty) true ))

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
;; Strategy:Combine simpler functions.
;;Examples:
;;(initial-world 8)
;;(make-world 200 150 '() #true)
;;(initial-world 10)
;;(make-world 200 150 '() #true)

(define (initial-world n)
  (make-world HALF-WORLD-WIDTH HALF-WORLD-HEIGHT empty true))

;;Tests:
(begin-for-test
  (check-equal? (initial-world 8)
                (make-world 200 150 '() #true)
                "returns a world"))

;; initial-rect : Any->WorldState.
;; GIVEN:          any input
;; RETURNS: the initial rect specified in the problem set.
;;          here we start as the rectangles in the positions(0,0)
;;          both in a paused state.
;;          true boolean in initial-world function signifies that the
;;          rectangle is paused.
;; Strategy: Combine simpler functions.
(define initial-rect (make-rect 200 150  0  0 false))

;; SCREENSAVER FUNCTION.

;; screensaver : PosReal -> WorldState.
;; GIVEN: the speed of the simulation, in seconds/tick.
;; EFFECT: runs the simulation, starting with the initial state as
;;         specified in the problem set.
;; RETURNS: the final state of the world
;; "speed" used with screensaver function is used to assign the speed
;; to the simulation in seconds/tick.
;; Strategy:Combine simpler functions.

;; BIG BANG EXPRESSION.

;; (big-bang (state-expr clause ...)
;; a big-bang expression returns the last world when the
;; stop condition is satisfied.
;; big-bang is responsible to run the animation.
;; on-draw calls world to scene when we need to draw the scene on the screen.
;; on-key responses to the keyevent whenever the keyevent occurs.


(define (screensaver speed)
  (big-bang (make-world HALF-WORLD-WIDTH HALF-WORLD-HEIGHT
                        empty true)
            (on-tick world-after-tick speed)
            (on-key  world-after-key-event)
            (on-mouse  world-after-mouse-event)
            (on-draw world-to-scene)))

;;example of world for tests:

;; examples for testing
;;(define pause-key-event " ")
;;(define non-pause-key-event "p")
;;Tests:


;;END DATA DEFINITIONS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Strategy:Combine Simpler functions.
;;Example
;;(new-rectangle 10 10 20 20)
;;(make-rect 10 10 20 20 #false)

(define (new-rectangle x y vx vy )
  (make-rect x y vx vy false))

;;Tests:
(begin-for-test
  (check-equal? (new-rectangle 10 10 10 10)
                (make-rect 10 10 10 10 #false)
                "Makes a rectangle andretrns it"))

;;draw-normal-rect: image -> Rectangle
;;Given: The coordinates dimensions and otline of the rectangle.
;;Returns: the rectangle feature when in unselected state.
;;Strategy: Combine simpler functions.
;;Example:
;;(draw-normal-rect RECTS-1)
;;RECTS-1)
;;(draw-normal-rect RECTS-2)
;;Rects-2)

(define (draw-normal-rect r)
  (overlay (rectangle RECT-HEIGHT RECT-WIDTH "outline" "blue")
           (text (string-append (number->string (rect-vx r))
                                "," (number->string (rect-vy r)))
                 TEXT-FONT "blue")))


;; draw-rectangle : NonNegInt NonNegInt Rect Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers mx and my
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Strategy:Combine Simpler function.


(define (draw-rectangle mx my r)
  (if (rect-selected? r)
      (draw-selected-rect r)
      (draw-normal-rect r)))

;;Example
;;(draw-rectangle 10 10 RECT-1)

;;world-to-scene :World->World.
;;Given:A world.
;;Returns:Returns a world with the rectangles and the designated
;;        images.
;;Strategy: Use template on world.
;;Examples:
;;(world-to-scene WORLD-1)
;;(world-to-scene WORLD-2)
;;See tests below.

(define(world-to-scene a)
  (if (any-rect-selected? (world-rects a))
      (place-image
       (circle RADIUS "outline" "red")
       (world-mx a) (world-my a)
       (make-canvas (world-rects a) (world-mx a) (world-my a)))
      (make-canvas (world-rects a) (world-mx a) (world-my a))))

(begin-for-test
  (check-equal? (world-to-scene WORLD-ICON)
                (place-image
                 (circle RADIUS "outline" "red")
                 100 100
                 (make-canvas (world-rects WORLD-ICON) 100 100)))
  (check-equal? (world-to-scene WORLD-NOTSELECTED)
                (make-canvas (world-rects WORLD-NOTSELECTED) 100 100)))

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world, the x- and y-positions of the mouse, and a mouse
;; event. 
;; RETURNS: the world that should follow the given mouse event and supports
;;          smoothh dragging.
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

(define (rect-after-mouse-event r wmx wmy mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down r mx my)]
    [(mouse=? mev "drag") (rect-after-drag r wmx wmy mx my)]
    [(mouse=? mev "button-up")(rect-after-button-up r)]
    [else r]))

;;Tests 
(begin-for-test
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "drag")
                (make-world 10 10 (list(make-rect -280 -280 10 10 #true))#true)
                "Returns a world after drag event")
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "button-down")
                (make-world 10 10 (list (make-rect 10 10 10 10 #true))#true)
                "Returns a world after button-down event")
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "button-up")
                (make-world 10 10 (list (make-rect 10 10 10 10 #false))#true)
                "Returns a world after button-up event")
  (check-equal? (world-after-mouse-event WORLD-1 10 10 "leave")
                (make-world 300 300 (list (make-rect 10 10 10 10 #true))#true)
                "Returns the rectangle as the given one")
  (check-equal? (rect-after-mouse-event RECTS-1 10 10 10 10 "drag")
                (make-rect 100 200 20 40 #false)
                "Returns the rectangle as the given one")
  (check-equal? (rect-after-mouse-event RECTS-1 10 10 10 10 "button-up")
                (make-rect 100 200 20 40 #false)
                "Returns the rectangle as the given one")
  (check-equal? (rect-after-mouse-event RECTS-1 10 10 10 10 "button-down")
                (make-rect 100 200 20 40 #false)
                "Returns the rectangle as the given one")
  (check-equal? (rect-after-mouse-event RECTS-1 10 10 10 10 "leave")
                (make-rect 100 200 20 40 #false)
                "Returns the rectangle as the original one"))


;; world-after-button-down : World Integer Integer -> World
;; GIVEN: a world and the location of the button-down
;; RETURNS: the world following a button-down at the given location.
;; if the button-down is inside the rectangle,returns a rectangle just like the
;; given one, except that it is selected.
;; STRATEGY: Use template for World on w

;;Examples:
;;(world-after-button-down WORLD-1 10 10)
;;(make-world 10 10 (list (make-rect 10 10 10 10 #true))
;;#true)

(define (world-after-button-down w mx my)
  (make-world
   mx
   my
   (rects-after-button-down (world-rects w) mx my)
   (world-paused? w)))

;;Tests
(begin-for-test
  (check-equal? (world-after-button-down WORLD-1 10 10)
                (make-world
                 10 10 (list (make-rect 10 10 10 10 #true))
                 #true)
                "Should Return a world after button-down event"))

;; rect-after-button-down : Rectangle Integer Integer -> World
;; GIVEN: a rectangle in the world and the location of the button-down
;; RETURNS: the rectangle following a button-down at the given location.
;; if the button-down is inside the rectangle,returns a rectangle just like the
;; given one, except that it is selected.
;; STRATEGY: Use template for rect  r
;;Example: (rect-after-button-down RECTS-1 10 10)
;;               (make-rect 100 200 20 40 #false)


(define (rect-after-button-down r mx my)
  (if (in-rect? r mx my)
      (make-rect (rect-x r) (rect-y r) 
                 (rect-vx r) (rect-vy r) true)
      r))

;;Tests:
(begin-for-test
  (check-equal? (rect-after-button-down RECTS-1 10 10)
                (make-rect 100 200 20 40 #false)
                "Returns a rectangle after button-down event"))

;; rects-after-button-down :Rectanglelist Integer Integer.
;; Given:A rectangle list in the list and the coordinates for the mouse events.
;; Returns: The rectangle following the button down at the given location
;;  returns all the rectangle fittingly in the 
;;Strategy :use template for list rl.
;;Examples:
;;(rects-after-button-down RECTS-1 10 10)

;;Template:

;;list-fn:ListOfX -> ??
;;(define (list-fn lst)
;;(cond
;;  [(empty? lst)...]
;;  [else(... (first lst)
;;            (list-fn (rest lst)))]))


(define (rects-after-button-down rl mx my)
  (cond[(empty? rl) rl]
       [else (cons (rect-after-button-down (first rl) mx my)
                   (rects-after-button-down (rest rl) mx my))]))

;; in-rect? : World Integer Integer -> World
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the rectangle.
;; EXAMPLES:
;;(rects-after-button-down RECT-1 10 10)
;;(list (make-rect 10 10 10 10 #true))
;; strategy: Use simple functions.

(define (in-rect? r x y)
  (and
   (<= 
    (- x HALF-RECT-HEIGHT)
    (rect-x r)
    (+ x HALF-RECT-HEIGHT))
   (<= 
    (- y HALF-RECT-WIDTH)
    (rect-y r)
    (+ y HALF-RECT-WIDTH))))



;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world and the location of the drag event
;; RETURNS: the world following a drag at the given location.
;; if the world is selected, then return a world just like the given
;; one, except that it is now centered on the mouse position.
;; STRATEGY: Combine simpler function.
;; Examples:
;;(world-after-drag WORLD-1 10 10)
;;(make-world 10 10
;(list (make-rect -280 -280 10 10 #true))
;;#true)

(define (world-after-drag w mx my)
  (make-world mx my
              (rects-after-drag (world-rects w)(world-mx w) (world-my w) mx my)
              (world-paused? w)))

;;Tests:
(begin-for-test
  (check-equal? (world-after-drag WORLD-1 10 10)
                (make-world
                 10  10 (list (make-rect -280 -280 10 10 #true))
                 #true)
                "Returns a world after drag event"))

;; rect-after-drag : Rectangle Integer Integer -> Rectangle.
;; GIVEN: a rectangle and the location of the drag event
;; RETURNS: The rectangle following a drag at the given location.
;; if the world is selected, then return a rectangle just like the given
;; one, except that it is now centered on the mouse position.
;; STRATEGY: Combine simpler function.
;; Interp: r is the rectangle.
;;Interp: wmx is the world mx coordinate.
;;Interp: wmy is the world my coordinate.
;;Examples:See tests below.

(define (rect-after-drag r wmx wmy mx my)
  (if (rect-selected? r)
      (make-rect  (+ (rect-x r) (- mx wmx)) 
                  (+ (rect-y r) (- my wmy))
                  (rect-vx r) (rect-vy r) true)
      r))

;;Tests
(begin-for-test
  (check-equal? (rect-after-drag RECTS-1 10 10 10 10)
                (make-rect 100 200 20 40 #false)
                "Returns a rectangle after drag event"))

;;rects-after-drag :RectangeList Int Int Int Int Rect->.Rect
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

(define (rects-after-drag rl wmx wmy mx my)
  (cond [(empty? rl) empty]
        [else (cons (rect-after-drag (first rl) wmx wmy mx my)
                    (rects-after-drag (rest rl) wmx wmy mx my))]))

;; world-after-button-up : World Integer World -> World
;; RETURNS: the world following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected.
;; STRATEGY: Combine Simpler functions.
;;Examples:
;(world-after-button-up WORLD-1 10 10)
;                (make-world
;                 10 10(list (make-rect 10 10 10 10 #false))
;                 #true)
;                "Should return a rectangle world after button up event")

(define (world-after-button-up w mx my)
  (make-world
   mx my
   (rects-after-button-up (world-rects w))
   (world-paused? w)))

;;Tests:
(begin-for-test
  (check-equal? (world-after-button-up WORLD-1 10 10)
                (make-world
                 10 10(list (make-rect 10 10 10 10 #false))
                 #true)
                "Should return a rectangle world after button up event"))


;; rect-after-button-up : Rectangle Integer Integer -> Rectangle
;; Given: A rectangle for applying the function.
;; RETURNS: the rectangle following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected.
;; STRATEGY: Combine Simpler functions.
;;;Examples:
;(rect-after-button-up RECTS-1)
;                (make-rect 100 200 20 40 #false)
;                "Returns a rectangle after button-up event")

(define (rect-after-button-up r)
  (if (rect-selected? r)
      (make-rect (rect-x r)
                 (rect-y r)
                 (rect-vx r)
                 (rect-vy r)
                 false)
      r))

;;Tests:
(begin-for-test
  (check-equal? (rect-after-button-up RECTS-1)
                (make-rect 100 200 20 40 #false)
                "Returns a rectangle after button-up event"))

;; rects-after-button-up : Rectangle Integer Integer -> Rectangle.
;; Given: A rectanglelist for applying the functions.
;; RETURNS: the rectanglelist following a button-up at the given location.
;; if the rectangle is selected, return a rectangle just like the given one,
;; except that it is no longer selected.
;; STRATEGY: Use template for lists..

(define (rects-after-button-up rl)
  (cond [(empty? rl) empty]
        [else (cons (rect-after-button-up (first rl))
                    (rects-after-button-up (rest rl)))]))

;;any-rect-selected: World ->World
;;Given: a rectangle position.
;;Returns:Whether a rectangle is selected or not in the given world.
;;Strategy:Use simpler functions.
;;Examples:
;;(any-rect-selected? RECT-2)
;;              #false
;;             "Should Return false if no rectangle is selected")

(define(any-rect-selected? rl)
  (if (empty? rl)
      false
      (or (rect-selected? (first rl)) (any-rect-selected? (rest rl)))))

;;Tests
(begin-for-test
  (check-equal? (any-rect-selected? RECT-2)
                #false
                "Should Return false if no rectangle is selected")
  (check-equal? (any-rect-selected? RECT-1)
                #true
                "Should Return true if no rectangle is selected"))

;;make-canvas : RectangleList->World.
;;Given: canvas and its coordinates.
;;Returns: A canvas with the world scene.
;;Strategy: Functional Composition.
;;Interp: rl is a list of rectangles rl.
;;Interp: mx is the mouse event x coordinate.
;;Interp: my is the mouse event y coordinate.
;;Examples:See Tests below.

(define (make-canvas rl mx my )
  (if (empty? rl)
      EMPTY-CANVAS
      (place-image (draw-rectangle mx my (first rl))
                   (rect-x (first rl))
                   (rect-y (first rl))
                   (make-canvas  (rest rl)mx my))))


;;draw-selected-rect: NonNegInt NonNegInt Int Int -> Rectangle
;;Given: The coordinates dimensions and otline of the rectangle.
;;Returns: the rectanglefeature when in selected state.
;;Strategy: functional composition .

(define (draw-selected-rect r)
  (overlay (rectangle RECT-HEIGHT RECT-WIDTH "outline" "red")
           (text (string-append (number->string (rect-vx r))
                                "," (number->string (rect-vy r)))
                 TEXT-FONT "red")))


;;World-after-key-event: world->world
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
                         (world-paused? a))]))

;;Tests:
(begin-for-test
  (check-equal? (world-after-key-event WORLD-1 "p")
                (make-world
                 300 300
                 (list (make-rect 10 10 10 10 #true))
                 #true)
                "Returns the world after key event")
  (check-equal? (world-after-key-event WORLD-1 "n")
                (make-world
                 300 300
                 (list
                  (make-rect 200 150 0 0 #false)
                  (make-rect 10 10 10 10 #true))
                 #true)
                "Returns the world after key event")
  
  (check-equal? (world-after-key-event WORLD-1 " ")
                (make-world
                 300  300
                 (list (make-rect 10 10 10 10 #true))
                 #false)
                
                "Returns the world after key event"))

;;rects-after-key-event: rectlist->rectlist
;;Given: A rectangle and a key event.
;;RETURNS: the WorldState that should follow the given worldstate
;;after the given keyevent
;;STRATEGY: Use template for list.
;;Examples:See tests below.


(define (rects-after-key-event rl kev)
  (cond [(empty? rl) empty]
        
        [else (cons (rect-after-key-event (first rl) kev)
                    (rects-after-key-event (rest rl) kev))]))

;;rect-after-key-event: rect->rect
;;Given: A rectanglelist and a key event.
;;RETURNS: the rect State that should follow the given worldstate
;;here the speed of rectangle increases by 2 ticks per second.
;;after the given keyevent.
;;STRATEGY: Cases on whether the kev is an assigned event.
;;Examples:See tests below.


(define (rect-after-key-event r kev)
  (cond
    [(not (rect-selected? r)) r]
    [(key=? kev "up") (make-rect (rect-x r) (rect-y r)
                                 (rect-vx r)(- (rect-vy r)VELO-CHANGE) true)]
    [(key=? kev "down")(make-rect (rect-x r) (rect-y r)
                                  (rect-vx r)(+ (rect-vy r)VELO-CHANGE) true)]
    [(key=? kev "left")(make-rect (rect-x r) (rect-y r)
                                  (- (rect-vx r)VELO-CHANGE) (rect-vy r) true)]
    [(key=? kev "right")(make-rect (rect-x r) (rect-y r)
                                   (+ (rect-vx r)VELO-CHANGE)(rect-vy r) true)] 
    [else r]
    ) )

;;Tests:
(begin-for-test
  (check-equal? (rect-after-key-event RECTS-1 "up")
                (make-rect 100 200 20 40 #false)
                "Returns a rectangle after key-event")
  (check-equal? (rect-after-key-event RECTS-2 "up")
                (make-rect 106 200 20 38 #true)
                "Returns a rectangle after key-event")
  
  (check-equal? (rect-after-key-event RECTS-2 "down")
                (make-rect 106 200 20 42 #true)
                "Returns a rectangle after key-event")
  
  (check-equal? (rect-after-key-event RECTS-2 "left")
                (make-rect 106 200 18 40 #true)
                "Returns a rectangle after key-event")
  
  (check-equal? (rect-after-key-event RECTS-2 "right")
                (make-rect 106 200 22 40 #true)
                "Returns a rectangle after key-event"))


;;add-new-rect :world
;;Given :A world a
;;Returns :A new rectangle on the keyevent "n")
;;Strategy: Functinal Composition.
;;Examples:Used in tests.

(define (add-new-rect a)
  (make-world (world-mx a)(world-my a)
              (cons initial-rect (world-rects a))(world-paused? a)))

;; world-with-paused-toggled : World -> World
;; Given: A world .
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Functional Composition.
;; Examples:See in Tests.


(define (world-with-paused-toggled a)
  (make-world
   (world-mx a)
   (world-my a)
   (world-rects a)
   
   (not (world-paused? a))))

;; world-after-tick : WorldState -> WorldState
;; Given :A world w
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; STRATEGY: Functional Composition.
;;Examples:Used in tests.

(define (world-after-tick w)
  (if (world-paused? w) 
      w
      (make-world
       (world-mx w)
       (world-my w)
       (move-rects (world-rects w)) false)))

;;Tests:
(begin-for-test
  (check-equal?(world-after-tick WORLD-1)
               (make-world 300 300 (list (make-rect 10 10 10 10 #true)) #true)
               "Returns the world after tick")
  (check-equal? (world-after-tick WORLD-2)
                (make-world 300 300 (list (make-rect 10 10 10 10 #true)) #false)
                "Returns the world after tick"))

;;move-rects: Rectangle ->Rectangle.
;;Given: The rectangle in the world current state.
;;Returns:The world state and moves the rectangle as specified
;;in the problem set.
;;Strategy:Use template on lists.
;;Example:See tests.

(define (move-rects rl)
  (cond [(empty? rl) empty]
        
        [else (cons (move-rect (first rl)) (move-rects (rest rl)))]))


;;move-rect: Rectangle ->Rectangle.
;;Given: The rectangle in the world current state.
;;Returns:The world state and moves the rectangle as specified
;;in the problem set.
;;Functional Composition.
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
                 (rect-selected? r))))

;;Tests:
(begin-for-test
  (check-equal? (move-rect RECTS-1)
                (make-rect 120 240 20 40 #false)
                "Makes a rectangle after moving"))

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

;; left-right-boundary? WorldState->Boolean
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


