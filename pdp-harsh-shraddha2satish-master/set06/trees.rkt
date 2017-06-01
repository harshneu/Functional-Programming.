;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;This code uses the concept of trees to perform  basic
;;operations such as addition and deletion of children nodes in our tree.
;;Higher order functions are used when and wherever required.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Requires
(require rackunit)       ;;Testing framework.
(require "extras.rkt")
(require 2htdp/universe) ;;Inbuilt Module for the bigbang events.
(require 2htdp/image)    ;;Inbuilt Module  for the mouse/Key events.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides for automated testing systems
(provide
 initial-world 
 run 
 world-after-mouse-event 
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS


;; Templates for struct node

;; Constructor template
;; A node is a
;; (make-node Int Int Boolean ListOfNodes

;; Interpretation:
;; x is the x co ordinate of the centre of the circular node
;; y is the y co ordinate of the centre of the circular node
;; to-selected? returns boolean true when the node has been selected
;; children-node-list is the list of children of that particular node

;; Destructor template:
;; node-fn: node -> ??
;; (define (node-fn n)
;;         (node-x n)
;;         (node-y n)
;;         (node-to-selected? n)
;;         (node-children-node-list n))

(define-struct node (x y  to-selected? children-node-list))

;; List template for children-node-list

;; ListOfChildNode is one of
;; -- empty
;; Represents that the node does not have any children and
;; its children node list is empty

;; -- (cons child LOC)
;; child-node-list-fn: child-node-list -> ??

;; (define (child-node-list loc)
;;  (cond

;;    [(empty? loc) ...]
;;    [else (child-node-fn (first loc)
;;          (child-node-list-fn (rest loc))]))


(define initial-node (make-node 250 0 '() false))

;; Struct template for world
;; Constructor template
;; A world is a
;; (make-world Int Int ListOfNodes
;; Interpretation:
;; mx is the x co ordinate of the cursor when in the world canvas
;; my is the y co ordinate of the cursor when in the world canvas
;; node-list signifies the list of root nodes that are present in the world
;; Destructor template:
;; world-fn: world -> ??
;; (define 
;;         (world-node-list w))

(define-struct world (node-list))

;;List template for node-list
;;node-list-fn : node-list -> ??
;;(define (node-list-fn l)
;;  (cond
;;    [(empty? l) ...]
;;    [else (...
;;             (node-fn (first l))
;;             (node-list-fn (rest l)))]))


;; initial-world: AnyValue -> World
;; Given: Any value
;; Returns: The initial state of the world, the value is ignored
;; Examples:
;; (initial-world "a")
;; EMPTY
;; Design Strategy:Using struct template of world
;; Function Definition:

(define (initial-world any)
  EMPTY) 

;; Tests
(begin-for-test
  (check-equal? (initial-world (make-world "A"))
                EMPTY)
  "Returns the initial world that is empty")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
(define NODES-RADIUS 10)
(define SELECTED-NODE (circle NODES-RADIUS "solid" "green"))
(define UNSELECTED-NODE (circle NODES-RADIUS "outline" "green"))
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define TWO 2)
(define THREE 3)
(define FOUR 4)
(define  FORTY 40)
(define TANGENT-POS 1.5)
(define HALF-NODES-RADIUS (/ NODES-RADIUS TWO))
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH TWO))
(define IN-NODE-RADIUS (/ NODES-RADIUS THREE))
(define CENTRE-X-COORDINATE (/ CANVAS-WIDTH TWO))
(define CENTRE-Y-COORDINATE (/ CANVAS-HEIGHT TWO))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define TREE-DEPTH "blue")
(define NODES-RADII (* THREE NODES-RADIUS))
(define EMPTY '())
(define TEST-NODE (make-node 10 10 false empty))
(define TEST-LIST (list (make-node 10 10 false
                                   (list (make-node 20 20 true empty)))
                        ))
(define TEST-WORLD (cons TEST-NODE TEST-LIST ))
(define TEST-MOVE-SON(list(make-node
                           10 101 #false
                           (list (make-node 20 111 #true '())))))

(define TEST-DRAG (list
                   (make-node 10 10 #false '())
                   (make-node 10 10 #false (list (make-node 10 10 #true '())))))


(define TEST-UP (list
                 (make-node 10 10 #false '())
                 (make-node 10 10 #false (list (make-node 20 20 #false '())))))

(define TEST-DOWN (list
                   (make-node 10 10 #true '())
                   (make-node 10 10 #true (list (make-node 20 20 #true '())))))

(define TEST-ELSE (list
                   (make-node 10 10 #false '())
                   (make-node 10 10 #false (list (make-node 20 20 #true '())))))

(define TEST-T (list
                (make-node 250 11.5 #false '())
                (make-node 10 10 #false '())
                (make-node 10 10 #false (list (make-node 20 20 #true '())))))

(define TEST-N (list
                (make-node 10 10 #false '())
                (make-node
                 10
                 10
                 #false
                 (list (make-node 20 20 #true
                                  (list (make-node 20 50 #false '())))))))

(define TEST-D (list (make-node 10 10 #false '())
                     (make-node 10 10 #false '())))
(define TEST-L
  '())
(define TEST-ELSE-KEY (list
                       (make-node 10 10 #false '())
                       (make-node 10 10 #false
                                  (list (make-node 20 20 #true '())))))
(define TEST-L-DELETED (make-node 200 300 true
                                  (list(make-node 10 20 false '())
                                       (make-node 20 30 false '())
                                       (make-node 30 40 false '())
                                       (make-node 300 400 false '()
                                                  ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run: RealNo -> World
;; Given: a real number which which will start the simulation
;; Effect: simulates the world by adding trees
;; Returns: the final state of the world after the simulation is over
;; Examples:
;; (run 0)
;; (list (make-node 250 11 #false '()))
;; Design Strategy: combining simple functions
;; Function definition:

(define (run var)
  (big-bang (initial-world  empty)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)
            (on-draw world-to-scene)))



;; world-after-mouse-event: World Int Int MouseEvent
;; Given: A world, mouse ordinates and a mouse event
;; Returns: A world which is a result of the mouse event
;; Examples:
;; (world-after-mouse-event TEST-WORLD 10 10 "drag")
;; TEST-DRAG
;; Design Strategy: Dividing into cases on mev
;; Function Design:

(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "button-up") (world-after-button-up w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [else w]))

;;Tests:
(begin-for-test
  (check-equal?
   (world-after-mouse-event TEST-WORLD 10 10 "drag")
   TEST-DRAG
   "Returns The world after Drag event")
  (check-equal?
   (world-after-mouse-event TEST-WORLD 10 10 "button-up")
   TEST-UP
   "Returns The world after button-up event")
  (check-equal?
   (world-after-mouse-event TEST-WORLD 10 10 "button-down")
   TEST-DOWN
   "Returns The world after button-down event")
  (check-equal?
   (world-after-mouse-event TEST-WORLD 10 10 "leave")
   TEST-ELSE
   "Returns The world after any other ubdefined event"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-down: ListOfNodes Int Int
;; Given: a list of nodes and the x and y coordinates for the mouse event
;; Returns: A world after button down
;; Examples:
;; (world-after-button-down (list(make-node 10 10 false empty))10 10)
;; (list (make-node 10 10 #true '()))
;; Design Strategy: Combine simple functions
;; Function definition

(define (world-after-button-down node-list mx my)
  (node-list-after-drag-event  node-list mx my node-after-button-down))
;; Tests
(begin-for-test
  (check-equal?
   (world-after-button-down (list(make-node 10 10 false empty))10 10)
   (list (make-node 10 10 #true '()))
   "Returns the result for the button down scenario"))

;; node-after-button-down Node Int Int
;; Given: a node and the x and y co ordinates ofmouse event
;; Returns: Node in which the x and y coordinates of the centre of the circular
;; have been updated as per the value of the coordinates of
;; the mouse event
;; Examples
;;(node-after-button-down TEST-NODE 10 10)
;;(make-node 10 10 #true '())
;; Design Strategy: Using struct template for node on n
;; Function definition:

(define (node-after-button-down node mx my)
  (make-node (node-x node) (node-y node) 
             (in-node? node mx my)
             (world-after-button-down 
              (node-children-node-list node) mx my)))


;; node-list-after-drag-event: ListOfNodes Int Int MouseEvent -> ListOfNodes
;; Given: a list of nodes , the mouse co ordinates , and a mouse event
;; Returns: List Of nodes after the mouse event has been applied
;; Examples:
;; (world-after-drag (list
;;                     (make-node 10 10 false empty)) 10 10)
;; (list (make-node 10 10 #false '())
;; Design Strategy: Using HOF map on node-list
;; Function definition:

(define (node-list-after-drag-event node-list mx my mouse-events)
  (map
   ;; Node -> Node
   ;; GIVEN: a node.
   ;; RETURNS: a node that follows the mouse function.
   (lambda (node) (mouse-events node mx my))
   node-list))
;; Tests 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-drag :ListOFNodes Int Int->WORLD
;;Given:A list of nodes and the mouse coordinates.
;;Returns:The list of nodes applies on the given events.
;;Examples:
;;(world-after-drag TEST-LIST 10 10)
;;(list
;; (make-node 10 10 #false
;;(list (make-node 10 10 #true '()))))
;;Design Strategy:Combining Simpler Functions.

(define (world-after-drag node-list mx my)
  (node-list-after-drag-event node-list mx my node-after-drag))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;node-after-drag: Node Int Int->Node
;;Given:A node which is to be dragged with its mouse coordinates.
;;Returns:The node after drag.
;;Example:
;;(node-after-drag TEST-NODE 10 10)
;;(make-node 10 10 #false '())
;;Design Strategy:Use cases on nodes.
;;Function Definition:

(define (node-after-drag node mx my)
  (if (node-to-selected? node)
      (drag-selected-node node mx my)
      (pass-unselected-node node mx my)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drag-selected-node:Node Int Int ->World
;; Given:A selected node and where they are needed to be dragged.
;; Returns: A world with the nose moved.
;; Example:
;;(drag-selected-node TEST-NODE 10 10)
;;(make-node 10 10 #true '())
;; Design Strategy:Use template on node.
;; Function Definition:


(define (drag-selected-node node mx my)
  (make-node mx my true
             (drag-children-node-list  
              (node-children-node-list node) (node-x node)
              (node-y node) mx my)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Pass-unselected-node Node Int Int->World
;;Given:A node and its coordinates. .
;;Returns:A world with no node selected.
;;Example:(pass-selected-node 
;;Design Strategy :Use Template on node.
;;Function Definition:
;;(pass-unselected-node TEST-NODE 10 10)
;;(make-node 10 10 #false '())

(define (pass-unselected-node node mx my)
  (make-node (node-x node) (node-y node)
             false (world-after-drag 
                    (node-children-node-list node) mx my)))

;;Tests:Used in Tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag-children-node-list NodeList Int Int Int Int->World
;; Given :A nodelist and its coordinates.
;; Returns:A world for dragging the children.
;; Example:
;; (drag-children-node-list TEST-LIST 10 10 10 101)
;; TEST-MOVE-SON
;;Strategy:USE HOF map on node.
(define (drag-children-node-list  a x y mx my)
  (map
   ;;Node->Node
   ;;Given:A node.
   ;; RETURNS: A scene after the node is dragged.
   (lambda (node) (move-son-node node x y mx my))
   a))

;;Tests:
(begin-for-test
  (check-equal?
   (drag-children-node-list TEST-LIST 10 10 10 101)
   TEST-MOVE-SON
   "REtuns a world after moving the children-nodes"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-Son Node Int Int Int INt->World 
;; Given:A node and the coordinates
;; Returns:A scene with moved child nodes.
;; Design Strategy:Use Template on node.
;; Examples:
;; (move-son TEST-NODE 10 10 10 10)
;; (make-node 10 10 #false '())
;; Function Definition:

(define (move-son-node node x y mx my)
  (make-node (new-drag-posn-x node x mx)
             (new-drag-posn-y node y my) 
             (node-to-selected? node)
             (drag-children-node-list (node-children-node-list node) x y mx my)))
;;Tests:USed In TEsts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-drag-posn-x Node Int Int
;;Given:A node and its xcoordinates.
;;Returns:A node at a new position.
;;Strategy:Combine Simpler Function.
;;Examples:
;;(new-drag-posn-x TEST-NODE 10 45)
;;NEW-POS
;;Functional Definition
(define (new-drag-posn-x n x mouse-x)
  (- mouse-x (- x (node-x n))))
;;TEsts:Used in Test Cases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-drag-posn-y Node Int Int
;;Given:A node and its xcoordinates.
;;Returns:A node at a new position.
;;Strategy:Combine Simpler Function.
;;Examples:
;;(new-drag-posn-y TEST-NODE 10 35)
;;NEW-Y-POS)
;;Functional Definition
(define (new-drag-posn-y n y mouse-y)
  (- mouse-y (- y (node-y n))))

;;Tests:Used in the Tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-button-down : Node-list Integer Integer -> World
;; Given: a node-list which is a part of the world ,the mouse coordinates
;; and the operations to be performed
;; Returns: the world following a button-event  at the given location.
;; if the button-devent is inside the rectangle,returns a rectangle just
;; like the given one, except that it is unselected
;; Examples:
;; (world-after-button-up (list
;;                          TEST-NODE)10 10))
;; (list (make-node 10 10 #false '())
;; Design strategy: Combine Simple Function.

(define (world-after-button-up node-list mx my)
  (node-list-after-drag-event node-list mx my node-after-button-up))

;;Tests
(begin-for-test
  (check-equal?
   (world-after-button-up (list
                           TEST-NODE)10 10) 
   (list (make-node 10 10 #false '()))
   "Returns a world if any after the button event"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-button-down : Node Integer Integer -> World
;; GIVEN: a node on  which the button event was implemented.
;; RETURNS: the world following a button-event  at the given location
;; the list is updated.
;; if the button-devent is inside the rectangle,returns a rectangle just
;; Example:
;;(node-after-button-up TEST-NODE 10  10)
;;(make-node 10 10 #false '())
;; STRATEGY: Use template on node.
;; FunctionDefinition.
(define (node-after-button-up n mx my)
  (make-node (node-x n) (node-y n)
             false 
             (world-after-button-up 
              (node-children-node-list n) mx my)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-node?: Node Int Int -> Boolean
;; Given: Node and mouse co ordinates
;; Returns: A boolean true if te mouse coordinates lie inside the node
;; Examples:
;; (in-node? (make-node 20 30 false '()) 10 15)
;; #false
;; Design Strategy: Using simpler function.
;; Function definition:

(define (in-node? n mx my)
  (and 
   (<=
    (- mx (* pi IN-NODE-RADIUS  ))
    (node-x n) 
    (+ mx (* pi IN-NODE-RADIUS )))
   (<= 
    (- my (* pi IN-NODE-RADIUS ))
    (node-y n)
    (+ my (* pi IN-NODE-RADIUS )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; World-after-key-event: world keyevent  world->world
;; Given: A world and a key event.
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; STRATEGY: Cases on Key Event.
;; Examples:
;; (world-after-key-event TEST-WORLD "t")
;; TEST-T
;;Function Definition

(define (world-after-key-event w kev)
  (cond
    [(key=? kev "t") (add-new-root-node w)]
    [(key=? kev "n") (add-new-node w)]
    [(key=? kev "d") (delete-selected-node w)]
    [(key=? kev "l") (delete-the-left-canvas-nodes w)]
    [else w]))

;;Tests:
(begin-for-test
  (check-equal?
   (world-after-key-event TEST-WORLD "t")
   TEST-T
   "Returns the state of the world after t is placed")
  
  (check-equal?
   (world-after-key-event TEST-WORLD "n")
   TEST-N
   "Returns the state of the world after t is placed")
  
  (check-equal?
   (world-after-key-event TEST-WORLD "d")
   TEST-D
   "Returns the state of the world after d is placed")
  
  (check-equal?
   (world-after-key-event TEST-WORLD "l")
   TEST-L
   "Returns the state of the world after l is placed")
  
  (check-equal?
   (world-after-key-event TEST-WORLD "L")
   TEST-ELSE-KEY
   "Returns the state of the world after any key but the specified is placed"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add-new-root-node:World->ListOfNodes.
;;Given: A world which is empty.
;;Returns:A world with a node added to the canvas list.
;;Strategy:Apply template on node.
;;Examples:
;;add-new-root-node TEST-WORLD)
;;(list
;; (make-node 250 11 #false '())
;;(make-node 10 10 #false '())
;; (make-node 10 10
;;  #false(list (make-node 20 20 #true '()))))
;;Function definitions:
(define (add-new-root-node w)
  (cons (make-node HALF-CANVAS-WIDTH ( + NODES-RADIUS TANGENT-POS)false
                   empty) w))
;;Tests:Used In test.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-new-node: ListOfNodes -> ListOfNodes
;; Given: a list of nodes
;; Returns: another list of nodes that i a result of the given list and "n"
;; key operation
;; Examples:
;; (add-new-node (list
;;                 (make-node 10 20 false '())
;;                 (make-node 20 30 false '())))
;; (list (make-node 10 20 #false '()) (make-node 20 30 #false '()))
;; Design Strategy: Using HOF Map on node-list
;; Function Definition:
(define (add-new-node node-list)
  (map
   ;;Node->Node
   ;;Returns:A node after following the key event "n".
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: the node following the "n" key event 
   (lambda (a) (node-after-n-key-event a))
   node-list))
;; Tests
(begin-for-test
  (check-equal? (add-new-node (list
                               (make-node 10 20 false '())
                               (make-node 20 30 false '())))
                (list (make-node 10 20 #false '())
                      (make-node 20 30 #false '()))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-n-key-event: Node -> Node
;; Given: A node whose to-selected? value is extracted
;; and that decides whether sons must be added or not
;; Returns: If to-selected? value of node is true, then son is added
;; else the same node is returned
;; Examples:
;; (node-after-n-key-event (make-node 10 20 false '()))
;; (make-node 10 20 #false '())
;; (node-after-n-key-event (make-node 10 20 true '()))
;; (make-node 10 20 #true (list (make-node 10 50 #false '())))
;; Design Strategy: Using struct template of node on n
;; Function definition:

(define (node-after-n-key-event n)
  (if (node-to-selected? n)
      (make-node (node-x n) (node-y n) (node-to-selected? n)
                 (add-new-son n))
      (make-node (node-x n) (node-y n) (node-to-selected? n)
                 (add-new-node (node-children-node-list n)))))
;; Tests
(begin-for-test
  (check-equal? (node-after-n-key-event (make-node 10 20 false '()))
                (make-node 10 20 #false '()))
  (check-equal? (node-after-n-key-event (make-node 10 20 true '()))
                (make-node 10 20 #true (list (make-node 10 50 #false '())))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-new-son: Node -> ListOfNodes
;; Given: A node
;; Returns: the list of children of that node
;; Examples:
;; (add-new-son (make-node 10 20 false '()))
;; (list (make-node 10 50 #false '()))
;; (add-new-son (make-node 10 20 true '()))
;; (list (make-node 10 50 #false '()))
;; Design Strategy: Using struct template of node on n
;; Function Definition: 

(define (add-new-son n)
  (cons (make-node
         (x-coordinate-new-node n) 
         (y-coordinate-new-node n) false empty)
        (add-new-node (node-children-node-list n))))

;; Tests
(begin-for-test
  (check-equal? (add-new-son (make-node 10 20 false '()))
                (list (make-node 10 50 #false '())))
  (check-equal? (add-new-son (make-node 10 20 true '()))
                (list (make-node 10 50 #false '()))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x-coordinate-new-node: Node -> Int
;; Given: a node
;; Returns: The x coordinate of the centre of the circular node which is
;; a child of the given node
;; Examples:
;; (x-coordinate-new-node (make-node 10 20 false '()))
;; 10
;; Design Strategy: Combining Simpler Function.
;; Function Definition:
(define (x-coordinate-new-node n)
  (new-son-posn-x (node-children-node-list n)(node-x n)))
;; Tests
(begin-for-test
  (check-equal? (x-coordinate-new-node (make-node 10 20 false '())) 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; y-coordinate-new-node: Node -> Int
;; Given: A node
;; Returns: the y co ordinate of the centre of the circular node which is a
;; child of the given node
;; Examples:
;; (y-coordinate-new-node (make-node 10 20 false '()))
;; 50
;; Design Strategy: Combining Simpler Function.
;; Function Definition:
(define (y-coordinate-new-node n)
  (+ (node-y n) NODES-RADII))
;; Tests
(begin-for-test
  (check-equal? (y-coordinate-new-node (make-node 10 20 false '())) 50))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-selected-node: ListOfNodes -> ListOfNodes
;; Given: A list of nodes:
;; Returns: The list of children node of a given node is deleted, if the
;; parent node is selected
;; Examples:
;; (delete-selected-node (list (make-node 20 20 #true
;; (list (make-node 20 50 #false '())))))
;; '()
;; Design Strategy: Using HOF map-filter on node-list and
;; using struct template of node on n
;; Function definition:
(define (delete-selected-node node-list)
  (map
   node-after-deletion
   (filter
    ;; Node -> Node
    ;; GIVEN: a node
    ;; RETURNS: the same node, if it is not selected
    ;; node from the node-list.
    (lambda (n) (not (node-to-selected? n)))
    node-list)))


;; Tests
(begin-for-test
  (check-equal? (delete-selected-node
                 (list (make-node 20 20 #true
                                  (list (make-node 20 50 #false '())))))
                '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-son-posn-x: ListOfNodes Int -> Int
;; Given: A list of nodes and an integer which will be used to
;; find the max x coordinate value
;; Returns: The new x coordinate of the centre of the circular node
;; which is a child
;; of the given node
;; Examples:
;; (new-son-posn-x (list (make-node 20 50 #false '())
;; (make-node 20 20 #true  '())) 10)
;; 50
;; Design Strategy: Using HOF foldr on node-list and
;; struct template of node on w
;; Function Definition:
(define (new-son-posn-x node-list x) 
  (foldr
   ;;Node PosINT ->PosInt.
   ;;Returns :The x coordinate after checking the max coordinate of
   ;;any of the previous sons so the world the scene function can plot new son in
   ;;accordance to the coordinate.
   (lambda (w x-max) (max (+(node-x w) NODES-RADII)x-max))x node-list)) 

;;Tests:
(begin-for-test
  (check-equal?
   (new-son-posn-x TEST-LIST 10)
   FORTY
   "Returns posn after adding 3 times radii in the x node"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-deletion: Node -> Node
;; Given: A node
;; Returns: Updates node after some of its children have been
;; deleted using the d key
;; Examples:
;; (node-after-deletion (make-node 10 20 true
;; (list(make-node 10 20 true '())(make-node 20 30 false '())
;;  (make-node 20 30 true (list (make-node 10 20 false '())
;;   (make-node 20 30 false '())
;;  (make-node 30 40 false '()))))))                                                                                                                     
;; (make-node 10 20 #true (list (make-node 20 30 #false '())))
;; Design Strategy: Using struct template of node on n
;; Function definition:
(define (node-after-deletion n)
  (make-node (node-x n)(node-y n)(node-to-selected? n)
             (delete-selected-node (node-children-node-list n))))

;; Tests
(begin-for-test
  (check-equal?
   (node-after-deletion
    (make-node 10 20 true
               (list(make-node 10 20 true '())
                    (make-node 20 30 false '())
                    (make-node 20 30 true
                               (list(make-node 10 20 false '())
                                    (make-node 20 30 false '())
                                    (make-node 30 40 false '()))))))
   (make-node 10 20 #true (list (make-node 20 30 #false '())))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-the-left-canvas-nodes: ListOfNodes -> ListOfNodes
;; Given: A list of nodes
;; Returns: a new list of nodes some of whose elements may be
;; deleted if their centres lie in
;; the left half of the ccanvas
;; Examples:
;; (delete-the-left-canvas-nodes
;; (list(make-node 10 20 false '())
;;     (make-node 20 30 false '()) (make-node 30 40 false '())))
;; '()
;; Design Strategy: Using HOF map-Filter on node-list
;; Function definition:

(define (delete-the-left-canvas-nodes node-list)
  (map
   node-after-left-canvas-empty
   (filter
    ;;Node->Node
    ;;Given: A node
    ;;Returns :The same node ,if it's center is in the lower half
    ;; of canvas.
    (lambda (n) (>= (node-x n)HALF-CANVAS-WIDTH))
    node-list)))
;; Tests
(begin-for-test
  (check-equal? (delete-the-left-canvas-nodes(list
                                              (make-node 10 20 false '())
                                              (make-node 20 30 false '())
                                              (make-node 30 40 false '())))'()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-left-canvas-empty: Node -> Node
;; Given: A node
;; Returns: a single node whose children to the left of the canvas have been
;; deleted
;; Examples:
;; (node-after-left-canvas-empty (make-node 200 300 true (list
;;  (make-node 10 20 false '())
;; (make-node 20 30 false '())
;; (make-node 30 40 false '())
;;  (make-node 300 400 false '()))))
;; (make-node 200 300 #true (list (make-node 300 400 #false '())))
;; Design Strategy: Using struct template of node on n
;; Function definition:

(define (node-after-left-canvas-empty n)
  (make-node (node-x n) (node-y n) (node-to-selected? n)
             (delete-the-left-canvas-nodes (node-children-node-list n))))
;;Tests:
(begin-for-test
  (check-equal?
   (node-after-left-canvas-empty TEST-NODE)
   (make-node 10 10 #false '())
   "Returns a node after the event")
  (check-equal? (node-after-left-canvas-empty TEST-L-DELETED)
                (make-node 200 300 #true (list (make-node 300 400 #false '())))
                "Returns a canvas after deleting left canvas nodes"
                ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene: World -> Scene
;; Given: a world
;; Returns: A scene on to which world has been rendered
;; Examples:
;; ((world-to-scene TEST-WORLD)
;; WORLD-TO-SCENE-ICON )
;; Design Strategy: Using HOF foldr on world-node-list
;; Function Definition:

(define (world-to-scene w)
  (foldr
   ;;Node->Scene
   ;;Returns: A new scenewith newnodes added to the existing scene.
   (lambda (node scene)
     (tree-to-scene node scene))
   EMPTY-CANVAS
   w))
;; Tests

(begin-for-test
  (check-equal?(world-to-scene TEST-WORLD)
               world-to-scene-icon ))       

;;tree-to-scene: Node ChildNode -> Scene
;; Given: A node and its child node
;; Returns: A scene onto which the node and its child have been rendered.
;; Examples:
;;(tree-to-scene TEST-NODE
;; WORLD-TO-SCENE-ICON)
;; Design Strategy: Combining simple functions:
;; Function definition:

(define(tree-to-scene node scene)
  (if (node-to-selected? node)
      (place-the-node node scene SELECTED-NODE)
      (place-the-node node scene UNSELECTED-NODE)))

;; son-to-scene: Node Int Int ChildNode -> Scene
;; Given: A node and its child node 
;; Returns: A scene in which node and child will be placed on the scene
;; with a line in between them
;; Examples:
;;(son-to-scene TEST-NODE
;;10 10 WORLD-TO-SCENE-ICON)
;; Design Strategy: Using template of struct node on c
;; Function Definition:

(define (son-to-scene a x y scene)
  (scene+line (tree-to-scene a scene) x y
              (node-x a) (node-y a)TREE-DEPTH))


;; Tests: Used in Test Cases.

;; place-the-node: Node ChilNode String -> Image
;; Given: A node, its child, and a string signifying whether the node has been
;; selected.
;; Returns: An image onto which the appropriateselected or unselected node is
;; placed, along
;; withits child.
;; Examples: (place-the-node TEST-NODE
;; WORLD-TO-SCENE-ICON SELECTED-NODE)
;; Design Strategy: Using more general functions, and using struct template
;; for node on n
;; Function definition:

(define (place-the-node node scene str)
  (place-image str (node-x node)(node-y node)
               (place-children-node-list
                (node-children-node-list node)(node-x node)(node-y node)scene
                )))

;; Tests:USED in Test Case.


;; place-children-node-list: ListOfNodes Int Int ChildNode -> Node
;; Given: A list of nodes, a child node, x and y co rodinates of the
;;circular child node
;; Returns: A scene in which the son has been rendered onto it at position (x,y)
;; Examples: (place-children-node-list (list
;;                             (make-node 10 10 false empty)
;;                             (make-node 20 20 true empty))
;;                              10 10 SELECTED-NODE)
;;                                  ICON-SELECTED
;; Design Strategy: Using HOF foldr on node-list
;; Function Definition:

(define (place-children-node-list node-list x y scene)
  (foldr
   ;;NODE Scene->SCENE
   ;;Returns:A scene with added son to the node list.
   (lambda (node scene) (son-to-scene node x y scene))
   scene
   node-list))

(define ICON-SELECTED (place-children-node-list (list
                                                 (make-node 10 10 false empty)
                                                 (make-node 20 20 true empty))
                                                10 10 SELECTED-NODE));; Tests
(begin-for-test
  (check-equal? (place-children-node-list (list
                                           (make-node 10 10 false empty)
                                           (make-node 20 20 true empty))
                                          10 10 SELECTED-NODE)
                ICON-SELECTED
                "Retuns The node list after placing the node"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-trees: World -> ListOfNodes
;; Given: The world state
;; Returns: The list of nodes contained in the world structure
;; Examples:
;; (world-to-trees (make-world 10 10 empty)
;; '()
;; Design Strategy: Using struct template of world on w
;; Function Definition:
(define (world-to-trees w)
  (world-node-list w))
;;Tests:
(begin-for-test
  (check-equal?
   (world-to-trees (make-world empty))
   '()
   "Returns the world trees in the present scenario"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-to-center: Node -> Position
;; Given: A node
;; Returns: the position of its centre in the format (make-posn x y) where
;; x and y are the coordinates
;; of the centre of the node in the graphic coordinate canvas
;; Examples:
;; (node-to-center TEST-NODE)
;; (make-posn 10 10)
;; Design Strategy: Using struct template of node on n
;; Function Definition:
(define (node-to-center node)
  (make-posn (node-x node) (node-y node)))
;;Tests
(begin-for-test
  (check-equal?
   (node-to-center TEST-NODE)
   (make-posn 10 10)
   "Returns the positions for our events."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-sons: Node -> ListOfChildNodes
;; Given: A node
;; Returns: The list of children nodes each of which is child to the given node.
;; Examples:
;; (tree-to-sons TEST-NODE)
;; '()
;; Design Strategy: Using struct template of node on n
;; Function Design:
(define (tree-to-sons n)
  (node-children-node-list n))
;;Tests:
(begin-for-test
  (check-equal?
   (tree-to-sons TEST-NODE)
   '()
   "Returns the list of elements in the current list of child nodes"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-root: Node -> Node
;; Given: A node
;; Returns: The root to the given node, where the root itself is a node
;; Examples:
;; (tree-to-root TEST-NODE)
;; (make-node 10 10 #false '())
;; Design Strategy: Using the struct template of node on n
;; Function Definition:
(define (tree-to-root n)
  n) 

;;Tests:
(begin-for-test
  (check-equal?
   (tree-to-root TEST-NODE)
   (make-node 10 10 #false '())
   "A root node of our given tree is returned"))

;;world-to-scene-icon :Image->Image
;;Given: Takes an image world to scene.
;;Returns: An image that is used in testcases to check tests.
;;Strategy:Combine Simpler Function.
;;Example:(world-to-scene-icon (world-to-scene TEST-WORLD))
(define world-to-scene-icon (world-to-scene TEST-WORLD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;