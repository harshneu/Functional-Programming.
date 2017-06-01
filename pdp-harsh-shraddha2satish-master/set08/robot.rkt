;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;On the chessboard, we have a robot and some blocks. The robot occupies
;;a single square on the chessboard, as does each of the blocks. The robot can
;;move any number of squares in any diagonal direction, but it can never move to
;;or through a square occupied by a block. In this way, its behavior is like that
;;of a bishop in chess.The code uses a modified approach to DFS algorithm and
;;traverses throughout till the robot does not move from initial position to
;;target position.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)
;;inbuilt module required for testing.
(require "extras.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Provides for automated testing.
(provide
 path
 eval-plan)


;;Data Definitions:


;; A Position is a (list Integer Integer)
;; (list x y) represents the position (x,y).

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction. 

;; A Direction is one of
;; -- "ne"
;;represents the North-East direction

;; -- "se"
;;represents the South-East direction

;; -- "sw"
;;represents the South-West direction

;; -- "nw"
;;represents the North-West direction



;;Templates:

;;Template for position function:
;;position-fn->??
;;(define (pos-fn n)
;;(..
;;   (first n)
;;   (rest n)))

;;Interp:
;;(first n):Represents the first element of any list.
;;(rest n):Represnts the rest of the elements of any list.


;; Template for list of Positions:

;;A list of position can be one of
;;--empty
;;--(cons Position LOP)

;;Interp:
;;cons adds the element to the list.
;;LOP is the list of positions.

;;lop-fn ->lop
;;(define(lop-fn lop)
;;   (cond
;;     [(empty?lop)..]
;;     [else(..
;;           (pos-fn(first lop))
;;          (lop-fn (rest lop))])

;;Template for move-fn:
;;move-n :Move->??
;;(define (move-fn m)
;;  (...
;;   (first mov)
;;   (rest mov)))

;;Template for direction:
;;direction-fn :Direction-> ??
;;(define (dir-fn d)
;;  (cond
;;    [(string=? "ne")...fn]
;;    [(string=? "nw")...fn]
;;    [(string=? "se")...fn]
;;    [(string=? "sw")...fn]))

;;Template for Equality-Tests:
;;(begin-for-test 
;;  (check-equal?
;;   (fn ...
;;   Result
;;   "Test Message")).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define BASE -100)      ;;For boundary condition.
(define FIRST-POSN 1)
(define NO-ELEMENT 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Direction-Constants
(define NORTH-WEST "nw")     
(define SOUTH-WEST "sw")
(define NORTH-EAST "ne")
(define SOUTH-EAST "se")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constants for Testing.
(define NON-NEG-INT 0)
(define TEST-DIRECTION-RESULT1 (list "ne" 1))
(define TEST-DIRECTION-RESULT2 (list "nw" 1))
(define TEST-DIRECTION-RESULT3 (list "se" 1))
(define TEST-DIRECTION-RESULT4 (list "sw" 1))
(define list1 (list 5 5))
(define list2 (list 8 4))
(define list3 (list 4 4))
(define list5 (list 4 8))
(define TEST1-RESULT (list (list "se" 3)))
(define TEST-LIST1 (list (list 10 10) (list 7 9)))
(define TEST-LIST2 (list (list "ne" 1) (list "sw" 1)))
(define list4 (list 1 1))
(define list6 (list 10 10))
(define ADDENDS 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Examples for testing:

(define wall1
  '((0 3)(2 3)(4 3)
         (0 5)     (4 5)
         (0 7)(2 7)(4 7)))

(define two-walls
  '((0 3)(4 3)
         (0 5)(4 5)
         (0 7)(4 7)
         (0 9)(4 9)
         (0 11)(4 11)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;belongs-to? position listofposition.->Boolean.
;;Given:A position and a subsequent list of position.
;;Returns:A boolean result whether a position belongs to given list or not.
;;Example:
;;(belongs-to? (list 0 3) wall1)
;;#true
;; strategy: HO Function Combination
(define (belongs-to? pos lpos)
  (ormap
   (lambda (b) (equal? pos b))
   lpos))
;;Tests:
;;Used in tests.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;path : Position Position ListOfPosition -> MaybePlan
;;GIVEN:
;;1. the starting position of the robot,
;;2. the target position that robot is supposed to reach
;;3. A list of the blocks on the board

;;RETURNS: a plan that, when executed, will take the robot from
;;the starting position to the target position without passing over any
;;of the blocks, or false if no such sequence of moves exists.
;;Design Strategy:Combine simpler functions.
;;Examples:
;;(path (list 1 1)(list 4 4)empty)
;;(list (list "se" 3))
;;Function Definition:

(define (path initial-pos target loblocks)
  (path-locus initial-pos target loblocks (list initial-pos)(list initial-pos)))

;;Tests:
;;See Below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; form-move:ListofPosition->MaybeListofMoves
;; GIVEN: the list of positions in between initial-pos and target.
;; RETURNS:A plan for the traversal of our robot from initial pos to the target.
;; EXAMPLES:
;;(form-move (list (list 1 1) (list 2 2)))
;;(list (list "nw" 1))
;;Design Strategy:Use simpler functions.

(define (form-move path)
  (append-unidirectional-moves (form-moves-append (reverse path))))

;;Tests:Used in tests.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; form-moves-append: ListOfPositions -> ListOfMoves
;; GIVEN: the path follwed by the robot to traverse the whole path.
;; RETURNS:the list of moves our robot takes to traverse whole path.
;; EXAMPLES:
;;(form-moves-append (list (list 1 1) (list 2 2)))
;;(list (list "se" 1))

;;Design Strategy:USe template on lists.
;;Function Definition:
(define (form-moves-append path)
  (cond
    [(empty? (rest path)) empty]
    [else (cons
           (find-direction (first path)(second path))
           (form-moves-append (rest path)))]))
;;Tests:
;;Used in tests.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; append-unidirectional-moves:ListOfMoves ->ListofMoves.
;; GIVEN:the list of moves taken by robot to reach from initial-pos to target.
;; RETURNS:Appends the two moves which are in same directions.
;; EXAMPLES:
;;(append-unidirectional-moves (list (list "ne" 1) (list "sw" 1)))
;; (list (list "ne" 1) (list "sw" 1))
;;Design strategy: Use template on lists.

(define (append-unidirectional-moves lomoves)
  (cond
    [(empty? lomoves)empty]
    [else (concat-successive-moves (first lomoves) (rest lomoves))]))


;;Tests:
;;See below for Tests:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; concat-successive-moves: Move ListOfMoves -> ListOfMoves.
;; GIVEN: the moves takenby th erobot in the process.
;; RETURNS:adds the new moves to the existing listofmoves.
;; EXAMPLES:
;;(concat-successive-moves(list "ne" 1)(list (list "ne" 1) (list "sw" 1)))
;;(list (list "ne" 2) (list "sw" 1))
;;Design strategy: Use Template on lists.

;;Function Definition:

(define (concat-successive-moves move lom)
  (cond
    [(empty? lom) (cons (list (first move) (second move)) empty)]
    [else (concat-next-move move lom)]))

;;Tests:
;;Used in test.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;concat-next-move :Move ListOfMoves->ListOfMoves
;; GIVEN: the moves takenby th erobot in the process.
;; RETURNS:adds the new moves to the existing listofmoves.
;; EXAMPLES:
;;(concat-successive-moves(list "ne" 1)(list (list "ne" 1) (list "sw" 1)))
;;(list (list "ne" 2) (list "sw" 1))
;;Design strategy: Use Template on lists.

;;Function definition;
(define (concat-next-move move lom)
  (if (string=? (first move) (first (first lom)))
      (concat-successive-moves (list (first move) 
                                     (add1 (second move))) 
                               (rest lom))
      (concat-checker move lom)))


;; concat-checker: Move ListOfMoves -> ListOfMoves
;; GIVEN: A move and the path list of moves from initial-pos and target.
;; position.
;; RETURNS:The list of moves taken to traverse from the initial-pos to target. 
;; EXAMPLES:
;;(concat-checker(list "ne" 1)(list (list "ne" 1) (list "sw" 1)))
;;(list
;; (list "ne" 1)
;; (list "ne" 1)
;; (list "sw" 1))
;;Design strategy: Use Template on lists.

;;Function Definition:
(define (concat-checker move lom)
  (cond
    [(empty? lom) move]
    [else (cons move (concat-successive-moves (first lom) (rest lom)))]))


;;tests for concat-checker

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; find-direction: Position Position -> Move,,

;; GIVEN: two positions..
;; RETURNS: determines the direction for the path of robot.
;; EXAMPLES:
;;(find-direction (list 1 1)(list 4 4))
;;(list "se" 1)
;; STRATEGY: Use template on lists.
;;Termination Argument:Terminates after returning the directions will be
;; called at every recursion .

;;Function definition:

(define (find-direction pos1 pos2)
  (cond
    [(<= (-(first pos2)(first pos1)) NON-NEG-INT)
     (if (< (-(second pos2)(second pos1)) NON-NEG-INT)
         (list NORTH-WEST FIRST-POSN)
         (list SOUTH-WEST FIRST-POSN))]
    
    [(>= (-(first pos2)(first pos1)) NON-NEG-INT)
     (if (< (-(second pos2)(second pos1)) NON-NEG-INT)
         (list NORTH-EAST FIRST-POSN)
         (list SOUTH-EAST FIRST-POSN)
         )]))

;;tests for find direction
;;See Tests Below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; in-boundary: ListOfblocks Position Position -> Position
;; GIVEN: A list of position and two position for initial and target.
;; RETURNS: The maximum element of the boundary is calculated
;; through the given fields.
;; EXAMPLES:
;; (in-boundary (list (list 10 10) (list 7 9)) 
;;  (list 4 4) (list 1 1))
;;  (list 10 10)
;;Strategy:Use template on list.

;;Function definition:
(define (in-boundary loblocks target initial-pos)
  (list(max (first initial-pos) (new-boundary loblocks first (first target)))
       (max (second initial-pos) (new-boundary loblocks second (second target)))))

;;Tests:
;;See tests below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; new-boundary: ListOfBlocks posnfunction Position-> Position
;;Given:A list of blocka function for x and y coordinates target position. 
;;Returns:The boundary condition for the given conditions.
;;Example:
;;(new-boundary (list (list  10 10) new-posn (list 1 1))
;;Strategy:Use HOF folde on loblocks.

;;Function Definition:
(define (new-boundary loblocks new-posn target)
  (foldr
   max
   target
   (map
    ;;Position -> Integer
    ;;GIVEN: a position.
    ;;RETURNS: a positive integer representing x or y coordinate 
    ;;of the position.
    (lambda (newpos) (new-posn newpos))
    loblocks))) 

;; node-successors: Position -> ListOfPosition
;; GIVEN: a position.
;; RETURNS:all the  diagonal blocks of the given position in the
;; chessboard.
;; EXAMPLES:
;; (node-successors (list 6 5))
;; (list
;; (list 7 6)
;; (list 5 6)
;; (list 5 4)
;; (list 7 4))
;;Strategy:Combine simpler function and use template on lists.

(define (node-successors start-pos)
  (list (list (add1 (first start-pos)) (add1(second start-pos)))
        ;;Northeast Successor.
        (list (sub1 (first start-pos)) (add1 (second start-pos)))
        ;;Northwest Successor.
        (list (sub1 (first start-pos)) (sub1 (second start-pos)))
        ;;SouthWest Successor.
        (list (add1 (first start-pos)) (sub1 (second start-pos)))))
;;Southeast Successor.
;;Tests:
;;see examples.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; potential-successor? : Position ListOfPostion ListOfPostion Position
;;                                Position -> Boolean
;; GIVEN: a  position, list of blocks,path-traversed,target
;;        and initial-pos of the robot.
;; WHERE:Path traversed is the list of points that the robot has traversed yet.
;; RETURNS: true iff,
;; 1. the given potential-successor is not a block,
;; 2. the given potential-successor is not already traversed. 
;; 3. the given potential-successor is within the bounding walls.
;; HALTING-MEASURE:When all potential successors are determined or returns
;;                false if no successors could be identified.
;;Strategy: Combine simpler function.
;;Example:
;;(potential-successor? (list 1 2) 
;;                      (list (list 2 2) (list 3 3))
;;                     (list (list 4 4)) (list 5 5)
;;                      (list 6 7))
(define (potential-successor? p loblocks path-traversed target initial-pos)
  (and
   (not (belongs-to? p loblocks))
   (not (belongs-to? p path-traversed))
   (in-walls? p (in-boundary loblocks target initial-pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; in-walls?: Position listofPosition -> Boolean
;; GIVEN: the boundary condition and the position of the successor.
;; RETURNS: checks if the successor is in the boundary or not
;; if yes returns true.
;; EXAMPLES:
;;(in-walls? (list 1 1)(list 1 3))
;;#true
;;Strategy:Use template on list.

;;Function Definition:
(define (in-walls? r-posn walls)
  (and (<= (first r-posn) (first walls))
       (>= (first r-posn) BASE)
       (<= (second r-posn) (second walls))
       (>= (second r-posn) BASE)))
;;Tests:
;;Used in test.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; successors: Position ListOfPosition ListOfPosition Position 
;;                                             Position-> ListOfPosition
;; GIVEN:the present location of the robot the path already traversed by the
;; robot the initail position and the target to be visited by the robot.
;; WHERE:Path traversed is the path that the robot has traversed yet.
;; RETURNS: all the successors which match the condition of a
;; potential successor i.e:
;; 1. the given potential-successor is not a block,
;; 2. the given potential-successor is not already traversed. 
;; 3. the given potential-successor is within the bounding walls.
;; Halting Measure:When all the successors in the nodes are assigned or false
;; in case of no faesible successor.
;; EXAMPLES:
;; (successors (list 2 2) (list (list 1 1) (list 4 4))
;;                            (list (list 5 5)) (list 6 6) (list 1 1))
;;
;;(list
;; (list 3 3)
;; (list 1 3)
;; (list 3 1))
;;Strategy:Use HOF filter on succeessors.

;;Function Definition.
(define (successors sin-block loblocks path-traversed target initial-pos)
  (filter
   ;;Position -> Boolean
   ;;GIVEN: the successor of the present location..
   ;;RETURNS: True if
   ;; 1. the given potential-successor is not a block,
   ;; 2. the given potential-successor is not already traversed. 
   ;; 3. the given potential-successor is within the walls.
   
   (lambda(p)(potential-successor? p loblocks path-traversed target initial-pos))
   (node-successors sin-block)))


;;path-locus:Position Position ListOfBlocks
;;                         ListOfPositions ListOfPositions->MaybePlan
;;Given: A initial-pos and a target position,list of blocks,
;;       list of positions already visited and the locus of the robot traversed.
;;WHERE:Path traversed are the location s visited  by the robot currently.
;;      current-path is the path that the robot has traversed yet.
;;Returns :A trajectory followed by robot from initial position to the target.
;;Strategy:Use cases on list of positions.
;;Halting Measure:When the target is reached or when no path can be determine
;;                due to path deadlock.
;;Termination Argument:At every recursive call the number of possible successor
;;deacreases
;;      and the list may terminate or will become zero if the target is reached.

;;Examples
;;(path-locus (list 2 5)(list 4 9)
;;              (rest wall1)(list (list 1 1))(list (list 2 2)))
;;#false

;;Function Definition:
(define (path-locus initial-pos target loblocks path-traversed current-path)
  (cond
    [(belongs-to? initial-pos loblocks) false]
    [(belongs-to? target loblocks) false]
    [(= (length path-traversed) NO-ELEMENT) false]
    [else(form-lists initial-pos target loblocks path-traversed current-path)]))

;;form-lists Position Position ListOfBlocks
;;                         ListOfPositions ListOfPositions->MaybePlan

;;Given: A initial-pos and a target position,list of blocks,
;;       list of positions already visited and the locus of the robot traversed.
;;WHERE:Path traversed are the location s visited  by the robot currently.
;;      current-path is the path that the robot has traversed yet.
;;Returns :A trajectory followed by robot from initial position to the target.
;;Strategy:Use cases on list of positions.
;;Example:
;;(form-lists (list 2 5)(list 4 9)
;;            empty(list (list 1 1))(list (list 2 2)))
;;#false


;;Function Definition:
(define (form-lists initial-pos target loblocks path-traversed current-path)
  (if (equal? (first path-traversed) target) 
      (form-move path-traversed)
      (if (empty? (blocks initial-pos target path-traversed current-path loblocks))
          (list-locus initial-pos target loblocks path-traversed current-path)
          (form-list-extention initial-pos target path-traversed current-path loblocks))))

;;list-locus Position Position ListOfBlocks
;;                         ListOfPositions ListOfPositions->MaybePlan

;;Given: A initial-pos and a target position,list of blocks,
;;       list of positions already visited and the locus of the robot traversed.

;;Returns :A trajectory followed by robot from initial position to the target
;;returns false if no position is added.
;;Strategy:Use cases on list of positions.

;;Example:
;;(list-locus (list 2 5)(list 4 9)
;;            (rest wall1)(list (list 1 1))(list (list 2 2)))
;;#false
;;Function Definition:
(define (list-locus initial-pos target loblocks path-traversed current-path)
  (path-locus initial-pos target loblocks (rest path-traversed) 
              (cons (first path-traversed) current-path)))

;;blocks Position Position ListOfBlocks
;;                         ListOfPositions ListOfPositions->MaybePlan
;;Given: A initial-pos and a target position,list of blocks,
;;       list of positions already visited and the locus of the robot traversed.
;;Returns :A trajectory followed by robot from initial position to the target.
;;Strategy:Use cases on list of positions.
;;Examples:
;;(blocks (list 2 5)(list 4 9)
;;              (rest wall1)(list (list 1 1))(list (list 2 2)))
;;(list
;; (list 3 4)
;; (list 1 4)
;; (list 1 2)
;; (list 3 2))

;;Function Definition:
(define (blocks initial-pos target path-traversed current-path loblocks)
  (successors (first path-traversed) 
              loblocks current-path target initial-pos))

;;form-list-extention Position Position ListOfBlocks
;;                         ListOfPositions ListOfPositions->MaybePlan

;;Given: A initial-pos and a target position,list of blocks,
;;       list of positions already visited and the locus of the robot traversed.
;;Returns :A trajectory followed by robot from initial position to the target.
;;Strategy:Use cases on list of positions.

;;Example
;;(form-list-extention (list 2 5)(list 4 9)
;;              (rest wall1)(list (list 1 1))(list (list 2 2)))
;;(list
;; (list "sw" 2)
;; (list "ne" 1)
;; (list "sw" 1)
;; (list "ne" 1)
;; (list "sw" 1)
;; (list "se" 2)
;; (list "sw" 1)
;; (list "se" 1)
;; (list "sw" 1)
;; (list "se" 1))

;;Function Definition
(define (form-list-extention initial-pos target path-traversed current-path loblocks)
  (path-locus initial-pos target loblocks
              (cons
               (first (successors (first path-traversed)
                                  loblocks current-path target initial-pos))
               path-traversed)(cons (first (successors (first path-traversed) loblocks
                                                       current-path target initial-pos))
                                    current-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;eval-plan : Position ListOfPosition Plan ->  MaybePosition
;;GIVEN:
;;1. the starting position of the robot,
;;2. A list of the blocks on the board
;;3. A plan for the robot's motion
;;RETURNS:
;;The position of the robot at the end of executing the plan, or false
;;if  the plan sends the robot to or  through any block.
;;Strategy:Use template on list
;;Example:
;;  (eval-plan (list 2 5) (rest wall1) (path (list 2 5) (list 4 9) wall1))
;;   #false
;;   "Returns false after moving the assigned path")
;;Function Definition:
(define (eval-plan node blocks plan)
  (cond
    [(empty? plan) node]
    [(equal? false plan)
     false]
    [(nodes-between-clear? node (first plan) blocks)
     (eval-plan (next-node node(first (first plan))
                           (second (first plan)))blocks(rest plan))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;node-between-clear? Move Move loblocks->boolean
;;Given: A Position A Position List of block.
;;Returns:Returns true if there are no blocks in the way else returns false.
;;Strategy:Use HOF andmap .
;;Example:

;;Function-Definition:
(define (nodes-between-clear? prev move blocks)
  (andmap (lambda (n)
            (not (belongs-to? n blocks)))
          (all-nodes empty prev (first move) (second move))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;all-nodes Position Position Position INT
;;Given: A start position next position position and no. of steps.
;;Returns:The position for the nodes returns all if there are no element.
;;Strategy:Combine simpler function.
;;Example:
;;(all-nodes (list 2 2)(list 2  2)(list 3 3) 0)
;;(list 2 2)

;;Function-Definition:
(define (all-nodes all prev dir steps)
  (cond
    [(= NO-ELEMENT steps) all]
    [else
     (all-nodes (cons prev (next-node prev dir ADDENDS)) (next-node prev dir ADDENDS) dir
                (sub1 steps))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;next-node Position Position Int ->Position.
;;Given:A position ,a position and an integer value for addition.
;;Returns:The nect node for the given node.  
;;Strategy:Use template onlist.
;;Example:
;;(next-node (list 1 1) (list 3 3) 1)
;;Function-Definition:
(define (next-node node dir addend)
  (cond
    [(equal? SOUTH-EAST dir)
     (list (+ (first node) addend) (+ (second node) addend))]
    [(equal? SOUTH-WEST dir)
     (list (- (first node) addend) (+ (second node) addend))]
    [(equal? NORTH-EAST dir)
     (list (+ (first node) addend) (- (second node) addend))]
    [(equal? NORTH-WEST dir)
     (list (- (first node) addend) (- (second node) addend))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tests:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Test for path function.
(begin-for-test
  (check-equal?
   TEST-PATH1
   TEST1-RESULT
   "Returns the path to be traversed by the Robot")
  
  (check-equal?
   TEST-PATH2
   #false
   "Returns false as initial-pos part of list of blocks")
  
  (check-equal?
   TEST-PATH3
   #false
   "Returns false as target part of list of blocks")
  
  (check-equal? TEST-PATH4
                #false
                "Returns false as successors returns the path to be empty")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Test for boundary condition.
  (check-equal?
   (in-boundary TEST-LIST1
                list3 list4)
   list6
   "Returns the boundary value for the condition")
  
  (check-equal?
   (append-unidirectional-moves (list (list "ne" 1) (list "sw" 1)))
   (list (list "ne" 1) (list "sw" 1))
   "Returns the moves for the robot")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Test for move append   
  (check-equal?
   (append-unidirectional-moves empty)
   '()
   "Returns empty list as there are no moves possible")
  
  
  (check-equal?
   (concat-checker "ne"empty)
   NORTH-EAST
   "Returns the move for empty list")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Test To Check Direction.
  (check-equal?
   (find-direction list5 list3 )
   TEST-DIRECTION-RESULT2
   "Returns the direction for the move")
  
  (check-equal?
   (find-direction list1 list3 )
   TEST-DIRECTION-RESULT2
   "Returns the direction for the move")
  
  (check-equal?
   (find-direction list1 list5)
   TEST-DIRECTION-RESULT4 
   "Returns the direction for the move")
  
  
  (check-equal?
   (find-direction list1 list2)
   TEST-DIRECTION-RESULT1
   "Returns the direction for the move")
  (check-equal?
   (eval-plan (list 2 5) (rest wall1) (list (list "ne" 1)))
   (list 3 4)
   "Returns the node after moving the assigned path")
  (check-equal?
   (eval-plan (list 2 5) (rest wall1) (path (list 2 5) (list 4 9) wall1))
   #false
   "Returns false after moving the assigned path")
  (check-equal?
   (eval-plan (list 2 5) (rest wall1) (path (list 2 5) (list 4 9) wall1))
   #false
   "Returns false after moving the assigned path")
  (check-equal?
   (eval-plan (list 2 5) (rest wall1) (list (list "se" 1)))
   (list 3 6)
   "Returns the node after moving the assigned path")
  (check-equal?
   (eval-plan (list 2 5) (rest wall1) (list (list "sw" 1)))
   (list 1 6)
   "Returns the node after moving the assigned path")
  (check-equal?
   (eval-plan (list 2 5) (rest wall1) (list (list "nw" 1)))
   (list 1 4)
   "Returns the node after moving the assigned path"))


(define TEST-PATH1 (path (list 1 1)(list 4 4)empty))
(define TEST-PATH2 (path (list 1 1 ) (list 2 2) (list (list 1 1))))
(define TEST-PATH3 (path (list 1 1 ) (list 2 2) (list (list 2 2))))
(define TEST-PATH4 (path (list 1 1)(list 1 2)empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**************************************************************************
;**************`************************************************************