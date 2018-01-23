;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Problem set 08;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

(provide
 eval-plan
 path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;; A Position is a (list Integer Integer)
;; INERPRETATION:
;; (list x y) represents the position (x,y) on the chessboard
;; WHERE: x and y are Integers
;; position-fn : Position -> ??
#; (define (position-fn pos)
     (...(first pos)
         (second pos)))

;; Example:
(define pos1 (list 2 5))
(define pos2 (list -5 -10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListofPositions (LOP) is one of
;; -- empty
;; -- (cons Position LOP)
;; WHERE: a LOP is a list of positions without duplication
;; list-fn : LOP -> ??
#; (define (list-fn lst)
     (cond
       [(empty lst)...]
       [else
        (position-fn (first lst)
                     (list-fn (rest lst)))]))

;; Examples:
(define pos-set-1 (list (list 2 5) (list 3 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListofPositionDirection (LPD) is one of
;; -- empty
;; -- (cons PositionDirection LPD)
;; WHERE: a LPD is a list of position and direction
;; list-fn : LPD -> ??
#; (define (list-fn lst)
     (cond
       [(empty? lst)...]
       [else
        (pos-dir-fn (first lst)
                    (list-fn (rest lst)))]))

;; Examples:
(define pos-dir (list (list 2 5) "ne"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListofListofPositionDirection (LOLPD) is one of
;; -- empty
;; -- (cons LPD LOLPD)
;; WHERE: a LOLPD is a list of list of position and direction
;; list-fn : LOLPD -> ??
#;(define (list-fn lst)
    (cond
      [(empty? lst)...]
      [else
       (list-pos-dir-fn (first lst)
                        (list-fn (rest lst)))]))

;; Examples:
(define list-pos-dir (list (list (list 2 5) "ne")
                           (list (list 3 6) "se")
                           (list (list 4 9) "sw")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListofDistancePositionDirection (LDPD) is one of
;; -- empty
;; -- (cons DistancePositionDirection LDPD)
;; WHERE: a LDPD is a list of distance, position and direction
;; list-fn : LDPD -> ??
#; (define (list-fn lst)
     (cond
       [(empty? lst)...]
       [else
        (dis-pos-dir-fn (first lst)
                        (list-fn (rest lst)))]))

;; Examples:
(define dis-pos-dir (list 5 (list 2 5) "ne"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListofListofDisPositionDirection (LOLDPD) is one of
;; -- empty
;; -- (cons LDPD LOLDPD)
;; WHERE: a LOLDPD is the list of list of distance, position and direction
;; list-fn : LCPD -> ??
#;(define (list-fn lst)
    (cond
      [(empty? lst)...]
      [else
       (list-dis-posdir-fn (first lst)
                           (list-fn (rest lst)))]))

;; Examples:
(define list-dis-pos-dir (list
                          (list 5 (list 2 5) "ne")
                          (list 10 (list 4 5) "se")
                          (list 3 (list 4 9) "sw")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction.
#; (define (move-fn move)
     (...(dir-fn (first move))
         (second move)))

;; Examples:
(define move1 (list "ne" 1))
(define move2 (list "sw" 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListofMove (LOM) is one of
;; -- empty
;; -- (cons Move LOM)
;; move-fn : LOM -> ??
#; (define (lomove-fn lst)
     (cond
       [(empty? lst)...]
       [else
        (...
         (move-fn (first lst))
         (lomove-fn (rest lst)))]))

;; Examples:
(define list-move (list (list "ne" 1)
                        (list "sw" 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Plan is a special ListOfMove is one of
;; -- empty
;; -- (cons Move Plan)
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.
;; list-fn : Plan -> ??
#; (define (plan-fn lst)
     (cond
       [(empty lst)...]
       [else
        (move-fn (first lst)
                 (plan-fn (rest lst)))]))

;; Examples:
(define PLAN (list (list "ne" 1) (list "sw" 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Direction is one of
;; -- "ne" INTRP: ne represents movement of robot towards northeast direction on
;;                chessboard
;; -- "se" INTRP: se represents movement of robot towards southeast direction on
;;                chessboard
;; -- "sw" INTRP: sw represents movement of robot towards southwest direction on
;;                chessboard
;; -- "nw" INTRP: nw represents movement of robot towards northwest direction on
;;                chessboard
#; (define (dir-fn dir)
     (cond
       [(string=? dir NE) ...]
       [(string=? dir SE) ...]
       [(string=? dir SE) ...]
       [(string=? dir SW) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MaybePlan is one of
;; -- false   INTERP: There is no path from start position of robot to final position
;; -- Plan    INTERP: There exists a path for robot to reach the final position from
;;                    the given start position
;; may-be-plan : MBP -> ??
#; (define (may-be-plan-fn mbp)
     (cond
       [(false? mbp)...]
       [else
        (plan-fn mbp)]))

;; Examples:
(define may-be-plan-false false)
(define may-be-plan PLAN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MaybePosition is one of
;; -- false       INTERP: There exists a block and consequently the position cannot
;;                        be travelled by the robot
;; -- Position    INTERP: Final position to be reached by the robot
;; may-be-pos : MBPOS -> ??
#; (define (may-be-pos mbpos)
     (cond
       [(false? mbpos)...]
       [else
        (pos-fn mbpos)]))

;; Examples:
(define may-be-pos-false false)
(define may-be-pos pos1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Operator is one of
;; -- +
;; -- -

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Path is a ListofPositions
;; INTERP: a path is a list of positions
;; WHERE: each position is a consecutive move made by the robot with distance one

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS:

(define pos5 (list 2 5))
(define blocks1 (list
                 (list 0 5)
                 (list 0 7)
                 (list 2 3)
                 (list 2 7)
                 (list 4 3)
                 (list 4 5)
                 (list 4 7)))
(define plan1 (list
               (list "sw" 2)
               (list "se" 3)
               (list "ne" 2)))

(define plan3 (list
               (list "sw" 2)))
(define plan2 (list
               (list "ne" 2)))
(define wall1
  '((0 3)(2 3)     (4 3)
         (0 5)     (4 5)
         (0 7)(2 7)(4 7)))

(define two-walls
  '((0 3)(4 3)
         (0 5)(4 5)
         (0 7)(4 7)
         (0 9)(4 9)
         (0 11)(4 11)))

(define LIST
  (list
   (list "ne" 1)
   (list "se" 1)
   (list "sw" 1)
   (list "nw" 1)))

(define NE "ne")
(define NW "nw")
(define SE "se")
(define SW "sw")

(define OPERAND1 +)
(define OPERAND2 -)
(define ZERO 0)
(define ONE 1)
(define DIR "ne")
(define START (list 2 5))
(define FINAL1 (list 5 2))
(define PLAN4 (list (list "nw" ONE)))
(define FINAL2 (list 1 6))
(define BLOCKS1 (list (list 3 6)))
(define PLAN5 (list (list "ne" 1)))
(define BLOCKS2
  (list (list 3 6)
        (list 1 6)
        (list 3 4)
        (list 1 4)))
(define FOUR 4)
(define TWO 2)

(define PATH1
  (list
   (list "se" 1)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 2)
   (list "sw" 1)
   (list "nw" 2)
   (list "nw" 2)
   (list "nw" 2)
   (list "nw" 1)
   (list "sw" 2))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eval-plan : Position LOP Plan -> MaybePosition
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. A list of the blocks on the board
;; 3. A plan for the robot's motion
;; WHERE: Plan is a list of moves following which the robot may or may not reach
;; the final position
;; RETURNS:
;; The position of the robot at the end of executing the plan, or false
;; if the plan sends the robot to or through any block.
;; HALTING MEASURE: length of plan
;; TERMINATION ARGUMENT: If a block is encounterd while executing the plan,
;; fucntion terminates with a false, else at each recursive call next move from
;; plan is executed and the length of plan decreases
;; Examples/Tests:
(begin-for-test
  (check-equal? (eval-plan START (rest wall1) plan1) FINAL1)
  (check-equal? (eval-plan START wall1 PLAN4) FINAL2)
  (check-equal? (eval-plan START BLOCKS1 PLAN5) false))
;; Design Startegy: General recursion on may-be-eval-plan
(define (eval-plan pos blocks plan) 
  (cond 
    [(empty? plan) pos]
    [else
     (may-be-eval-plan plan pos blocks)]))


;; may-be-eval-plan : Plan Position LOP -> MaybePosition
;; GIVEN: list of moves as plan, a start position, list of positions as blocks
;; WHERE: Plan is a list of moves following which the robot may or may not reach
;; the final position
;; RETURNS: final position if reachable or false
;; Examples/Tests: See function eval-plan
;; Design Strategy: Use template for Plan on plan
(define (may-be-eval-plan plan pos blocks)
  (cond
    [(boolean? (execute-plan (first plan) pos blocks)) false]
    [else
     (eval-plan (execute-plan (first plan) pos blocks)
                blocks (rest plan))]))


;; execute-plan : Move Position LOP -> MayBePosition
;; GIVEN: a move for robot to move, starting position of robot, list of positions
;; as blocks
;; RETURNS: new position of the robot if during movement it doesnot encounter any
;; block else false
;; Examples/Tests: See function eval-plan
;; Design Strategy: Cases on direction in move
(define (execute-plan move pos blocks)
  (cond
    [(string=? (first move) "ne") (move-ahead (second move)
                                              pos blocks OPERAND1 OPERAND1)]
    [(string=? (first move) "nw") (move-ahead (second move)
                                              pos blocks OPERAND2 OPERAND1)]
    [(string=? (first move) "se") (move-ahead (second move)
                                              pos blocks OPERAND1 OPERAND2)]
    [(string=? (first move) "sw") (move-ahead (second move)
                                              pos blocks OPERAND2 OPERAND2)]))


;; move-ahead : Move Position LOP -> MaybePosition
;; GIVEN: a move with a direction and steps, start position, list of positions
;; as blocks
;; WHERE: initially number of steps is greater than zero
;; RETURNS: new position of the robot in the direction specified in the given move
;; if during movement it doesnot encounter any block else false
;; Examples/Tests: See function eval-plan
;; HALTING MEASURE: (steps - 1)
;; TERMINATION ARGUMENT: If there are no further steps that can be travelled,
;; function terminates, else at each recursive call steps keep on decreasing
;; Design Strategy: Cases on steps
(define (move-ahead steps pos blocks op1 op2)
  (if
   (= steps ZERO)
   pos
   (may-be-position pos blocks steps op1 op2)))

;; may-be-position : Position LOP NonNegInt Operator Operator -> MaybePosition
;; GIVEN: a position, a list of positions as blocks, number of steps and an operator
;; RETURNS: new position of the robot in the direction specified in the given move
;; if during movement it doesnot encounter any block else false
;; Examples/Tests: See function eval-plan
;; Design Strategy: Use template for Position on pos
(define (may-be-position pos blocks steps op1 op2)
  (if
   (check-move? pos blocks op1 op2)
   (move-ahead (- steps ONE)
               (list (op1 (first pos) ONE) (op2 (first (rest pos)) ONE))
               blocks op1 op2)
   false))

;; check-move? : Position LOP Operator Operator -> Boolean
;; GIVEN: a position, a list of positions as blocks, and two operators
;; RETURNS: true iff the next move of the robot in this direction doesnot
;; encounter a block
;; Examples/Tests: See function eval-plan
;; Design Strategy: Use HOF andmap on blocks
(define (check-move? pos blocks op1 op2)
  (andmap
   ;; Position -> Boolean
   ;; RETURNS: true iff the position is a block
   (lambda (block) (not (check-block? pos block  op1 op2)))
   blocks))

;; check-block? : Position Position Operator Operator
;; GIVEN: a position of robot, position of a block and two opertators
;; RETURNS: true if robot's position is same as the position of block
;; Examples/Tests: See function eval-plan
;; Design Strategy: Combine simpler functions
(define (check-block? pos block op1 op2)
  (and (= (op1 (first pos) ONE) (first block))
       (= (op2 (second pos) ONE) (second block))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path : Position Position ListOfPosition -> MaybePlan
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; WHERE: the starting position and final position is not a member of list of blocks
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; Examples/Tests:
(begin-for-test
  (check-equal? (path (list 2 5) (list 2 6) empty) false)
  (check-equal? (path (list -3 6) (list 7 6) two-walls) PATH1))
;; Design Strategy: Cases on position
(define (path start-pos final-pos blocks)
  (if
   (check-position? start-pos final-pos)
   (check-final-blocked? start-pos final-pos blocks)
   false))

;; check-position? : Position Position -> Boolean
;; GIVEN: start and final position
;; RETUNS: true iff the final position is reachable when robot is allowed
;; to move only diagonally
;; Examples/Tests:
(begin-for-test
  (check-equal? (check-position? (list 2 2) (list 4 4)) true)
  (check-equal? (check-position? (list 2 2) (list 2 3)) false))
;; Design Strategy: Combine simpler functions
(define (check-position? pos1 pos2)
  (= (modulo(+ (first pos1) (second pos1))TWO)
     (modulo(+ (first pos2) (second pos2))TWO)))

;; check-final-blocked? : Position Position LOP -> MaybePlan
;; GIVEN: starting position of block, final position to be reached,
;; list of positions as blocks
;; RETURNS: false if the final position is blocked and cannot be
;; reached by the robot or a path following which the robot will reach destination
;; Examples/Tests:
(begin-for-test
  (check-equal? (check-final-blocked? (list 1 1) (list 2 5) BLOCKS2) false))
;; Design Strategy: Cases on final position
(define (check-final-blocked? start-pos final-pos blocks)
  (if
   (find-blocks-for-target final-pos final-pos blocks ZERO
                           (calculate-limit blocks) start-pos DIR)
   false
   (get-list-position start-pos final-pos blocks empty DIR)))

;; calculate-limit : LOP -> NonNegInt
;; GIVEN: a list of positions as blocks
;; RETURNS: the division of number of blocks by four, rounded off to highest vlaue
;; Examples/Tests:
(begin-for-test
  (check-equal? (calculate-limit wall1) 2))
;; Design Strategy: Combine simpler functions
(define (calculate-limit blocks)
  (ceiling (/ (length blocks) FOUR)))

;; generate-path : LOM -> LOM
;; GIVEN: a list of moves with number of steps travelled as one
;; RETURNS: a list of moves generated with either two moves combined as one by adding 
;; the number of steps added for two consecutive move in the same direction or
;; moves appended in the list of moves
;; Examples/Tests:
(begin-for-test
  (check-equal? (generate-path (list (list "ne" 1) (list "sw" 1) (list "sw" 1)))
                (list (list "ne" 1) (list "sw" 2)))
  (check-equal? (generate-path (list (list "ne" 1)))
                (list (list "ne" 1))))
;; Design Strategy: Use template for LOM on lst
(define (generate-path lst)
  (cond
    [(empty? (rest lst)) lst]
    [else
     (if
      (check-direction-same? lst)
      (append-same-move lst)
      (append-move lst))]))

;; check-direction-same? : LOM -> Boolean
;; GIVEN: a list of moves
;; RETURNS: true if two consecutive moves are in the same direction
;; Examples/Tests: See function generate-path
;; Design Startegy: Combine simpler functions
(define (check-direction-same? lst)
  (string=? (first (first lst)) (first (second lst))))

;; append-same-move : LOM -> LOM
;; GIVEN: a list of moves with number of steps travelled one, in the same direction
;; RETURNS: a list of moves with steps added for consecutive move in the same
;; direction
;; Examples/Tests: See function generate-path
;; Design Strategy: Use template for LOM on lst
(define (append-same-move lst)
  (cons
   (add-steps (first lst) (second lst))
   (if
    (empty? (rest (rest lst))) 
    empty
    (generate-path (rest (rest lst))))))

;; append-move : LOM -> LOM
;; GIVEN: a list of moves with number of steps travelled one, in different direction
;; RETURNS: a list of moves when two consecutive moves are not in the same direction
;; Examples/Tests: See function generate-path
;; Design Strategy: Use template for LOM on lst
(define (append-move lst)
  (cons
   (first lst)
   (generate-path (rest lst))))


;; add-steps : Move Move -> Move
;; GIVEN: two moves in the same direction travelled by the robot
;; RETURNS: a move in the same direction, with number of steps added
;; Examples/Tests: See function generate-path
;; Design Strategy: Use template for Move on m1 and m2
(define (add-steps m1 m2)
  (list (first m2) (+ (second m1) (second m2))))

;; get-list-path : LOP -> LOM
;; GIVEN: a list of positions tarversed by the robot
;; RETURNS: a list of moves according to the given list of positions
;; Examples/Tests:
(begin-for-test
  (check-equal? (get-list-path (list(list 1 4) (list 2 5) (list 3 6)))
                (list (list "ne" 1) (list "sw" 1) (list "sw" 1))))
;; Design Strategy: Use HOF map on lst
(define (get-list-path lst)
  (map
   ;; Position Position -> Move
   ;; RETURNS: a move with respect to two positions given
   (lambda (s) (get-path s (second lst)))
   lst))

;; get-path : Position Position -> Move
;; GIVEN: two positons
;; RETURNS: a move with respect to the new and old position
;; Examples/Tests:
(begin-for-test
  (check-equal? (get-path (list 2 2) (list 3 4)) (list NE 1))
  (check-equal? (get-path (list 2 2) (list 3 1)) (list SE 1))
  (check-equal? (get-path (list 3 2) (list 3 3)) (list NW 1))
  (check-equal? (get-path (list 4 2) (list 3 1)) (list SW 1)))
;; Design Strategy: Cases on position
(define (get-path pos1 pos2)
  (if
   (> (first pos2) (first pos1))
   (get-east-path pos1 pos2)
   (get-west-path pos1 pos2)))

;; get-east-path : Position Position -> Move
;; GIVEN: two positions
;; RETURNS:amove in the east direction
;; Examples/Tests: See function get-path
;; Design Strategy: Cases on position
(define (get-east-path pos1 pos2)
  (if
   (> (second pos2) (second pos1))
   (list NE ONE)
   (list SE ONE)))

;; get-west-path : Position Position -> Move
;; GIVEN: two positions
;; RETURNS:amove in the west direction
;; Examples/Tests: See function get-path
;; Design Strategy: Cases on position
(define (get-west-path pos1 pos2)
  (if
   (> (second pos2) (second pos1))
   (list NW ONE) 
   (list SW ONE)))

;; compare : LDPD LDPD -> Boolean
;; GIVEN: two lists of list of distance, position and direction
;; RETURNS: true iff the distance in first list is less than distance in second list
;; Examples/Tests: See function find-list-dir
;; Design Strategy:  Use template for LDPD on x and y
(define (compare x y)
  (<
   (first x)
   (first y)))

;; get-list-position : Position Position LOP Path Direction -> MaybePlan
;; GIVEN: starting position, final position, list of positions as blocks, a path
;; and a direction
;; WHERE:
;; 1. The starting position initially is the given start position, but at each
;; call chnages as the new position returned by function move-further.
;; 2. Initially the path is empty into which each time a position will be added
;; if it leads to final position
;; 3. Direction is one which will be placed in the end of the list 
;; of directions to be traversed from a particular position, which is initially
;; given a random value
;; RETURNS: a plan when executed will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; Design Strategy: Cases on starting and final positions
(define (get-list-position start-pos final-pos blocks path dir)
  (if 
   (final-reached? start-pos final-pos)
   (generate-path (get-list-path (reverse (cons start-pos path))))
   (move-further start-pos blocks(get-direction-list start-pos final-pos dir)
                 final-pos path)))

;; final-reached? : Position Position -> Boolean
;; GIVEN: current position of robot and final position
;; RETURNS: true iff the both the positions are same, i.e.final position is reached
;; by the robot
;; Exmples/Tests: See function get-list-position
;; Design Strategy: Combine simpler functions
(define (final-reached? pos1 pos2)
  (=(+
     (abs(- (first pos1) (first pos2)))
     (abs(- (second pos1) (second pos2))))ZERO))

;; move-further : Position LOP LOM Position Path -> MaybePlan
;; GIVEN: a starting position, list of positions as blocks, a list of moves,
;; final position, a path
;; WHERE:
;; 1. The starting position initially is the given start position, but at each
;; call chnages as the new position returned by get-list-position.
;; 2. The path (list of positions)is initailly taken as empty, but at each iteration 
;; when a position is found appropriate towards reaching final position, the
;; position is added to this list of positons, generated as plan
;; RETURNS: a plan when executed will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; HAULTING MEASURE: List of Moves
;; TERMINATING ARGUMENT: the function terminates with a false if the list of moves
;; is empty, else at each recursive next move is examined from the list of moves
;; Design Strategy: General recursion on may-be-move-further
(define (move-further pos blocks lst trgt path)
  (cond
    [(empty? lst) #f]
    [else 
     (if
      (boolean? (eval-plan pos blocks (list (first lst))))
      (move-further pos blocks (rest lst) trgt path)
      (may-be-move-further pos blocks lst trgt path))]))

;; may-be-move-further : Position LOP LOM Position Path -> MaybePlan
;; GIVEN: a starting position, list of positions as blocks, a list of moves,
;; final position, a path
;; WHERE:
;; 1. The starting position initially is the given start position, but at each
;; call chnages as the new position returned by function get-list-position.
;; 2. The path (list of positions)is initailly taken as empty, but at each iteration 
;; when a position is found appropriate towards reaching final position, the
;; position is added to this list of positions, geenerated as plan
;; RETURNS: a plan when executed will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; Examples/Tests:
(begin-for-test
  (check-equal? (may-be-move-further (list 2 5) wall1 (list (list "ne" 1))
                                     (list 4 9) empty) false))
;; Design Strategy: Cases on list of moves
(define (may-be-move-further pos blocks lst trgt path)
  (if (empty? (rest lst))
      (get-list-position (eval-plan pos blocks (list (first lst)))
                         trgt (cons pos blocks) (cons pos path)
                         (opposite-direction (first (first lst))))
      (get-list-position (eval-plan pos blocks (list (first lst)))
                         trgt blocks (cons pos path)
                         (opposite-direction (first (first lst))))))

;; opposite-direction : Direction -> Direction
;; GIVEN: a direction
;; RETURNS: opposite direction to the given direction
;; Design Strategy: Cases on direction
(define (opposite-direction dir)
  (cond
    [(string=? "ne" dir) "sw"]
    [(string=? "nw" dir) "se"]
    [(string=? "se" dir) "nw"]
    [(string=? "sw" dir) "ne"]))

;; calculate-distance-list : LOM Position -> LCPD
;; GIVEN: a list of move, a final position
;; RETURNS: same list with distance form the final postion appended to it
;; Examples/Tests:
(begin-for-test
  (check-equal? (calculate-distance-list  (list
                                           (list (list 3 6) "ne")
                                           (list (list 3 4) "se")
                                           (list (list 1 4) "sw")
                                           (list (list 1 6) "nw")) (list 4 9))
                (list
                 (list 4 (list (list 3 6) "ne"))
                 (list 6 (list (list 3 4) "se"))
                 (list 8 (list (list 1 4) "sw"))
                 (list 6 (list (list 1 6) "nw")))))
;; Design Startegy: Use HOF map on lst
(define (calculate-distance-list lst trgt)
  (map
   ;; Move -> NonNegInt
   ;; RETURNS: distance of robot from final position
   (lambda (pos) (calculate-distance pos trgt))
   lst))

;; calculate-distance : Move Position -> LDPD
;; GIVEN: a move and the final position
;; RETURNS: same list with distance from final position appended to it
;; Examples/Tests: See function calculate-distance-list
;; Design Strategy: Use template for move on m
(define (calculate-distance m trgt)
  (list
   (+
    (abs(- (first (first m)) (first trgt)))
    (abs(- (second (first m)) (second trgt))))
   m))

;; get-direction-list : Position Position Direction -> LOM
;; GIVEN: two positions and a direction
;; RETURNS: list of moves to be traversed with the move in given direction at last
;; Examples/Tests:
(begin-for-test
  (check-equal?  (get-direction-list (list 2 5) (list 4 9) NE)
                 (list
                  (list "se" 1)
                  (list "nw" 1)
                  (list "sw" 1)
                  (list "ne" 1))))
;; Design Startegy: Combine simpler functions
(define (get-direction-list start-pos final-pos dir)
  (convert-to-move(add-dir-to-last
                   (remove-dir start-pos final-pos dir)
                   (find-list-dir start-pos final-pos dir))))

;; find-list-dir : Position Position Direction -> LOLDPD
;; GIVEN: a starting and final position and a direction
;; RETURNS: a list of distance, position, direction, but the element of list 
;; removed with which the given direction matches
;; Examples/Tests:
(begin-for-test
  (check-equal? (find-list-dir (list 2 5) (list 4 9) NE)
                (list 4 (list (list 3 6) "ne"))))
;; Design Strategy: Combine simpler functions
(define (find-list-dir start-pos final-pos dir)
  (find-dir 
   (sort
    (calculate-distance-list
     (traverse-direction start-pos LIST)
     final-pos) compare)
   dir))

;; remove-dir : Position Position Direction -> LOLDPD
;; GIVEN: a starting and final position and a direction
;; RETURNS: a list of distance, position, direction, but the element of list 
;; removed with which the given direction matches
;; Examples/Tests:
(begin-for-test
  (check-equal? (remove-dir (list 2 5) (list 4 9) NE)
                (list
                 (list 6 (list (list 3 4) "se"))
                 (list 6 (list (list 1 6) "nw"))
                 (list 8 (list (list 1 4) "sw")))))
;; Design Strategy: Combine simpler functions
(define (remove-dir start-pos final-pos dir)
  (adjust-for-dir
   (sort
    (calculate-distance-list
     (traverse-direction start-pos LIST)
     final-pos) compare)
   dir))

;; convert-to-move : LOLDPD -> LOM
;; GIVEN: a list of distance, position and direction
;; RETURNS : list of moves and distances removed
;; Examples/Tests:
(begin-for-test
  (check-equal? (convert-to-move (list
                                  (list 4 (list (list 3 6) "ne"))
                                  (list 6 (list (list 1 6) "nw"))
                                  (list 8 (list (list 1 4) "sw"))
                                  (list 6 (list (list 3 4) "se"))))
                (list
                 (list "ne" 1)
                 (list "nw" 1)
                 (list "sw" 1)
                 (list "se" 1))))
;; Design Strategy: Use template on LOLDPD on lst
(define (convert-to-move lst)
  (cond
    [(empty? lst) empty]
    [else (cons (list (second (second (first lst))) ONE)
                (convert-to-move (rest lst)))]))


;; add-dir-to-last : LOLDPD LDPD -> LOLDPD
;; GIVEN: a list of list of distance, position, direction and a list of distance,
;; position, direction
;; RETURNS: the second list added to the first list in the end
;; Examples/Tests:
(begin-for-test
  (check-equal? (add-dir-to-last (list
                                  (list 4 (list (list 3 6) "ne"))
                                  (list 6 (list (list 1 6) "nw"))
                                  (list 8 (list (list 1 4) "sw")))
                                 (list 6 (list (list 3 4) "se")))
                (list
                 (list 4 (list (list 3 6) "ne"))
                 (list 6 (list (list 1 6) "nw"))
                 (list 8 (list (list 1 4) "sw"))
                 (list 6 (list (list 3 4) "se")))))
;; Design Strategy: Use template for LOLDPD on lst
(define (add-dir-to-last lst lst-to-add)
  (reverse (cons lst-to-add (reverse lst))))

;; adjust-for-dir : LOLDPD Direction -> LOLDPD
;; GIVEN: a list of distance, position, direction and a direction
;; RETURNS: same list but the element of list removed with which the given direction
;; matches
;; Examples/Tests:
(begin-for-test
  (check-equal?(adjust-for-dir (list
                                (list 4 (list (list 3 6) "ne"))
                                (list 6 (list (list 3 4) "se"))
                                (list 6 (list (list 1 6) "nw"))
                                (list 8 (list (list 1 4) "sw")))
                               SE)(list
                                   (list 4 (list (list 3 6) "ne"))
                                   (list 6 (list (list 1 6) "nw"))
                                   (list 8 (list (list 1 4) "sw")))
                                  ))
;; Design Strategy: Use template for LOLDPD on lst
(define (adjust-for-dir lst dir)
  (cond
    [(empty? lst) empty]
    [else
     (if (string=?  (second (second (first lst))) dir) 
         (adjust-for-dir  (rest lst) dir)
         (cons (first lst) (adjust-for-dir (rest lst) dir)) )]))

;; find-dir : LOLDPD Direction -> LDPD
;; GIVEN: a list of list of distance, position, direction and a direction
;; RETURNS: the list which matches the given direction
;; Examples/Tests:
(begin-for-test
  (check-equal? (find-dir (list
                           (list 4 (list (list 3 6) "ne"))
                           (list 6 (list (list 3 4) "se"))
                           (list 6 (list (list 1 6) "nw"))
                           (list 8 (list (list 1 4) "sw"))) SE)
                (list 6 (list (list 3 4) "se")))
  (check-equal? (find-dir empty "ne") '()))
;; Design Strategy: Use template for LOLDPD on lst
(define (find-dir lst dir)
  (cond
    [(empty? lst) empty]
    [else (if (string=? (second (second (first lst))) dir)
              (first lst)
              (find-dir (rest lst) dir))]))

;; traverse-direction : Position LOM -> LOPD
;; GIVEN: a position and a list of moves
;; RETURNS: a list of positions and direction
;; Examples/Tests: See function find-list-dir
;; Design Strategy: Use HOF map on lst
(define (traverse-direction start-pos lst)
  (map
   ;; Position -> LOPD
   ;; RETURNS: a list of position where each position is appended with a direction
   (lambda (s) (list (return-pos-list start-pos s) (first s)))
   lst))

;; return-pos-list : Position Move -> Position
;; GIVEN: a position and a move
;; RETURNS: a new position of robot, travelled a step a head in the given direction
;; from the move
;; Examples/Tests: See function find-list-dir
;; Design Strategy: Cases on direction
(define (return-pos-list pos m)
  (cond
    [(string=? (first m) NE) (next-pos  pos OPERAND1 OPERAND1)]
    [(string=? (first m) NW) (next-pos  pos OPERAND2 OPERAND1)]
    [(string=? (first m) SE) (next-pos  pos OPERAND1 OPERAND2)]
    [(string=? (first m) SW) (next-pos  pos OPERAND2 OPERAND2)]))

;; next-pos : Position Operator Operator -> Position
;; GIVEN: a position with two operators
;; RETURNS: a position next to the given position
;; Examples/Tests: See function find-list-dir
;; Design Strategy: Combibe simpler functions
(define (next-pos pos op1 op2)
  (list (op1 (first pos) ONE)
        (op2 (second pos) ONE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find-blocks-for-target :
;;        Position Position LOP NonNegInt NonNegInt Position Direction -> Boolean
;; GIVEN: a position, final position, list of positions as blocks, a distance 
;; travelled by the robot distance, a distance limit which if acquired by robot,
;; final position is reachable, a starting position and a direction
;; WHERE:
;; 1. The position initially is the given as the final position, but at each
;; call chnages as the new position returned by function eval-plan
;; 2. Distance is the distance travelled by the robot to a new position from the
;; final position, taken as start position
;; 3. Direction is one which will be placed in the end of the list 
;; of directions to be traversed from a particular position, which is initially
;; given a random value
;; RETURNS: true if the final position is blocked and is not reachable by the robot
;; HAULTING MEASURE: distance 
;; TERMINATION ARGUMENT: if distance is greater than or equal to the distance limit,
;; function is terminated and a false is returned, otherwise at each recursive call, 
;; the distance of the robot from the final position, taken as start position, is
;; calculated
;; Design Strategy: General recursion on final-move
(define (find-blocks-for-target pos final-pos blocks dist dist-limit start-pos dir)
  (if (>= dist dist-limit)
      #f
      (final-move pos final-pos blocks dist-limit
                  (get-direction-list final-pos start-pos dir)
                  start-pos)))

;; final-move : Position Position LOP NonNegInt LOM Position -> Boolean
;; GIVEN: a position, final-position, list of positions as blocks, a distance
;; limit which if acquired by robot,final position is reachable,
;; a list of moves to be taken and a starting position
;; WHERE: The position initially is the given as the final position, but at each
;; call chnages as the new position returned by function eval-plan
;; RETURNS: true if the final position is blocked and is not reachable by the robot
;; HAULTING MEASURE: List of moves
;; TERMINATION ARGUMENT: the function terminates with a true, if the list of moves
;; is empty, else at each recursive call next move from the list of moves is examined 
;; Design Strategy: General recursion on may-be-blocked
(define (final-move pos final-pos blocks dist-limit lst start-pos)
  (cond
    [(empty? lst) #t]
    [else (if (boolean? (eval-plan pos blocks (list (first lst))))
              (final-move pos final-pos blocks dist-limit (rest lst) start-pos)
              (may-be-blocked pos final-pos blocks dist-limit lst start-pos))]))

;; may-be-blocked : Position Position LOP NonNegInt LOM Position -> Boolean
;; GIVEN: a position, final-position, list of positions as blocks, a distance
;; limit which if acquired by robot,final position is reachable,
;; a list of moves to be taken and a starting position
;; WHERE: The position initially is the given as the final position, but at each
;; call chnages as the new position returned by function eval-plan
;; RETURNS: true if the final position is blocked and is not reachable by the robot
;; Examples/Tests:
(begin-for-test
  (check-equal? (may-be-blocked (list 2 5) (list 2 5) wall1 0 (list (list "ne" 1))
                                (list 4 9))false))
;; Design Startegy: Cases on list of moves
(define (may-be-blocked pos final-pos blocks dist-limit lst start-pos)
  (if (empty? (rest lst))
      (find-blocks-for-target (eval-plan pos blocks (list (first lst)))
                              final-pos blocks
                              (calc-dist (eval-plan pos blocks (list (first lst))) final-pos)
                              dist-limit start-pos (opposite-direction (first (first lst))))    
      (find-blocks-for-target (eval-plan pos blocks (list (first lst)))
                              final-pos (cons pos blocks)
                              (calc-dist (eval-plan pos blocks (list (first lst))) final-pos)
                              dist-limit start-pos (opposite-direction (first (first lst))))))

;; calc-dist : Position Position -> NonNegInt
;; GIVEN: two positions
;; RETURNS: distance between current position and final position of the robot
;; Examples/Tests: See function may-be-blocked
;; Design Strategy: Combine simpler functions
(define (calc-dist pos1 pos2)
  (max (abs (- (first pos1) (first pos2)))
       (abs (- (second pos1) (second pos2)))))

