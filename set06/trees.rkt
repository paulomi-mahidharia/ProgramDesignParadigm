;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;PDP Problem set 06 : Question 01;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct world (lot))
;;A world is a
;;(make-world (ListOfTrees))
;;INTERPRETATION
;;ListOfTrees is the list of trees present in the given world

;;TEMPLATE:
;;world-fn : World -> ??
;;(define (world-fn w)
;  (... (world-lot w)))

(define-struct node (center mx my selected? sons))
;;A Node is a
;;(make-node (make-posn NonNegInt NonNegInt) NonNegInt NonNegInt Boolean Boolean)
;;INTERPRETATION:
;;center is (make-posn NonNegInt NonNegInt) that represents the center of a node
;;mx is the x-coordinate of the mouse 
;;my is the y-coordinate of the mouse
;;selected? describes whether the node is selected or not
;;sons is a ListOfTrees that represents the list of child nodes of the given node

;;DESTRUCTOR TEMPLATE:
;;node-fn : Node -> ??
;;(define (node-fn n)
;  (... (posn-x(node-center n))
;       (posn-y(node-center n))
;       (node-mx n)
;       (node-my n)
;       (node-selected? n)
;       (lot-fn (node-sons n))))

;;A Tree is a Node.

;;A ListOfTrees (LOT) is either
;;-- Node
;;-- (cons Node ListOfTrees)

;;TEMPLATE
;;lot-fn : ListOfTrees -> ??
;;(define (lot-fn lot)
;  (...
;;   (cond  
;;     [(empty? lot) ...]  
;;     [else (...  
;;             (node-fn (first lot))  
;;             (lot-fn (rest lot)))]))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define NODE-RADIUS 10)
(define THRICE-NODE-RADIUS (* NODE-RADIUS 3))
(define ZERO 0)
(define LINE-COLOR "blue")

(define LEVEL1-Y-COOR (+ NODE-RADIUS THRICE-NODE-RADIUS))
(define LEVEL2-Y-COOR (+ LEVEL1-Y-COOR THRICE-NODE-RADIUS))
(define LEVEL3-Y-COOR (+ LEVEL2-Y-COOR THRICE-NODE-RADIUS))

(define SEC-CHILD-X (+ CANVAS-WIDTH-HALF THRICE-NODE-RADIUS))
(define THIRD-CHILD-X (+ SEC-CHILD-X THRICE-NODE-RADIUS))

;;dimentions and look of a node
(define UNSEL-NODE (circle NODE-RADIUS "outline" "green"))
(define SEL-NODE (circle NODE-RADIUS "solid" "green"))
(define NEW-ROOT-AT-TOP (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                                   ZERO
                                   ZERO
                                   #false
                                   empty))

;;Trees for testing
(define SEL-NEW-ROOT-AT-TOP (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                                       ZERO
                                       ZERO
                                       #true
                                       empty))
(define NEW-ROOT-AT-TOP-FIRST-SON
  (make-node
   (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR)
   ZERO
   ZERO
   #false
   '()))
(define NEW-ROOT-AT-TOP-FIRST-SEL-SON
  (make-node
   (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR)
   ZERO
   ZERO
   #true
   '()))
(define NEW-ROOT-AT-TOP-WITH-ONE-SON
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
             ZERO
             ZERO
             #f
             (list NEW-ROOT-AT-TOP-FIRST-SON)))
(define SEL-NEW-ROOT-AT-TOP-WITH-ONE-SON
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
             ZERO
             ZERO
             #t
             (list NEW-ROOT-AT-TOP-FIRST-SON)))
(define NEW-ROOT-AT-TOP-WITH-ONE-SEL-SON
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
             ZERO
             ZERO
             #f
             (list NEW-ROOT-AT-TOP-FIRST-SEL-SON)))

(define TREE-WITH-CHILD-SEL
  (make-node
   (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false
   (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false
                    (list (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR) 0 0
                                     #true '())))
         (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 #false '()))))

(define NEW-CHILD-OF-TREE-WITH-CHILD-SEL
  (make-node (make-posn THIRD-CHILD-X LEVEL1-Y-COOR)
             (- (* 2 THRICE-NODE-RADIUS))
             (- THRICE-NODE-RADIUS) #false '()))

(define NEW-SEL-CHILD-OF-TREE-WITH-CHILD-SEL
  (make-node (make-posn THIRD-CHILD-X LEVEL1-Y-COOR)
             (- (* THRICE-NODE-RADIUS 2)) (- THRICE-NODE-RADIUS) #true '()))

(define TREE-WITH-CHILD-SEL-AFTER-CREATE-TREE
  (make-node
   (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false
   (list (make-node (make-posn THIRD-CHILD-X LEVEL1-Y-COOR)
                    (- (* THRICE-NODE-RADIUS 2))
                    (- THRICE-NODE-RADIUS)
                    #true
                    (list (make-node (make-posn THIRD-CHILD-X LEVEL2-Y-COOR)
                                     (- (* THRICE-NODE-RADIUS 2))
                                     (- (* THRICE-NODE-RADIUS 2))
                                     #false '()))))))

(define TREE-WITH-ROOT-CHILD-SEL
  (make-node
   (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #true
   (list (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 #false
                    (list (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR) 0 0 
                                     #false '())))
         (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #true
                    (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL2-Y-COOR) 0 0 
                                     #false '()))))))

(define TREE-WITH-CHILD-ON-LEFT-CANVAS
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false
             (list (make-node (make-posn 200 LEVEL1-Y-COOR) 0 0 #false
                              (list (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR)
                                               0 0 #true
                                               (list (make-node (make-posn
                                                                 SEC-CHILD-X
                                                                 LEVEL3-Y-COOR)
                                                                0 0 #false '())))))
                   (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '()))))

(define TREE-WITH-CHILD-SEL-AFTER-NEW-SON
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false
             (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false
                              (list (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR)
                                               0 0 #true
                                               (list (make-node
                                                      (make-posn SEC-CHILD-X
                                                                 LEVEL3-Y-COOR)
                                                      0 (- THRICE-NODE-RADIUS) #false '())))))
                   (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 #false '()))))

(define TREE-WITH-ROOT-CHILD-SEL-AFTER-NEW-SON
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #true
             (list (make-node (make-posn THIRD-CHILD-X LEVEL1-Y-COOR)
                              (- (* THRICE-NODE-RADIUS 2))
                              (- THRICE-NODE-RADIUS)
                              #false '())
                   (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 #false
                              (list (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR)
                                               0 0 #false '())))
                   (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #true
                              (list (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR)
                                               (- THRICE-NODE-RADIUS)
                                               (- THRICE-NODE-RADIUS)
                                               #false '())
                                    (make-node (make-posn CANVAS-WIDTH-HALF LEVEL2-Y-COOR)
                                               0 0 #false '()))))))

(define TREE-WITH-CHILD-SEL-AFTER-DEL
  (make-node
   (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false
   (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '())
         (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 #false '()))))

(define TREE-IN-LEFT-CANVAS-AFTER-DEL
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false
             (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '()))))



;;key events' constants
(define NEW-ROOT "t")
(define NEW-SON "n")
(define NODE-DEL "d")
(define DEL-LEFT-HALF "l")
(define INVALID-KEY "p")

;;mouse events' constants
(define BUTTON-DOWN "button-down")
(define DRAG "drag")
(define BUTTON-UP "button-up")
(define OTHER-EVENT "enter")

;; Contants for mouse events
(define WORLD-1
  (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '()))))

(define WORLD-2
  (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                               5 (- 15 NODE-RADIUS) true '()))))

(define WORLD-3 (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                                             5 5 #true '()))))

(define WORLD-4 (make-world (list (make-node (make-posn 295 35)
                                             5 5 #true '()))))

(define WORLD-5 (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                                             0 0 #false
                                             (list
                                              (make-node (make-posn SEC-CHILD-X
                                                                    LEVEL1-Y-COOR)
                                                         0 0 #false '())
                                              (make-node (make-posn CANVAS-WIDTH-HALF
                                                                    LEVEL1-Y-COOR)
                                                         0 0 #false '()))))))

(define WORLD-6 (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                                             5 5 #true
                                             (list
                                              (make-node (make-posn SEC-CHILD-X
                                                                    LEVEL1-Y-COOR)
                                                         -25 -25 #false '())
                                              (make-node (make-posn CANVAS-WIDTH-HALF
                                                                    LEVEL1-Y-COOR)
                                                         5 -25 #false '()))))))

(define POSN1 (make-posn CANVAS-WIDTH-HALF NODE-RADIUS))
(define NODE-SONS (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0  true '()))
(define NODE-SONS-RESPONSE (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 5 5 #true '()))
(define NODE-NOT-SELECTED (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 false '()))

(define NODE-SELECTED (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 50 50 true '()))
(define NODE-UNSELECTED (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 50 50 false '()))

(define NODE-SELECTED-AFTER-DRAG
  (make-node (make-posn CANVAS-WIDTH-HALF CANVAS-WIDTH-HALF) 50 50 true '()))
(define NODE-UNSELECTED-AFTER-DRAG
  (make-node (make-posn CANVAS-WIDTH-HALF CANVAS-WIDTH-HALF) 50 50 false '()))

(define NODES
  (list
   (make-node (make-posn SEC-CHILD-X LEVEL2-Y-COOR) 0 0 #false '())
   (make-node (make-posn CANVAS-WIDTH-HALF LEVEL2-Y-COOR) 0 0 #false '())))
(define NODES-AFTER-DRAG
  (list
   (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '())
   (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '())))

(define NODE1
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '()))
(define NODE2
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 5 5 true '()))
(define NODE3
  (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 5 5 #true '()))
(define NODE4
  (make-node (make-posn 295 35) 5 5 #true '()))
(define NODE5
  (make-node
   (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
   0
   0
   #false
   (list
    (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR) 0 0 #false '())
    (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '()))))
(define NODE6
  (make-node
   (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
   5
   5
   #true
   (list
    (make-node (make-posn SEC-CHILD-X LEVEL1-Y-COOR)
               (- THRICE-NODE-RADIUS) (- THRICE-NODE-RADIUS) #false '())
    (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR)
               0 (- THRICE-NODE-RADIUS) #false '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;initial-world : Any -> World
;;GIVEN: any value
;;RETURNS: an initial world.  The given value is ignored.
;;STRATEGY: Call simpler function
;;EXAMPLES:(initial-world 3)=(make-world '())

(define (initial-world n)
  (make-world 
   empty))

;;TESTS:
(begin-for-test
  (check-equal? (initial-world 3)
                (make-world '())
                "The world is initiated with an empty list of trees"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;run :  Any -> World
;;GIVEN: any value
;;EFFECT: runs a copy of an initial world
;;RETURNS: the final state of the world.  The given value is ignored.
;;STRATEGY:
;;EXAMPLES:

(define (run simulation-speed)
  (big-bang (initial-world 5)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-key-event : World KeyEvent -> World
;;GIVEN: a World and a key event
;;RETURNS: the state of the world as it should be following the given key event
;;STRATEGY: Dividing cases for KeyEvent on kev
;;EXAMPLES:
;;(world-after-key-event (initial-world 3) NEW-ROOT)
;; => (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '())))
;;(world-after-key-event (make-world (list TREE-WITH-CHILD-SEL TREE-WITH-ROOT-CHILD-SEL))
;;                        NEW-SON)
;; => (make-world (list TREE-WITH-CHILD-SEL-AFTER-NEW-SON
;;                      TREE-WITH-ROOT-CHILD-SEL-AFTER-NEW-SON))
;;(world-after-key-event (make-world (list TREE-WITH-ROOT-CHILD-SEL TREE-WITH-CHILD-SEL ))
;;                        NODE-DEL)
;; => (make-world (list TREE-WITH-CHILD-SEL-AFTER-DEL))
;;(world-after-key-event (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS)) DEL-LEFT-HALF)
;; => (make-world (list TREE-IN-LEFT-CANVAS-AFTER-DEL))
;;(world-after-key-event (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS)) INVALID-KEY)
;; => (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS))

(define (world-after-key-event w kev)
  (cond
    [(key=? kev NEW-ROOT) (world-after-new-root w)]
    [(key=? kev NEW-SON) (world-after-new-son w)]
    [(key=? kev NODE-DEL) (world-after-node-delete w)]
    [(key=? kev DEL-LEFT-HALF) (world-after-node-del-in-left-canvas w)]
    [else w]))

;;TESTS:
(begin-for-test
  (check equal? (world-after-key-event (initial-world 3) NEW-ROOT)
         (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '())))
         "A new node is made in the world at the top center")
  (check equal? (world-after-key-event (make-world (list
                                                    TREE-WITH-CHILD-SEL
                                                    TREE-WITH-ROOT-CHILD-SEL))
                                       NEW-SON)
         (make-world (list TREE-WITH-CHILD-SEL-AFTER-NEW-SON
                           TREE-WITH-ROOT-CHILD-SEL-AFTER-NEW-SON))
         "A new son is created for selected nodes and their selected descendants")
  (check equal? (world-after-key-event (make-world (list
                                                    TREE-WITH-ROOT-CHILD-SEL
                                                    TREE-WITH-CHILD-SEL ))
                                       NODE-DEL)
         (make-world (list TREE-WITH-CHILD-SEL-AFTER-DEL))
         "The selected node and all its descendants are deleted")
  (check equal? (world-after-key-event (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS))
                                       DEL-LEFT-HALF)
         (make-world (list TREE-IN-LEFT-CANVAS-AFTER-DEL))
         "All the nodes in left half of the canvas and their descendants are deleted")
  (check equal? (world-after-key-event (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS))
                                       INVALID-KEY)
         (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS))
         "The tree is returned as it is if an invald key is pressed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-new-root : World -> World
;;GIVEN: a world
;;RETURNS: world after a new root is created
;;STRATEGY: Use template for World on w
;;EXAMPLE:
;;(world-after-key-event (initial-world 3) NEW-ROOT)
;; => (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '())))

(define (world-after-new-root w)
  (make-world (cons NEW-ROOT-AT-TOP (world-lot w))))

;TEST:
(begin-for-test
  (check equal? (world-after-key-event (initial-world 3) NEW-ROOT)
         (make-world (list (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '())))
         "A new root node is made at (CANVAS-WIDTH-HALF,20)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-new-son : World -> World
;;GIVEN: a world
;;RETURNS: world after a new son is created for selected nodes
;;STRATEGY: Use template for World on w
;;EXAMPLE:
;;(world-after-new-son (make-world (list TREE-WITH-CHILD-SEL)))
;; => (make-world (list TREE-WITH-CHILD-SEL-AFTER-NEW-SON))

(define (world-after-new-son w)
  (make-world (tree-list-after-new-son (world-lot w))))

;TEST:
(begin-for-test
  (check equal? (world-after-new-son (make-world (list TREE-WITH-CHILD-SEL)))
         (make-world (list TREE-WITH-CHILD-SEL-AFTER-NEW-SON))
         "A new son is created for a selected node in the list of trees
         that belong to the given world"))
;...............................................................................
;;tree-list-after-new-son : ListOfTrees -> ListOfTrees
;;GIVEN: a list of trees
;;RETURNS: a list of trees after a new son is created for selected nodes
;;STRATEGY: Use HOF map on lot
;;EXAMPLE:
;;(tree-list-after-new-son (list TREE-WITH-CHILD-SEL TREE-WITH-ROOT-CHILD-SEL))
;; => (list TREE-WITH-CHILD-SEL-AFTER-NEW-SON TREE-WITH-ROOT-CHILD-SEL-AFTER-NEW-SON)

(define (tree-list-after-new-son lot) 
  (map node-after-new-son lot))

;;TEST:
(begin-for-test
  (check equal? (tree-list-after-new-son (list TREE-WITH-CHILD-SEL
                                               TREE-WITH-ROOT-CHILD-SEL))
         (list TREE-WITH-CHILD-SEL-AFTER-NEW-SON
               TREE-WITH-ROOT-CHILD-SEL-AFTER-NEW-SON)
         "A new son is created for a selected node in the given list of trees"))
;..............................................................................
;;node-after-new-son : Node -> Node
;;GIVEN: a node
;;RETURNS: a node with a new son created if selected 
;;STRATEGY: Use template for node on n
;;EXAMPLE:
;;(node-after-new-son TREE-WITH-CHILD-SEL)=TREE-WITH-CHILD-SEL-AFTER-NEW-SON

(define (node-after-new-son n)
  (if (node-selected? n)
      (create-tree-with-sons n (cons (make-son n) (node-sons n)))      
      (create-tree-with-sons n (node-sons n))))

;TEST:
(begin-for-test
  (check equal? (node-after-new-son TREE-WITH-CHILD-SEL)
         TREE-WITH-CHILD-SEL-AFTER-NEW-SON
         "A new son is created if the node or a node in its subtree is selected "))
;...............................................................................
;;create-tree-with-sons : Node ListOfTrees-> Node
;;GIVEN: a node and a list of trees
;;RETURNS: a node with the given list of trees appended as its descendents
;;         with sons created if any of them are selected
;;STRATEGY: Use template for node on n
;;EXAMPLE:
;;(create-tree-with-sons TREE-WITH-CHILD-SEL (list NEW-SEL-CHILD-OF-TREE-WITH-CHILD-SEL))
;; => TREE-WITH-CHILD-SEL-AFTER-CREATE-TREE

(define (create-tree-with-sons n lot) 
  (make-node (node-center n)
             (node-mx n)
             (node-my n)
             (node-selected? n)
             (tree-list-after-new-son lot)))
;TEST:
(begin-for-test
  (check-equal? (create-tree-with-sons TREE-WITH-CHILD-SEL
                                       (list NEW-SEL-CHILD-OF-TREE-WITH-CHILD-SEL))
                TREE-WITH-CHILD-SEL-AFTER-CREATE-TREE
                "the given list is appended to the given node with sons created for
                selected node"))
;..............................................................................
;;make-son : Node -> Node
;;GIVEN: a selected node
;;RETURNS: the new son node of the given node created beneath it or at a predefined offset
;;         to the rightmost son
;;STRATEGY: Use template for Node on n
;;EXAMPLE:
;;(make-son TREE-WITH-CHILD-SEL)=(make-node (make-posn THIRD-CHILD-X LEVEL1-Y-COOR) (- (* THRICE-NODE-RADIUS 2)) (- THRICE-NODE-RADIUS) #false '())

(define (make-son n)
  (make-node (make-posn (x-coordinate-of-new-son n)
                        (+ (posn-y(node-center n)) THRICE-NODE-RADIUS))
             (+ (node-mx n) (- (posn-x (node-center n)) (x-coordinate-of-new-son n))) 
             (+ (node-my n) (- THRICE-NODE-RADIUS))
             false
             empty))
;TESTS
(begin-for-test
  (check-equal? (make-son TREE-WITH-CHILD-SEL)
                NEW-CHILD-OF-TREE-WITH-CHILD-SEL
                "The new child node is created for the given node by a predifined offset
                 after its rightmost son"))
;..............................................................................
;;x-coordinate-of-new-son : Node -> NonNegInt
;;GIVEN: a selected node
;;RETURNS: the x-coordinate of the new son of the given node
;;STRATEGY: Use template for Node on n
;;EXAMPLE:
;;(x-coordinate-of-new-son TREE-WITH-CHILD-SEL)=THIRD-CHILD-X

(define (x-coordinate-of-new-son n)
  (if (empty? (node-sons n)) (posn-x(node-center n))
      (+ (get-max-x-coordinate-of-children n) THRICE-NODE-RADIUS)))

;TEST:
(begin-for-test
  (check-equal? (x-coordinate-of-new-son TREE-WITH-CHILD-SEL)
                THIRD-CHILD-X
                "The x-coordinate of the new child should be THIRD-CHILD-X"))
;..............................................................................
;;get-max-x-coordinate-of-children : Node -> NonNegInt
;;GIVEN: a node
;;RETURNS: the maximum value of x-coordinate of the child nodes of the given node
;;         i.e. the x-coordinate of the rightmost son of the given node
;;STRATEGY: Use template for Node on n
;;EXAMPLE:
;;(get-max-x-coordinate-of-children TREE-WITH-CHILD-SEL)=THIRD-CHILD-X

(define (get-max-x-coordinate-of-children n)
  (apply max (map
              ;;Node -> NonNegInt
              ;;GIVEN: a node
              ;;RETURNS: x-coordinate of the center of the node
              (lambda (son) (posn-x(node-center son)))
              (node-sons n))))
;;TEST:
(begin-for-test
  (check-equal? (get-max-x-coordinate-of-children TREE-WITH-CHILD-SEL)
                SEC-CHILD-X
                "The x-coordinate of the rightmost child is SEC-CHILD-X"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-node-delete : World -> World
;;GIVEN: a world
;;RETURNS: a world after the selected nodes and their subtrees are deleted
;;STRATEGY: Use template for World on w
;;EXAMPLE:
;;(world-after-node-delete (make-world (list TREE-WITH-ROOT-CHILD-SEL
;;                                         TREE-WITH-CHILD-SEL ))
;; => (make-world (list TREE-WITH-CHILD-SEL-AFTER-DEL))

(define (world-after-node-delete w)
  (make-world (tree-list-after-node-delete
               (world-lot w))))

;;TESTS:
(begin-for-test
  (check equal? (world-after-node-delete (make-world (list TREE-WITH-ROOT-CHILD-SEL
                                                           TREE-WITH-CHILD-SEL)))
         (make-world (list TREE-WITH-CHILD-SEL-AFTER-DEL))
         "The selected nodes and their subtrees are deleted"))
;.............................................................................
;;tree-list-after-node-delete : ListOfTrees -> ListOfTrees
;;GIVEN: a list of trees
;;RETURNS: a list of trees with the selected nodes and their subtrees deleted
;;STRATEGY: Use HOF map on lot
;;EXAMPLE:
;;(keep-only-undeleted-nodes (list TREE-WITH-ROOT-CHILD-SEL TREE-WITH-CHILD-SEL))
;; => (list TREE-WITH-CHILD-SEL-AFTER-DEL)

(define (tree-list-after-node-delete lot)
  (map tree-after-node-delete
       (filter
        ;;Node -> Boolean
        ;;GIVEN: a node
        ;;RETURNS: true if the node is not selected
        (lambda (t) (not (node-selected? t))) lot)))

;;TEST:
(begin-for-test
  (check equal? (tree-list-after-node-delete (list TREE-WITH-ROOT-CHILD-SEL
                                                   TREE-WITH-CHILD-SEL))
         (list TREE-WITH-CHILD-SEL-AFTER-DEL)
         "One tree is entirely deleted and for the other only the subtree
                 whose node is selected is deleted"))
;............................................................................
;;tree-after-node-delete : Node -> Node
;;GIVEN: an unselected node
;;RETURNS: the given node and its descendants added to it after deletion 
;;STRATEGY: Use template for Node on n
;;EXAMPLE:
;;(tree-after-node-delete TREE-WITH-CHILD-SEL empty)=TREE-WITH-CHILD-SEL-AFTER-DEL

(define (tree-after-node-delete n)
  (make-node
   (node-center n)
   (node-mx n)
   (node-my n)
   #f
   (tree-list-after-node-delete (node-sons n))))
;TEST:
(begin-for-test
  (check-equal? (tree-after-node-delete TREE-WITH-CHILD-SEL)
                TREE-WITH-CHILD-SEL-AFTER-DEL
                "List of trees with its selected children deleted "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-node-del-in-left-canvas: World -> World
;;GIVEN: a world
;;RETURNS:a world with all the nodes present in the left half of the canvas deleted along
;;        with their children
;;STRATEGY: Use template for World on w
;;EXAMPLE:
;;(world-after-node-del-in-left-canvas (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS)))
;; =>  (make-world (list TREE-IN-LEFT-CANVAS-AFTER-DEL))                                 

(define (world-after-node-del-in-left-canvas w)
  (make-world (list-tree-after-node-del-in-left
               (world-lot w))))
;TESTS:
(begin-for-test
  (check-equal? (world-after-node-del-in-left-canvas
                 (make-world (list TREE-WITH-CHILD-ON-LEFT-CANVAS)))
                (make-world (list TREE-IN-LEFT-CANVAS-AFTER-DEL))
                "All the nodes on the left side of canvas and their children are deleted"))
;....................................................................................
;;list-tree-after-node-del-in-left : ListOfTrees -> ListOfTrees
;;GIVEN: a list of trees
;;RETURNS: a list of trees with the nodes on the left side and their children deleted
;;STRATEGY: Use HOF map on lot
;;EXAMPLE:
;;(list-tree-after-node-del-in-left (list TREE-WITH-CHILD-ON-LEFT-CANVAS))
;; => (list TREE-IN-LEFT-CANVAS-AFTER-DEL)

(define (list-tree-after-node-del-in-left lot) 
  (map node-after-del-in-left
       (filter
        ;;Node -> Boolean
        ;;GIVEN: a node
        ;;RETURNS: true if the given node is not in the left half of canvas
        (lambda (t) (not (< (posn-x(node-center t)) CANVAS-WIDTH-HALF))) lot)))
;TESTS:
(begin-for-test
  (check equal? (list-tree-after-node-del-in-left (list TREE-WITH-CHILD-ON-LEFT-CANVAS))
         (list TREE-IN-LEFT-CANVAS-AFTER-DEL)
         "The node on the left side of the canvas is deleted along with its child
                 nodes"))
;....................................................................................
;;node-after-del-in-left : Node -> Node
;;GIVEN: a node to be preserved against deletion
;;RETURNS: the given node and all its sons to be preserved after deletion of the ones on
;;         the left side of the canvas.
;;STRATEGY: Use template for Node on n
;;EXAMPLE:
;;(node-after-del-in-left TREE-WITH-CHILD-ON-LEFT-CANVAS)=TREE-IN-LEFT-CANVAS-AFTER-DEL

(define (node-after-del-in-left n)
  (make-node
   (node-center n)
   (node-mx n)
   (node-my n)
   (node-selected? n)
   (list-tree-after-node-del-in-left (node-sons n))))

;TEST:
(begin-for-test
  (check-equal? (node-after-del-in-left TREE-WITH-CHILD-ON-LEFT-CANVAS)
                TREE-IN-LEFT-CANVAS-AFTER-DEL
                "constructs the node and its children that are to be preserved agaist
                 deletion"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given mouse
;; event at that location.
;; Examples:
;;(world-after-mouse-event WORLD-1 255 15 BUTTON-DOWN)=WORLD-2
;;(world-after-mouse-event WORLD-3 300 40 DRAG)=WORLD-4
;;(world-after-mouse-event WORLD-3 255 15 BUTTON-UP)=WORLD-1
;;(world-after-mouse-event WORLD-1 255 15 OTHER-EVENT)=WORLD-1
;;(world-after-mouse-event WORLD-1 400 400 BUTTON-DOWN)=WORLD-1
;;(world-after-mouse-event WORLD-5 255 15 BUTTON-DOWN)=WORLD-6

(define (world-after-mouse-event w mx my mev)
  (make-world
   (LOT-after-mouse-event (world-lot w) mx my mev)))

;Tests:
(begin-for-test
  (check-equal? (world-after-mouse-event WORLD-1 255 15 BUTTON-DOWN) WORLD-2
                "Must create a world like WORLD-2")
  (check-equal? (world-after-mouse-event WORLD-3 300 40 DRAG) WORLD-4
                "Must crreate a world like WORLD-4")
  (check-equal? (world-after-mouse-event WORLD-3 255 15 BUTTON-UP) WORLD-1
                "Must create a world like WORLD-1")
  (check-equal? (world-after-mouse-event WORLD-1 255 15 OTHER-EVENT) WORLD-1
                "Must create a world like WORLD-1")
  (check-equal? (world-after-mouse-event WORLD-1 400 400 BUTTON-DOWN) WORLD-1
                "Must create a world like WORLD-1")
  (check-equal? (world-after-mouse-event WORLD-5 255 15 BUTTON-DOWN) WORLD-6
                "Must create a world like WORLD-6"))
;.......................................................................
;; LOT-after-mouse-event : ListOfTrees NonNegInt NonNegInt MouseEvent -> ListOfTrees
;; GIVEN: a list of trees, x and y coordinates of mouse and the mouse event
;; RETURNS: a new list of trees following the mouse event
;; Examples:
;; (LOT-after-mouse-event (list NODE1) CANVAS-WIDTH-HALF NODE-RADIUS BUTON-DOWN)=(list NODE2)
;; Design Strategy: Use HOF map on lst
(define (LOT-after-mouse-event lst mx my mev)
  (map
   ;; Node -> Node
   ;; GIVEN: a Node
   ;; RETURNS: the Node that should follow the given Node after
   ;; the given mouse event 
   (lambda (n) (node-after-mouse-event n mx my mev))
   lst))

;........................................................................
;; node-after-mouse-event :  Node NonNegInt NonNegInt MouseEvent -> Node
;; GIVEN: a node, mouse coordinates and the mouse event
;; RETURNS: the node that should follow the given node after
;; the given mouse event
;; Examples:
;; (node-after-mouse-event NODE1 CANVAS-WIDTH-HALF NODE-RADIUS BUTTON-DOWN) NODE2)
;; (node-after-mouse-event NODE3 CANVAS-WIDTH-HALF NODE-RADIUS BUTTON-UP) NODE1)
;; (node-after-mouse-event NODE3 CANVAS-WIDTH-HALF NODE-RADIUS DRAG) NODE4)
;; (node-after-mouse-event NODE1 CANVAS-WIDTH-HALF NODE-RADIUS OTHER-EVENT) NODE1)
;; STRATEGY: Cases on mouse event

(define (node-after-mouse-event n mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (node-after-button-down n mx my)]
    [(mouse=? mev DRAG) (node-after-drag n mx my)]
    [(mouse=? mev BUTTON-UP) (node-after-button-up n mx my)]
    [else n]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-button-down : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and coordinates of the mouse
;; RETURNS: the node following a button-down at the given location
;; calculating the offset distance with respect to mouse position
;; STRATEGY:: Combine simpler functions
;; Examples:
;;(node-after-button-down NODE1 CANVAS-WIDTH-HALF NODE-RADIUS)=NODE2

(define (node-after-button-down n mx my)
  (if
   (in-node? n mx my)
   (node-selected-cal-offset n mx my) 
   (node-unselected-cal-offset n mx my)))

;.........................................................................
;; node-selected-cal-offset : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and mouse coordinates
;; RETURNS: same node with offset calculated when node is selected
;; Strategy: Use template for Node on n
;; Examples: (node-selected-cal-offset NODE1 255 15)=NODE2

(define (node-selected-cal-offset n mx my)
  (make-node (node-center n) (- mx (posn-x (node-center n)))
             (- my (posn-y (node-center n))) 
             #true   (LOT-after-mouse-event-button-down
                      (node-sons n) mx my (node-center n)
                      (- mx (posn-x (node-center n)))
                      (- my (posn-y (node-center n))) 
                      )))


;........................................................................
;; node-unselected-cal-offset : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and mouse coordinates
;; RETURNS: same node with offset calculated when node is not selected
;; STRATEGY: Use template for Node on n
;; Examples: (node-unselected-cal-offset NODE1 255 15)=NODE1

(define (node-unselected-cal-offset n mx my)
  (make-node (node-center n)
             ZERO
             ZERO
             #false
             (LOT-after-mouse-event (node-sons n) mx my BUTTON-DOWN)))
;...........................................................................
;; LOT-after-mouse-event-button-down :
;; ListOfTrees NonNegInt NonNegInt Posn Integer Integer -> ListOfTrees
;; GIVEN: a list of trees as sons, mouse coordinates, center coordinates of parent 
;; which is selected, offset values of parent which is selected
;; RETURNS: ListOfTrees with offset value calculated for each son in the tree
;; STRATEGY: Use HOF map on sons
;; Examples:
;;(LOT-after-mouse-event-button-down (list NODE1) CANVAS-WIDTH-HALF NODE-RADIUS)
;; (list NODE2)

(define (LOT-after-mouse-event-button-down sons mx my c offset-x offset-y)
  (map
   ;; NODE -> NODE
   ;; GIVEN: a node
   ;; RETURNS: same node with offset distance calculated
   (lambda (son) (node-after-button-down-sons son mx my c offset-x offset-y))
   sons))
;...........................................................................
;; node-after-button-down-sons : Node NonNegInt NonNegInt
;;                               Posn Integer
;;                               Integer -> Node
;; GIVEN: a node, mouse coordinates, center coordinates of parent which is selected,
;; offset values of parent which is selected
;; RETURNS: a node with its offset value calculated
;; STRATEGY:: Combine simpler functions 
;; Examples: (node-after-button-down-sons NODE-SONS 285 45 POSN1 5 5)
;;           => NODE-SONS-RESPONSE

(define (node-after-button-down-sons n mx my c offset-x offset-y)
  (if
   (in-node? n mx my)
   (mouse-in-cal-offset n mx my c)
   (mouse-out-cal-offset n mx my c offset-x offset-y)))

;TESTS:
(begin-for-test
  (check-equal? (node-after-button-down-sons NODE-SONS 285 45 POSN1 5 5)
                NODE-SONS-RESPONSE
                "Must create a node as NODE-SONS-RESPONSE"))
;............................................................................
;; mouse-in-cal-offset : Node NonNegInt NonNegInt
;;                      (make-posn NonNegInt NonNegInt) -> Node
;; GIVEN: a node, mouse coordinates, center coordinates of parents which is selected
;; RETURNS: same node with offset value calculated when mouse placed inside the
;; node
;; STRATEGY:: Use template for Node on n
;; Examples/Tests: See function node-after-button-down-sons

(define (mouse-in-cal-offset n mx my c)
  (make-node (node-center n)
             (- mx (posn-x (node-center n)))
             (- my (posn-y (node-center n)))
             #true
             (LOT-after-mouse-event-button-down
                    (node-sons n) mx my c
                    (- mx (posn-x (node-center n)))
                    (- my (posn-y (node-center n))))))
;...........................................................................
;; mouse-out-cal-offset : Node NonNegInt NonNegInt
;;                        Posn Integer Integer -> Node
;; GIVEN: a node, mouse coordinates, center coordinates of parents which is selected,
;; offset values of parent which is selected
;; RETURNS: same node with offset value calculated when mouse placed inside the
;; node
;; STRATEGY:: Use template for Node on n
;; Examples/Tests: See function node-after-button-down-sons

(define (mouse-out-cal-offset n mx my c offset-x offset-y)
  (make-node (node-center n)
             (+ offset-x (- (posn-x c) (posn-x (node-center n))))
             (+ offset-y (- (posn-y c) (posn-y (node-center n)))) 
             #false
             (LOT-after-mouse-event-button-down
                     (node-sons n) mx my c
                     offset-x offset-y)))
;............................................................................
;; node-after-drag : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and mouse coordinates
;; RETURNS: node at a new location, following a drag
;; STRATEGY: Use template for Node on n
;; Examples:
;; (node-after-drag NODE-NOT-SELECTED 300 300)=NODE-NOT-SELECTED

(define (node-after-drag n mx my)
  (if
   (node-selected? n)
   (node-selected-after-drag n mx my)
   (node-unselected-after-drag n mx my)))
;TEST:
(begin-for-test
  (check-equal? (node-after-drag NODE-NOT-SELECTED 300 300) NODE-NOT-SELECTED
                "Must return same node when not selected"))

;...........................................................................
;; node-selected-after-drag : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node, mouse coordinates
;; RETURNS: same node with new coordinates after drag, following
;; mouse coordinates, when the node is selected
;; STRATEGY:: Use template for Node on n
;; Examples/Tests: See function node-after-drag

(define (node-selected-after-drag n mx my)
  (make-node (make-posn (- mx (node-mx n)) (- my (node-my n))) (node-mx n)
             (node-my n)
             true
             (LOT-after-mouse-event-drag (node-sons n) mx my (node-center n))))

;............................................................................
;; node-unselected-after-drag : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node, mouse coordinates
;; RETURNS: same node with new coordinates after drag, following
;; mouse coordinates, when the node is unselected
;; STRATEGY:: Use template for Node on n
;; Examples/Tests: See function node-after-drag

(define (node-unselected-after-drag n mx my)
  (make-node (node-center n)
             (node-mx n)
             (node-my n)
             false
             (LOT-after-mouse-event (node-sons n) mx my DRAG)))
;.............................................................................
;; LOT-after-mouse-event-drag : ListOfTrees NonNegInt NonNegInt
;;                              Posn-> ListOfTrees
;; GIVEN: a list of trees as sons, mouse coordinates,center coordinates of parent
;;        which is selected
;; RETURNS: List of trees with new coordinates of all nodes in trees, following
;;          the mouse coordinates
;; STRATEGY:: Use HOF map on sons
;; Examples:
;;(LOT-after-mouse-event-drag NODES CANVAS-WIDTH-HALF LEVEL1-Y-COOR POSN1)=NODES-AFTER-DRAG

(define (LOT-after-mouse-event-drag sons mx my c)
  (map
   ;; NODE -> NODE
   ;; GIVEN: a node
   ;; RETURNS: a node with new coordinates
   (lambda (son) (node-after-drag-sons son mx my c))
   sons))

;TESTS:
(begin-for-test
  (check-equal? (LOT-after-mouse-event-drag NODES CANVAS-WIDTH-HALF LEVEL1-Y-COOR POSN1)
                NODES-AFTER-DRAG
                "Must create list of nodes as NODES-AFTER-DRAG"))
;............................................................................
;; node-after-drag-sons : Node NonNegInt NonNegInt
;;                        Posn -> Node
;; GIVEN: a node, mouse coordinates, center coordinates of parent which is selected
;; RETURNS: node at new location following drag
;; STRATEGY:: Combine simpler functions
;; Examples:
;;(node-after-drag-sons NODE-SELECTED 300 300 POSN1)=NODE-SELECTED-AFTER-DRAG
;;(node-after-drag-sons NODE-UNSELECTED 300 300 POSN1)=NODE-UNSELECTED-AFTER-DRAG

(define (node-after-drag-sons n mx my c)
  (if
   (node-selected? n)
   (drag-node-after-selected n mx my c)
   (drag-node-after-unselected n mx my c)))

;TESTS:
(begin-for-test
  (check-equal? (node-after-drag-sons NODE-SELECTED 300 300 POSN1)
                NODE-SELECTED-AFTER-DRAG
                "Must return a new node with new coordinates considering offset")
  (check-equal? (node-after-drag-sons NODE-UNSELECTED 300 300 POSN1)
                NODE-UNSELECTED-AFTER-DRAG
                "Must return a new node with new coordinates considering offset"))

;...........................................................................
;; drag-node-after-selected : Node NonNegInt NonNegInt
;;                            Posn -> Node
;; GIVEN: a node, mouse coordinates, center coordinates of parents which is selected
;; RETURNS: same node with new coordinates after drag, following
;; mouse coordinates, when the node is selected
;; STRATEGY: Use template for Node on n
;; Examples/Tests: See function node-after-drag-sons

(define (drag-node-after-selected n mx my c)
  (make-node (make-posn (- mx (node-mx n)) (- my (node-my n)))
             (node-mx n)
             (node-my n)
             true
             (LOT-after-mouse-event-drag (node-sons n) mx my (node-center n))))

;............................................................................
;; drag-node-after-unselected : Node NonNegInt NonNegInt
;;                              Posn -> Node
;; GIVEN: a node, mouse coordinates, center coordinates of parents which is selected
;; RETURNS: same node with new coordinates after drag, following
;;          mouse coordinates, when the node is unselected
;; STRATEGY: Use template for Node on n
;; Examples/Tests: See function node-after-drag-sons

(define (drag-node-after-unselected n mx my c)
  (make-node (make-posn (- mx (node-mx n)) (- my (node-my n)))
             (node-mx n)
             (node-my n)
             false
             (LOT-after-mouse-event-drag (node-sons n) mx my (node-center n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-button-up: Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and coordinates of the mouse
;; RETURNS: a node following the button-up, at given location
;; STRATEGY: Use template for Node on n
;; Examples: (node-after-button-up NODE3 CANVAS-WIDTH-HALF NODE-RADIUS)=NODE1

(define (node-after-button-up n mx my)
  (make-node (node-center n) ZERO ZERO 
             #false
             (LOT-after-mouse-event (node-sons n)  mx my BUTTON-UP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in-node? : Node NonNegInt NonNegInt -> Boolean
;; GIVEN: a node and coordinates of mouse
;; RETURNS: true iff the coordinates of mouse is within the boundaries node
;; STRATEGY: Use template for Node on n
;; Examples:
;;(in-node? NEW-ROOT-AT-TOP-FIRST-SON CANVAS-WIDTH-HALF LEVEL1-Y-COOR)=#true
;;(in-node? NEW-ROOT-AT-TOP-FIRST-SON (+ CANVAS-WIDTH-HALF NODE-RADIUS NODE-RADIUS)
;;                                     LEVEL1-Y-COOR)=#false

(define (in-node? n mx my)
  (<=  (sqrt
        (+ (sqr (- mx (posn-x (node-center n))))
           (sqr (- my (posn-y (node-center n))))))
       NODE-RADIUS))

;Tests:
(begin-for-test
  (check-equal? (in-node? NEW-ROOT-AT-TOP-FIRST-SON CANVAS-WIDTH-HALF LEVEL1-Y-COOR) #true)
  (check-equal? (in-node? NEW-ROOT-AT-TOP-FIRST-SON
                          (+ CANVAS-WIDTH-HALF NODE-RADIUS NODE-RADIUS)
                          LEVEL1-Y-COOR) #false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-to-trees : World -> ListOfTree
;;GIVEN: a World
;;RETURNS: a list of all the trees in the given world.
;;STRATEGY: Use template of World on w
;;EXAMPLES:
;;(world-to-trees (make-world (list TREE-WITH-CHILD-SEL TREE-WITH-ROOT-CHILD-SEL)))
;; => (list TREE-WITH-CHILD-SEL TREE-WITH-ROOT-CHILD-SEL)

(define (world-to-trees w)
  (world-lot w))

;;TESTS:
(begin-for-test
  (check equal? (world-to-trees (make-world (list TREE-WITH-CHILD-SEL
                                                  TREE-WITH-ROOT-CHILD-SEL)))
         (list TREE-WITH-CHILD-SEL TREE-WITH-ROOT-CHILD-SEL)
         "The list of trees for the given world is returned"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tree-to-root : Tree -> Node
;;GIVEN: a tree
;;RETURNS: the node at the root of the tree
;;STRATEGY: Use template for Node on n
;;EXAMPLES:
;;(tree-to-root TREE-WITH-CHILD-SEL)=(make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '())

(define (tree-to-root t)
  (make-node (node-center t) (node-mx t) (node-my t) (node-selected? t) empty))

;;TESTS:
(begin-for-test
  (check-equal? (tree-to-root TREE-WITH-CHILD-SEL)
                (make-node (make-posn CANVAS-WIDTH-HALF NODE-RADIUS) 0 0 #false '())
                "The root node of the three is returned"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tree-to-sons : Tree -> ListOfTree
;;GIVEN: a tree
;;RETURNS: the sons of the tree
;;STRATEGY: Use template for Node on n
;;EXAMPLES:
;;(tree-to-sons NEW-ROOT-AT-TOP-WITH-ONE-SON)
;; => (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '()))
(define (tree-to-sons t)
  (node-sons t))

;;TESTS:
(begin-for-test
  (check equal? (tree-to-sons NEW-ROOT-AT-TOP-WITH-ONE-SON)
         (list (make-node (make-posn CANVAS-WIDTH-HALF LEVEL1-Y-COOR) 0 0 #false '()))
         "The sons of the given tree is returned"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;node-to-center : Node -> Posn
;;RETURNS: the center of the given node as it is to be displayed on the
;;scene.
;;Note: this function returns a Posn (an ISL builtin).  This is for the
;;convenience of the testing framework, and you may or may not wish to
;;represent the center of the node in this way.
;;STRATEGY: Use template for Node on n
;;EXAMPLES:
;;(node-to-center NEW-ROOT-AT-TOP)=(make-posn CANVAS-WIDTH-HALF NODE-RADIUS)

(define (node-to-center n)
  (node-center n))

;;TESTS:
(begin-for-test
  (check-equal? (node-to-center NEW-ROOT-AT-TOP)
                (make-posn CANVAS-WIDTH-HALF NODE-RADIUS)
                "The center of the new root is (CANVAS-WIDTH-HALF,NODE-RADIUS)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;node-to-selected? : Node -> Boolean
;;GIVEN: a node
;;RETURNS: true iff the given node is selected.
;;STRATEGY: Use template for Node on node
;;EXAMPLES:
;;(node-to-selected? NEW-ROOT-AT-TOP)=#false
;;(node-to-selected? SEL-NEW-ROOT-AT-TOP)=#true
(define (node-to-selected? n)
  (node-selected? n))

;;TESTS:
(begin-for-test
  (check-equal? (node-to-selected? NEW-ROOT-AT-TOP)
                #false
                "The given node is not selected")
  (check-equal? (node-to-selected? SEL-NEW-ROOT-AT-TOP)
                #true
                "The given node is selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-to-scene : World -> Scene
;; GIVEN: a world state
;; RETURNS: tranlation of the given world into a scene
;; STRATEGY: Use HOF foldr on lot
;; EXAMPLES:
(define (world-to-scene w)
  (foldr
   place-node
   EMPTY-CANVAS
   (world-lot w)))

;TEST:
(begin-for-test
  (check-equal? (world-to-scene (make-world (list NEW-ROOT-AT-TOP-WITH-ONE-SON)))
                (place-image UNSEL-NODE
                             CANVAS-WIDTH-HALF
                             NODE-RADIUS
                             (scene+line (place-image UNSEL-NODE
                                                      CANVAS-WIDTH-HALF
                                                      (+ NODE-RADIUS THRICE-NODE-RADIUS)
                                                      EMPTY-CANVAS)
                                         CANVAS-WIDTH-HALF
                                         NODE-RADIUS
                                         CANVAS-WIDTH-HALF
                                         (+ NODE-RADIUS THRICE-NODE-RADIUS)
                                         LINE-COLOR))
                "Places an unselected child node under an unselected parent node")
  (check-equal? (world-to-scene (make-world (list SEL-NEW-ROOT-AT-TOP-WITH-ONE-SON)))
                (place-image SEL-NODE
                             CANVAS-WIDTH-HALF
                             NODE-RADIUS
                             (scene+line (place-image UNSEL-NODE
                                                      CANVAS-WIDTH-HALF
                                                      (+ NODE-RADIUS THRICE-NODE-RADIUS)
                                                      EMPTY-CANVAS)
                                         CANVAS-WIDTH-HALF
                                         NODE-RADIUS
                                         CANVAS-WIDTH-HALF
                                         (+ NODE-RADIUS THRICE-NODE-RADIUS)
                                         LINE-COLOR))
                "Places an unselected child node under an selected parent node")
  (check-equal? (world-to-scene (make-world (list NEW-ROOT-AT-TOP-WITH-ONE-SEL-SON)))
                (place-image UNSEL-NODE
                             CANVAS-WIDTH-HALF
                             NODE-RADIUS
                             (scene+line (place-image SEL-NODE
                                                      CANVAS-WIDTH-HALF
                                                      (+ NODE-RADIUS THRICE-NODE-RADIUS)
                                                      EMPTY-CANVAS)
                                         CANVAS-WIDTH-HALF
                                         NODE-RADIUS
                                         CANVAS-WIDTH-HALF
                                         (+ NODE-RADIUS THRICE-NODE-RADIUS)
                                         LINE-COLOR))
                "Places an selected child node under an unselected parent node")
  (check-equal? (world-to-scene (make-world (list empty)))
                EMPTY-CANVAS
                "Places a selected node at the top of the canvas"))
;...............................................................................
;; place-node : Node Scene -> Scene
;; GIVEN: a node and a scene
;; RETURNS: a scene similar to given one but with the given node placed in it
;; STRATEGY: Use template for Node on n
(define (place-node n s)
  (if (empty? n)
      s
      (place-image (check-node-type n)
                   (posn-x(node-center n))
                   (posn-y(node-center n))
                   (place-sons (node-center n) (node-sons n) s))))

;................................................................................
;; place-sons : Posn ListOfTrees Scene -> Scene
;; GIVEN: center of a node, a list of trees as its descendants and a scene
;; RETURNS: a scene similar to given one but with the given descendants placed on it
;;          with a line connecting them to the given center of their parent node
;; STRATEGY: Use HOF foldr on lot
(define (place-sons n-center lot s)
  (foldr
   (lambda (n i) (scene+line (place-node n i)
                             (posn-x n-center)
                             (posn-y n-center)
                             (posn-x(node-center n))
                             (posn-y(node-center n))
                             LINE-COLOR))
   s
   lot))
;...............................................................................
;; check-node-type : Node -> Image
;; GIVEN: a node
;; RETURNS: the corresponding image equivalent of whether the node is selected or not
;; STRATEGY: Use template for Node on n
(define (check-node-type n)
  (if (node-selected? n)
      SEL-NODE
      UNSEL-NODE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


