#lang racket

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide    
 make-block
 Block<%> )

;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 10 : QUESTION 02;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEM STATEMENT :
; The toy has the following specification:
; The toy consists of a canvas that is 600 pixels high and 500 pixels wide.
; When the child types "b", a new block pops up on the screen at the location
; of the last button-down or button-up.
; The block appears as a 20x20 outline square.
; The square is initially green. If the child types a "b" before the
; first button-down or button-up event, then the first block appears in a fixed
; place on the canvas. 
; A block does not move by itself, but the child can move it around using
; Smooth Drag. When the block is selected, it appears as red rather than green.
; If a block is dragged so that it contacts or overlaps another block,
; the two blocks become connected.The property of being a teammate is symmetric
; and transitive.
; So if block A is moved to touch block B, then a new team is formed consisting
; of A and all its teammates, and B and all its teammates.
; Two blocks overlap if they intersect at any point.
; For this purpose, the edges of the block are considered part of the block.
; Once two blocks become teammates, they remain teammates forever.
; When a block is moved, all its teammates move along with it.
; If A and B are teammates, and A is dragged in some direction, then B moves
; the same way. Only the selected block accumulates teammates.
; If A is being dragged, and B is a teammate of A, and A's motion causes B to
; come into contact with C, C does not become a teammate of A and B. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; GLOBAL CONSTANTS :

;;Dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; Constants for the block
(define BLOCK-SIDE-LENGTH 20)
(define HALF-BLOCK-SIDE-LENGTH (/ BLOCK-SIDE-LENGTH 2))
(define BLOCK-STYLE "outline")

;; Constants for color of blocks
(define BLOCK-SELECTED "red")
(define BLOCK-UNSELECTED "green")

;;Constants for KeyEvents
(define ADD-NEW-BLOCK-KEYEVENT "b")
(define NOT-VALID-KEYEVENT "n")

;;Constants for MouseEvents
(define MOUSE-BUTTON-DOWN "button-down")
(define MOUSE-BUTTON-UP "button-up")
(define MOUSE-DRAG "drag")
(define INVALID-MOUSE-EVENT "enter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Block<%> interface :

; Block<%> implements all the functions of SWidget<%> from WidgetWorks
; A Block reprsents a stateful widget in the world.

(define Block<%>
  (interface(SWidget<%>) 
    
    ; x-coordinate of the center of the square representing the Block
    block-x
    
    ; y-coordinate of the center of the square representing the Block
    block-y
    
    ; -> ListOfBlocks
    ; GIVEN : no arguments 
    ; RETURNS : the teammates of the given Block
    get-team
    
    ; -> Void
    ; GIVEN : no arguments
    ; EFFECT : adds a new Block to the list of teammate of the block
    add-teammate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A ListOfBlock<%> (LOB) is a list of Block<%>
; i.e a list of blocks that implement the Block<%>
; interface.

; ListOfBlock<%> can be :
; -- empty
; -- (cons Block<%> ListOfBlock<%>)

; INTERPRETATION :
; --empty                          : an empty list
; --(cons Block<%> ListOfBlock<%>) : a list where the first element is a block
;                                    and the rest is LOB

; TEMPLATE :
; lob-fn : LOB -> ??
#;(define (lob-fn l)
    (cond
      [(empty? l)...]
      [(else
        ...(first l)
        (lob-fn(rest l)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CLASS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Block is a (new Block% [w-mx NonNegInt]       
;                          [w-my NonNegInt]
;                          [x Int]
;                          [y Int]
;                          [selected? Boolean]
;                          [blocks ListOfBlock]    
;                          [teammates ListOfBlock]
;                          [mx-offset Int]           
;                          [my-offset Int])         
; A Block is a stateful Widget                           
; It responds to MouseEvents, it can be seleceted, unselected or dragged 
; using mouse events
; If two blocks intersect each other then they become teammates ie, they stick
; to each other forever
; the keyevent ADD-NEW-BLOCK-KEYEVENT adds a new block to the place holder block
; It implements the Block<%> interface
; The first Block added acts as a placeholder for all blocks 

(define Block%
  (class* object% (Block<%>)
    
    ; Fields for Block
    ; ----------------
    
    (init-field w-mx w-my)
    (init-field x y selected? blocks teammates)
    (init-field mx-offset my-offset) 
    
    ; INTERPRETATION :
    ; w-mx       : the x coordinate of mouse click
    ; w-my       : the y coordinate of mouse click
    ; x          : the x coordinate of block's center
    ; y          : the y coordinate of block's center
    ; selected?  : determines whether the block is seleceted or not
    ; blocks     : list of all blocks which are part of the place holder block
    ; teammates  : list of blocks which are teammates to a block.
    ;              Being teammates suggest that all these blocks will
    ;              stick to each other
    ; mx-offset  : the relative difference between the x coordinate of block's   
    ;              center and the mouse click
    ; my-offset  : the relative difference between the y coordinate of block's   
    ;              center and the mouse click
    
    (super-new)
    
    ; Methods of Block
    ; ----------------
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN      : a scene
    ; RETURNS    : a scene similar to the given scene but with the ListOfBlock
    ;              painted on it
    ; STRATEGY   : Use HOF flodr on the ListOfBlock
    
    (define/public (add-to-scene scene)
      (foldr
       ; Scene   -> Scene
       ; GIVEN    : a Scene
       ; RETURNS  : a Scene similar to the given scene but with the given block
       ;            painted on it
       (lambda (block scene1) (draw-the-image block scene1))
       scene
       blocks))
    
    ;......................................................................
    
    ; draw-the-image : Block<%> Scene -> Scene
    ; GIVEN    : an object of a class that implements the Block<%> interface
    ;            and a scene
    ; RETURNS  : a scene similar to the given scene but with the given block
    ;            painted on it
    ; STRATEGY : Combining simple functions
    
    (define/public (draw-the-image block scene)
      (local
        ((define color (if
                        (send block is-selected?)
                        BLOCK-SELECTED BLOCK-UNSELECTED)))
        (place-image
         (square BLOCK-SIDE-LENGTH BLOCK-STYLE color)
         (send block block-x)
         (send block block-y)
         scene)))
    
    ;......................................................................
    
    ; after-tick  : -> Void
    ; GIVEN       : no argument
    ; EFFECT      : no effect

    (define/public (after-tick) this)
    
    ;......................................................................
    
    ; after-key-event : -> Void
    ; GIVEN           : no argument
    ; EFFECT          : Creates a new block after ADD-NEW-BLOCK-KEYEVENT and
    ;                   it also updates th ListOfBlock for every other block to
    ;                   reflect this change
    ; STRATEGY        : Cases on KeyEvent
    
    (define/public (after-key-event kev)
      (cond 
        [(key=? kev ADD-NEW-BLOCK-KEYEVENT)
         (begin
           (set! blocks (cons (make-block w-mx w-my blocks) blocks))
           (for-each
            ; Block<%> -> Void
            ; GIVEN     : an object of a class that implements the Block<%>
            ;             interface
            ; EFFECT    : updates the given object after the KeyEvent                      
            (lambda (block) (send block update-listofblock-for-all-blocks blocks))        
            blocks))]               
        [else this]))
    
    ;......................................................................
    
    ; update-listofblock-for-all-blocks : ListOfBlock -> Void
    ; GIVEN     : a ListOfBlock
    ; EFFECT    : Updates the blocks field for every block after the
    ;             keyevent
    ; STRATEGY  : Combining simple functions
    
    (define/public (update-listofblock-for-all-blocks all-blocks)  
      (set! blocks (set-minus all-blocks this)))       
    
    ;........................................................... ........... 
    
    ; after-drag  : NonNegInt NonNegInt -> Void
    ; GIVEN       : the x and y coordinates of the mouse event
    ; EFFECT      : a block is updated for the following if it is selected :        
    ;               1. Updates the center coordinates of the block according
    ;                  to the coordinates of mouse click
    ;               2. Updates the center of the teammates of this selected
    ;                  block, according to the distance moved by this block
    ;               3. Checks for intersection with any block while dragging,
    ;                  and if so, then adds the intersected block to the list
    ;                  of teammates
    ;               4. Updates the list of teammates for all the blocks who are
    ;                  teamamtes for this block  
    ; STRATEGY    : Combining simpler functions
    
    (define/public (after-drag mx my)
      (for-each
       ; Block<%> -> Void
       ; GIVEN     : an object of a class that implements the Block<%> interface
       ; EFFECT    : updates the given object after the mouse drag event                       
       (lambda (block) (send block drag-the-block mx my))
       (cons this blocks)))
    
    ;......................................................................
    
    ; drag-the-block : NonNegInt NonNegInt -> Void
    ; GIVEN          :  the x and y coordinates of the mouse event
    ; EFFECT         : a block is updated for the following if it is selected :
    ;                   1. Updates the center cooordiantes of the block
    ;                      according to the coordiantes of mouse click
    ;                   2. Updates the center of the teammates of this selected
    ;                      block, according to the distance moved by this block
    ;                   3. Checks for intersection with any block while dragging,
    ;                      and if so, then adds the intersected block to the
    ;                      list of teammates
    ;                   4. Updates the list of teammates for all the blocks who
    ;                      are teamamtes for this block   
    ;STRATEGY         : Combinig simpler functions
    
    (define/public (drag-the-block mx my) 
      (if selected?
          (local
            ((define last-x x)
             (define last-y y)
             (define prev-mates teammates))
            (begin
              (set! x (- mx mx-offset)) 
              (set! y (- my my-offset))
              (change-mates-coordinate (- x last-x) (- y last-y))
              (set! teammates
                    (append teammates                           
                            (add-intersected-blocks prev-mates)))
              (change-all-block-teammates)))
          this))                                
    
    ;.....................................................................
    
    ; change-mates-coordinate : Integer Integer -> Void 
    ; GIVEN    : 1. the difference in the current x coordinate and previous x
    ;            coordiante of the selected block's center
    ;            2. the difference in the current y coordinate and previous y
    ;            coordiante of the selected block's center
    ; EFFECT   : changes the center coordinates of all the blocks that are
    ;            teammates of the selected block to satisfy the mouse drag
    ;            functionality
    ; STRATEGY : Use HOF for-each on teammates
    
    (define (change-mates-coordinate offset-x offset-y)   
      (for-each
       ; Block<%> -> void
       ; GIVEN   : an object of a class that implements the Block<%> interface
       ; EFFECT  : updates the given object with its center updated
       (lambda (block) (send block change-block-coordinate offset-x offset-y))
       teammates))
    
    ;......................................................................
    
    ; change-block-coordinate : Integer Integer -> Void
    ; GIVEN    : 1. the difference in the current x coordinate and previous x
    ;               coordiante of the selected block's center
    ;            2. the difference in the current y coordinate and previous y
    ;               coordiante of the selected block's center
    ; EFFECT   : Updates the center of the block(teammate of selected block)
    ;            to satisfy the mouse drag function
    ; STRATEGY : Combining simpler fucntions
    
    (define/public (change-block-coordinate offset-x offset-y) 
      (begin
        (set! x (+ x offset-x))
        (set! y (+ y offset-y))))
    
    ;......................................................................
    
    ; add-intersected-blocks : ListOfBlock -> ListOfBlock
    ; GIVEN     : a ListOfBlock which are already teammates of the selected
    ;             block
    ; RETURNS   : an updated ListOfBlock consisting of teammates of the
    ;             selected block (if any block, excluding the old teammates
    ;             was intersected, it is now a part of the selected block's
    ;             teammate    
    ; STRATEGY  : Combining simple function
    
    (define (add-intersected-blocks prev-mates)   
      (mates-of-new-intersected-blocks (intersecting-blocks prev-mates)))  
    
    ;......................................................................
    
    ; intersecting-blocks : ListOfBlock -> ListOfBlock
    ; GIVEN      : a ListOfBlock which are already teammates of the selected
    ;              block
    ; RETURNS    : a ListOfBlock which intersected with the selected block
    ;              (excluding its old teammates)
    ; STRATEGY   : Use HOF filter on the ListOfBlock which are not a teammate
    ;              of the selected block
    
    (define (intersecting-blocks prev-mates)     
      (filter 
       ; Block<%> -> Boolean
       ; GIVEN     : an object of a class that implements the Block<%> interface
       ; RETURNS   : true iff this block intersects with the selected block,
       ;             otherwise false
       (lambda (other-block) (intersect? (send other-block block-x)
                                         (send other-block block-y)))
       (set-diff blocks prev-mates)))
    
    ;......................................................................
    
    ; intersect? Integer Integer -> Boolean
    ; GIVEN     : the x and y coordinates of a Block
    ; RETURNS   : true, iff the given block intersects with the selected block,
    ;             otherwise false 
    ; STRATEGY  : Combining simpler functions
    
    (define (intersect? block2-x block2-y) 
      (and (>= (+ block2-x BLOCK-SIDE-LENGTH) x (- block2-x BLOCK-SIDE-LENGTH))
           (>= (+ block2-y BLOCK-SIDE-LENGTH) y (- block2-y BLOCK-SIDE-LENGTH))))
    
    ;......................................................................
    
    ; mates-of-new-intersected-blocks : ListOfBlock -> ListOfBlock  
    ; GIVEN     : a ListOfBlock represnting the newly intersected blocks for
    ;             the selected block
    ; RETURNS   : a ListOfBlock consisting of all blocks which are teammates
    ;             to the blocks in given LOB
    ; STRATEGY  : Template for LOB on list-new-blocks
    
    (define (mates-of-new-intersected-blocks list-new-blocks)    
      (cond 
        [(empty? list-new-blocks) empty] 
        [else
         (set-union (cons (first list-new-blocks)     
                          (send (first list-new-blocks) get-team)) 
                    (mates-of-new-intersected-blocks (rest list-new-blocks)))]))
    
    ;......................................................................
    
    ; change-all-block-teammates : -> Void
    ; GIVEN    : no arguments
    ; EFFECT   : updates the teammates list for every every teammate of the
    ;            selected block after an intersected block is added to the team
    ; STRATEGY : Use HOF for-each on teammates
    
    (define (change-all-block-teammates)   
      (for-each
       ; Block<%> -> void
       ; GIVEN     : a block which is teammate of the selected block
       ; EFEFCT    : updates block with updated list of teammates
       (lambda (block) (send block block-append-teammates
                             (set-minus (cons this teammates) 
                                        block)))              
       teammates))
    
    ;......................................................................
    
    ; block-append-teammates : ListOfBlock -> void
    ; GIVEN     : a ListOfBlock representing the teammates of a block
    ; EFFECT    : updates the list of teammates for the given block
    ; STRATEGY  : Combine simple functions
    
    (define/public (block-append-teammates lst-blocks)      
      (set! teammates (set-union teammates
                                 (set-diff lst-blocks teammates))))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN    : the x and y coordinates of the mouse click
    ; EFFECT   : Updates the blocks present according to the
    ;            mouse button down event
    ; STRATEGY : Use HOF for-each on (cons this blocks)   
    
    (define/public (after-button-down mx my)
      (begin
        (set! w-mx mx)    
        (set! w-my my)    
        (for-each
         ; Block<%> -> void
         ; GIVEN    : an object of a class that implements the Block<%>
         ;            interface
         ; EFFECT   : updates the given object after mouse button down event
         (lambda (block) (send block block-after-button-down mx my))
         (cons this blocks)))) 
    
    ;......................................................................
    
    ; block-after-button-down : NonNegInt NonNegInt -> void
    ; GIVEN    : the x and y coordinates of the mouse click
    ; EFFECT   : updates the fields of a block
    ;            as it should be after a mouse button down event
    ; STRATEGY : Combining simple functions
    
    (define/public (block-after-button-down mx my)
      (begin
        (set! w-mx mx) 
        (set! w-my my)
        (set! mx-offset (- mx x))
        (set! my-offset (- my y))
        (set! selected? (in-block? mx my))))
    
    ;......................................................................
    
    ; is-selected? -> Boolean
    ; GIVEN    : no argument
    ; RETURNS  : true, if the block is selected, otherwise false
    
    (define/public (is-selected?)
      selected?)
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN    : the x and y coordinates of the mouse click
    ; EFFECT   : Updates the blockS present according to the mouse button up
    ;            event
    ; STRATEGY : Combine simple functions
    
    (define/public (after-button-up mx my) 
      (begin
        (set! w-mx mx)  
        (set! w-my my)  
        (for-each
         ; Block<%>  -> void
         ; GIVEN     : an object of a class that implements the Block<%>
         ;             interface
         ; EFFECT    : updates the given object after mouse button down event
         (lambda (block) (send block block-after-button-up mx my))
         (cons this blocks))))
    
    ;......................................................................
    
    ; block-after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN    : the x and y coordinates of the mouse click
    ; EFFECT   : updates the fields of  block as it should be after a
    ;            mouse button up event
    ; STRATEGY : Combine simple function
    
    (define/public (block-after-button-up mx my)
      (begin (set! selected? false)
             (set! mx-offset mx)
             (set! my-offset my)))
    
    ;...................................................................... 
    
    ; in-block?    : NonNegInt NonNegInt -> Boolean
    ; GIVEN        : the x and y coordinates of the mouse event
    ; RETURNS      : true, iff the coordinates of mouse event lie within the
    ;                block, otherwise false
    ; STRATEGY     : Combining simpler functions
    
    (define/public (in-block? mx my)
      (and
       (<= (- x  HALF-BLOCK-SIDE-LENGTH) mx (+ x HALF-BLOCK-SIDE-LENGTH))
       (<= (- y  HALF-BLOCK-SIDE-LENGTH) my (+ y HALF-BLOCK-SIDE-LENGTH))))
    
    ;......................................................................
    
    ; get-blocks  : -> ListOfBlock
    ; GIVEN       : No argument
    ; RETURNS     : the ListOfBlock consisting of all blocks present
    
    (define/public (get-blocks)
      blocks)
    
    ;......................................................................
    
    ; block-x  : -> Int
    ; GIVEN    : No argument
    ; RETURNS  : the x coordinate of the block's center
    
    (define/public (block-x)
      x)
    
    ;......................................................................
    ; block-y  : -> Int
    ; GIVEN    : No argument
    ; RETURNS  : the y coordinate of the block's center
    
    (define/public (block-y)
      y)
    
    ;......................................................................
    
    ; get-team  : -> ListOfBlock
    ; GIVEN     : No argument
    ; RETURNS   : the ListOfBlock consisting of blocks that are teammates
    ;             for the block
    
    (define/public (get-team)
      teammates)
    
    ;......................................................................
    
    ; add-teammate : Block<%> -> Void
    ; GIVEN        : a Block<%>
    ; EFFECT       : adds the given block to this block's team
    
    (define/public (add-teammate block)
      (begin
        (set! teammates
              (append teammates (mates-of-new-intersected-blocks (list block))))
        (change-all-block-teammates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; make-block : NonNegInt NonNegInt ListOfBlock -> Block<%>
    ; GIVEN      : the x and y coordinates of the mouse click and ListOfBlock of
    ;              all blocks present
    ; RETURNS    : a new instance of class Block% with given coordinates of
    ;              mouse click and ListOfBlock
    ; STRATEGY   : Combining simpler functions

    (define (make-block mx my blocks)
      (new Block%
       [w-mx mx][w-my my]
       [x mx] [y my] 
       [selected? false]
       [mx-offset 0]
       [my-offset 0]
       [blocks blocks]     
       [teammates empty]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; run      : PosInt -> StatefulWorld<%>
; GIVEN    : a PosInt representing the frame rate
; RETURNS  : a StatefulWorld<%> after running the world
; STRATEGY : Combining simpler functions

(define (run rate)
  (local
    ((define initial-world (make-world CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      (send initial-world add-stateful-widget
            (make-block HALF-CANVAS-WIDTH
                        HALF-CANVAS-WIDTH empty))
      (send initial-world run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TEST CASES :

(define WORLD (make-world CANVAS-WIDTH CANVAS-HEIGHT))
(define INITIAL-WORLD (send WORLD add-stateful-widget
                            (make-block HALF-CANVAS-WIDTH
                                        HALF-CANVAS-WIDTH empty)))

(define BLOCK1 (make-block 100 100 empty))
(define BLOCK2 (make-block 100 100 empty))
(define BLOCK3 (make-block 100 100 empty))

;......................................................................

; after tick

(define BLOCK1-AFTER-TICK (send BLOCK1 after-tick))

(begin-for-test
  (begin BLOCK1-AFTER-TICK
         (check-equal? (send BLOCK1 block-x) 100
                       "The coordinates of block shouldnot change")))

;......................................................................

; after invalid KeyEvent

(define BLOCK1-AFTER-INVALID-KEYEVENT (send BLOCK1 after-key-event
                                            NOT-VALID-KEYEVENT))

(begin-for-test
  (begin BLOCK1-AFTER-INVALID-KEYEVENT 
         (check-equal? (send BLOCK1 block-x) 100
                       "the block shouldnot respond to invalid KeyEvents")))
;......................................................................

; after mouse-down 

(define BLOCK1-AFTER-BUTTON-DOWN (send BLOCK1 after-button-down 100 100))
(begin-for-test
  (begin BLOCK1-AFTER-BUTTON-DOWN
         (check-equal? (send BLOCK1 is-selected?) true
                       "the block should respond to mouse down event"))) 
;......................................................................

; after mouse-up

(define BLOCK1-AFTER-BUTTON-UP (send BLOCK2 after-button-up 100 100))
(begin-for-test
  (begin BLOCK1-AFTER-BUTTON-UP 
         (check-equal? (send BLOCK2 is-selected?) false
                       "the block should respond to mouse up event")))
;......................................................................

; after mosue-drag

(define BLOCK3-AFTER-MOUSE-DOWN(send BLOCK3 after-button-down 100 100))
(define BLOCK3-AFTER-DRAG (send BLOCK3 after-drag 150 150))
(begin-for-test
  (begin BLOCK3-AFTER-DRAG
         (check-equal? (send BLOCK3 block-x) 150
                       "the block should respond to mouse darg event")))
;......................................................................

; add teammates

(define BLOCK4 (make-block 100 100 empty))
(define BLOCK5 (make-block 110 110 empty))
(define BLOCK4-add-teammate (send BLOCK4 add-teammate BLOCK5))
(begin-for-test
  (begin BLOCK4
         (check-equal? (send (first (send BLOCK4 get-team)) block-x) 110
                       "the block should add a new teammate")))

(define BLOCK6 (make-block 200 200 (list BLOCK4 BLOCK5)))
(define BLOCK7 (make-block 210 210 empty))
(define BLOCK6-add-teammate (send BLOCK6  add-teammate BLOCK7))
(define BLOCK6-add-teammate-SELECT (send BLOCK6  after-button-down 200 200))

(define WHOLE-TEAM (send BLOCK6 after-drag 120 110))
(begin-for-test
  (begin WHOLE-TEAM
         (check-equal? (length (send BLOCK4 get-team)) 3
                       "the team members of the block should be equal to three")))
(begin-for-test
  (begin WHOLE-TEAM
         (check-equal? (length (send BLOCK6 get-blocks)) 2
                       "the number of blocks in world should be two "))) 
;......................................................................

; after valid KeyEvent

(define BLOCK-8 (make-block 150 150 empty))
(define BLOCK8-MOUSE-DOWN (send BLOCK-8 after-button-down 100 100))
(define BLOCK8-AFTER-VALID-KEYEVENT
  (send BLOCK-8 after-key-event ADD-NEW-BLOCK-KEYEVENT))
(begin-for-test
  (begin BLOCK8-AFTER-VALID-KEYEVENT
         (check-equal? (length (send BLOCK-8 get-blocks)) 1
                       "a new block should get added")))
;......................................................................

; add-to-scene

(define TEST-SCENE (empty-scene 300 300))
(define TEST-BLOCK1 (make-block 150 150 empty))
(define TEST-BLOCK2 (make-block 50 50 empty))
(define selected-block
  (new Block% [w-mx 0][w-my 100] [x 100] [y 100] [selected? true]
       [mx-offset 0] [my-offset 0]
       [blocks (list TEST-BLOCK1)] [teammates (list TEST-BLOCK1)]))
(check-equal?
 (send TEST-BLOCK1 draw-the-image TEST-BLOCK1 TEST-SCENE)
 (place-image (square BLOCK-SIDE-LENGTH BLOCK-STYLE BLOCK-UNSELECTED)
              150 150 TEST-SCENE)
 "Scene should have a green block in center")
(check-equal?
 (send selected-block draw-the-image selected-block TEST-SCENE)
 (place-image (square BLOCK-SIDE-LENGTH BLOCK-STYLE BLOCK-SELECTED)
              100 100 TEST-SCENE)
 "Scene should have a red block left to scene center")
(check-equal?
 (send selected-block add-to-scene TEST-SCENE)
 (place-image (square BLOCK-SIDE-LENGTH BLOCK-STYLE BLOCK-UNSELECTED)
              150 150 TEST-SCENE)
 "Scene should have a green block in center")

;......................................................................