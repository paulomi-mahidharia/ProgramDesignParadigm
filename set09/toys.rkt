
;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 09 : QUESTION 01;;;;;;;;;;;;;;;;;;;;;;

; PROBLEM STATEMENT :

; The marvellous toy :
; The toy consists of a canvas that is 600 pixels high and 500 pixels wide.
; On the canvas, the system displays a circle of radius 10 in outline mode.
; The circle initially appears in the center of the canvas. We call this circle
; the "target."
; The child interacts with the toy by dragging the target (using smooth drag)
; and by typing characters into the system.
; Each of the characters listed below causes a new toy to be created with its
; center located at the center of the target. Toys are also moveable using
; smooth drag.

; 1)  When the child types "s", a new square-shaped toy pops up.
;     It is represented as a 40x40 pixel outline square.
;     When a square-shaped toy appears, it begins travelling rightward at a
;     constant rate When its edge reaches the edge of the canvas, it executes
;     a Perfect Bounce.

; 2)  When the child types "t", a new throbber appears.
;     A throbber starts as a solid green circle of radius 5.
;     At every tick, it expands gradually until it reaches a radius of 20.
;     Once it reaches a radius of 20, it contracts gradually until it reaches a
;     radius of 5, and then resumes its cycle.

; 3) When the child types "w", a clock appears. This clock displays the number
;    of ticks since it was created. Otherwise the appearance of the clock is
;    unspecified.

; 4) When the child types "f", Football appears. The football initially appears
;    as an image of a football, but it gets smaller with every tick until it
;    reaches size 0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;(check-location "09" "toys.rkt")

(provide
 make-world
 run
 make-square-toy
 make-throbber
 make-clock
 make-football)

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; GLOBAL CONSTANTS :

;;Dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

;;Scene of an empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;Constants for KeyEvents
(define ADD-SQUARE-TOY-KEYEVEMT "s")
(define ADD-THROBBER-TOY-KEYEVEMT "t")
(define ADD-CLOCK-TOY-KEYEVEMT "w")
(define ADD-FOOTBALL-TOY-KEYEVEMT "f")
(define NOT-VALID-KEYEVENT "g")

;;Constants for MouseEvents
(define MOUSE-BUTTON-DOWN "button-down")
(define MOUSE-BUTTON-UP "button-up")
(define MOUSE-DRAG "drag")
(define INVALID-MOUSE-EVENT "enter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;INTERFACES :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The WorldState<%> interface:

; A WorldState<%> contains all the fuctions to which a world responds.
; A WorldState represents the state of the world at a given instant of time. 

(define WorldState<%>
  (interface ()
    
    ; -> WorldState<%>
    ; GIVEN   : no arguments
    ; RETURNS : the state of the world at the next tick
    after-tick          
    
    ; NonNegInt NonNegInt MouseEvent-> WorldState<%>
    ; GIVEN    : coordinates of mouse and a mouse event
    ; RETURNS  : the state of the world that should follow the
    ;            given mouse event at the given location.
    after-mouse-event
    
    
    ; KeyEvent : KeyEvent -> WorldState<%>
    ; GIVEN    : a key event
    ; RETURNS  : the state of the world that should follow the
    ;            given key event
    after-key-event     
    
    ; Scene -> Scene
    ; GIVEN    : a scene
    ; RETURNS  : a scene that depicts this World
    to-scene)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Widget<%> interface:

; Every object that lives in the world must implement the Widget<%>
; interface.

(define Widget<%>
  (interface()
    
    ; -> Widget<%>
    ; GIVEN    : no arguments
    ; RETURNS  : the state of this object that should follow at time t+1.
    after-tick
    
    ; NonNegInt NonNegInt -> Widget<%>
    ; GIVEN   : coordinates of mouse event
    ; RETURNS : the state of this object that should follow the
    ;           specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ; KeyEvent : KeyEvent -> Widget<%>
    ; GIVEN    : a key event 
    ; RETURNS  : the state of this object that should follow the
    ;            given key event
    after-key-event
    
    ; Scene -> Scene
    ; GIVEN    : a scene
    ; RETURNS  : a scene like the given one, but with this object
    ; painted on it.
    add-to-scene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The PlaygroundState<%> interface:

; PlaygroundState<%> implements all the functions of WorldState<%>
; PlaygroundState represensts a canvas which has the target and the toys(if any)

(define PlaygroundState<%>
  (interface(WorldState<%>) 
    
    ;; -> Integer
    ;; RETURN : the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; Is the target selected?
    target-selected?
    
    ;; -> ListOfToy<%>
    get-toys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Toy<%> interface:

; Toy<%> implements all the functions of Widget<%>
; A Toy represents any live object in the world

(define Toy<%>
  (interface(Widget<%>)
    
    ; -> Int
    ; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS : some data related to the toy.  The interpretation of
    ;;           this data depends on the class of the toy.
    ;; For each toy, it should be as follows:
    ;; -- (a) For a square, it is the velocity of the square (rightward is
    ;;        positive)
    ;; -- (b) For a throbber, it is the current radius of the throbber
    ;; -- (c) For the clock, it is the current value of the clock
    ;; -- (d) For a football, it is the current size of the football (in
    ;;        arbitrary units; bigger is more)
    toy-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A ListOfToy<%> (LOT) is a list of Toy<%>
; i.e a list of toys that implement the Toy<%>
; interface.

; ListOfToy<%> can be :
; -- empty
; -- (cons Toy<%> ListOfToy<%>)

; INTERPRETATION :
; --empty                      : an empty list
; --(cons Toy<%> ListOfToy<%>) : a list where the first element is a toy and the
;                                rest is LOT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNTIONALITY :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; run      : PosNum PosInt -> PlaygroundState<%> 
; GIVEN    : a frame rate (in seconds/tick) and a square-speed (in pixels/tick), 
;            creates and runs a world in which square toys travel at the given
;            speed. Returns the final state of the world
; STRATEGY : Combining simpler functions

(define (run frame-rate speed)
  (big-bang (make-world speed)
            (on-tick (lambda (w) (send w after-tick)) frame-rate)
            (on-draw (lambda (w) (send w to-scene)))
            (on-key (lambda (w kev) (send w after-key-event kev)))
            (on-mouse (lambda (w mx my mev) (send w after-mouse-event mx my mev))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-world : PosInt -> PlaygroundState<%>
; GIVEN      : a PosInt representing the speed of the square toy, in pixels/tick
; RETURNS    : a world with a target, but no toys, and in which any
;              square toys created in the future will travel at the given
;              speed (in pixels/tick)
; STRATEGY   : Combining simpler functions

(define (make-world speed)
  (new World% [toys empty] [square-speed speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-world-state : Widget<%> LOT Integer -> PlaygroundState<%> 
; GIVEN      : an object of Target% class, a list of toys' interface and
;              speed of square toys in the world, in pixels/tick
; RETURNS    : an object of a class that implements PlaygroundState<%>
; STRATEGY   : Creates an instance of class World%

(define (make-world-state target toys square-speed)
  (new World% [target target] [toys toys] [square-speed square-speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASSES :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The World% class

; a World is a (new  World% [target Widget<%>]
;                           [toys LOT]
;                           [square-speed Integer]) 
; a World represents a world that consists of a target and toys

(define World%
  (class* object% (PlaygroundState<%>)
    
    ; Fields for World
    ; ----------------
    
    (field [TARGET-INITIAL-X (/ CANVAS-WIDTH 2)])
    (field [TARGET-INITIAL-Y (/ CANVAS-HEIGHT 2)])
    
    (init-field toys square-speed)
    (init-field [target (new Target% [x TARGET-INITIAL-X][y TARGET-INITIAL-Y])])
    
    ; ITNTERPRETATION :
    
    ; target       : the target of the world that provides location for toys to
    ;                be placed
    ; toys         : ListOfToy in the world
    ; square-speed : speed of the square toys in the world, in pixels/tick
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for World
    ; ----------------
    
    ; after-tick : -> PlaygroundState<%>
    ; GIVEN      : no arguments
    ; RETURNS    : the state of the world at the next tick
    ; STRATEGY   : Use HOF map on LOT
    
    (define/public (after-tick)
      (make-world-state
       (send target after-tick)
       (map
        ; Toy<%> -> Toy<%>
        ; GIVEN   : an object of a class that implements the Toy<%> interface
        ; RETURNS : the given object after a tick
        (lambda (toy) (send toy after-tick))
        toys)
       square-speed))
    
    ;......................................................................
    
    ; to-scene  : -> Scene
    ; GIVEN     : a scene
    ; RETURNS   : a scene that depicts this World
    ; STRATEGY  : Use HOF foldr on LOT
    
    (define/public (to-scene)
      (foldr
       ; Toy<%> Scene -> Scene
       ; GIVEN   : an object of a class that implements the Toy<%> interface
       ;           and a scene
       ; RETURNS : the scene same as given one but with the given object
       ;           placed on it
       (lambda (toy scene) (send toy add-to-scene scene))
       (send target add-to-scene EMPTY-CANVAS)
       toys)) 
    ;......................................................................
    
    ; after-key-event : KeyEvent -> PlaygroundState<%>
    ; GIVEN    : a key event
    ; RETURNS  : the state of the world that should follow the given key event
    ; STRATEGY : Cases on KeyEvent
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev ADD-SQUARE-TOY-KEYEVEMT)
         (make-world-state
          target
          (cons (make-square-toy (target-x) (target-y) square-speed) toys)
          square-speed)]
        
        [(key=? kev ADD-THROBBER-TOY-KEYEVEMT)
         (make-world-state
          target
          (cons (make-throbber (target-x) (target-y)) toys)
          square-speed)]
        
        [(key=? kev ADD-CLOCK-TOY-KEYEVEMT)
         (make-world-state
          target 
          (cons (make-clock (target-x) (target-y)) toys)
          square-speed)]
        
        [(key=? kev ADD-FOOTBALL-TOY-KEYEVEMT)
         (make-world-state
          target
          (cons (make-football (target-x) (target-y)) toys)
          square-speed)]
        
        [else this]))
    ;......................................................................
    
    ; after-mouse-event : NonNegInt NonNeg MouseEvent -> PlaygroundState<%>
    ; GIVEN    : a coordinates of mouse click and a mouse event
    ; RETURNS  : the state of the world that should follow the given mouse
    ;            event at the given location
    ; STRATEGY : Divide into cases based on MouseEvent
    
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev MOUSE-BUTTON-DOWN) (world-after-button-down mx my)]         
        [(mouse=? mev MOUSE-DRAG) (world-after-drag mx my)]
        [(mouse=? mev MOUSE-BUTTON-UP) (world-after-button-up mx my)]
        [else this]))
    ;......................................................................
    
    ; world-after-button-down : NonNegInt NonNegInt -> PlaygroundState<%>
    ; GIVEN    : the x and y mouse cordinates of a mouse click 
    ; RETURNS  : World followed by the MOUSE-BUTTON-DOWN MouseEvent
    ; STRATEGY : Use HOF map on ListOfToy 
    
    (define/public (world-after-button-down mx my)
      (make-world-state
       (send target after-button-down mx my)
       (map
        ; Toy<%> -> Toy<%>
        ; GIVEN    : an object of a class that implements the Toy<%> interface
        ; RETURNS  : the state of the given object after MOUSE-BUTTON-DOWN
        ;            MouseEvent
        (lambda (toy) (send toy after-button-down mx my))
        toys)
       square-speed))
    ;......................................................................
    
    ; world-after-drag : NonNegInt NonNegInt -> PlaygroundState<%>
    ; GIVEN    : the x and y mouse cordinates of a mouse click 
    ; RETURNS  : World followed by the MOUSE-DRAG MouseEvent
    ; STRATEGY : Use HOF map on ListOfToy 
    
    
    (define/public (world-after-drag mx my)
      (make-world-state
       (send target after-drag mx my)
       (map
        ; Toy<%> -> Toy<%>
        ; GIVEN   : an object of a class that implements the Toy<%> interface
        ; RETURNS : the state of the given object after MOUSE-DRAG MouseEvent
        (lambda (toy) (send toy after-drag mx my))
        toys)
       square-speed))
    ;......................................................................
    
    ; world-after-button-up : NonNegInt NonNegInt -> PlaygroundState<%>
    ; GIVEN    : the x and y mouse cordinates of a mouse click 
    ; RETURNS  : World followed by the MOUSE-BUTTON-UP MouseEvent
    ; STRATEGY : Use HOF map on ListOfToy 
    
    
    (define/public (world-after-button-up mx my)
      (make-world-state
       (send target after-button-up mx my)
       (map
        ; Toy<%> -> Toy<%>
        ; GIVEN   : an object of a class that implements the Toy<%> interface
        ; RETURNS : the state of the given object after MOUSE-BUTTON-UP MouseEvent
        (lambda (toy) (send toy after-button-up mx my))
        toys)
       square-speed))
    
    ;......................................................................
    
    ; target-x : -> Integer
    ; RETURN : th x coordinate of the target
    (define/public (target-x)  
      (send target target-x))
    
    ;......................................................................
    
    ; target-y : -> Integer
    ; RETURN : th y coordinate of the target
    (define/public (target-y)  
      (send target target-y))
    
    ;......................................................................
    
    ; target-selected? : -> Boolean
    ; RETURN : the value of selected? field of target
    ;          (true, if it selected else false)
    (define/public (target-selected?)  
      (send target target-selected?))
    ;......................................................................  
    
    ; get-toys : -> ListOfToy<%>
    ; RETURN : list of all toys present in the world else returns empty
    (define/public (get-toys)
      toys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;

; The Taget% class:

; A Target is a (new Target% [x Int]
;                            [y Int]
;                            [selected? Boolean]
;                            [saved-mx NonNegInt]
;                            [saved-my NonNegInt])
; Target is the Widget which enables addition of toys in the World after
; specific KeyEvents
; A Target is draggable by mouse

(define Target%
  (class* object% (Widget<%>)
    
    ; Fields for Target
    ; -----------------
    (init-field x y)
    (init-field [selected? false] [saved-mx 0] [saved-my 0])
    
    ; INTERPRETATION :
    ; x         : the x coordinate of the center of the target
    ; y         : the y coordinate of the center of the target
    ; selected? : true if the target is selected with the mouse
    ; saved-mx  : the x coordinate of the mouse if target is selected
    ;             otherwise 0
    ; saved-my  : the y coordinate of the mouse if target is selected
    ;             otherwise 0

    ; Represents the radius of the circle representing the target
    (field [TARGET-RADIUS 10])
    ; Represents the shape of the target image
    (field [TARGET-SHAPE circle])
    ; Represents the color of the target image
    (field [TARGET-COLOR "red"])
    ; Represents the mode of the target image
    (field [TARGET-MODE "outline"])'
    ; Represents the target image
    (field [TARGET-IMAGE (TARGET-SHAPE TARGET-RADIUS
                                       TARGET-MODE
                                       TARGET-COLOR)])
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for Target
    ; ------------------
    
    ; after-tick : -> Widget<%>
    ; GIVEN      : No aruguements
    ; RETURNS    : a state of Target after a tick
    ; STRATEGY   : creates an instance for the class Target%
    
    (define/public (after-tick) 
      (new Target% [x x] [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;......................................................................    
    
    ; after-button-down : NonNegInt NonNegInt -> Widget<%>
    ; GIVEN       : the x and y coordinate of the mouse click
    ; RETURNS     : a target that should follow after the MOUSE-BUTTON-DOWN
    ;               event
    ; STRATEGY    : Cases on in-target? 
    
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (new Target%
               [x x] [y y]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Widget<%>
    ; GIVEN           : the x and y coordinate of the mouse click
    ; RETURNS         : a target that should follow after the MOUSE-BUTTON-UP
    ;                   event
    ; STRATEGY        : Creates an instance for the class Target%
    
    (define/public (after-button-up mx my) 
      (new Target%
           [x x] [y y]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Widget<%>
    ; GIVEN      : the x and y coordinate of the mouse click
    ; RETURNS    : a target that should follow after the MOUSE-DRAG event
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (new Target%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Widget<%>
    ; RETURNS     : A target as it should be after the given key event.
    ; DETAILS     : a target ignores key events
    ; STRATEGY    : returns the passed instance without any changes  
    
    (define/public (after-key-event kev)
      this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with the target painted on it
    ; STRATEGY : Combine simpler functions
    
    (define/public (add-to-scene scene)
      (place-image TARGET-IMAGE x y scene)) 
    
    ;......................................................................
    
    ; target-x : -> Integer
    ; target-y : -> Integer
    ; RETURNS: the x and y coordinates of the target
    (define/public (target-x) x)
    (define/public (target-y) y)
    
    ;......................................................................
    
    ; target-selected? : -> Boolean
    ; RETURNS : true iff the target is selected,
    ;           otherwise false
    (define/public (target-selected?) selected?)
    
    ;......................................................................
    
    ; in-target? : NonNegInt NonNegInt -> Boolean
    ; GIVEN      : the x and y coordinates of the mouse 
    ; RETURNS    : true, iff the mouse click is within the target else false
    ; STRATEGY   : Combine simpler functions
    
    (define (in-target? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))  
          (sqr TARGET-RADIUS)))
    
    ; methods for test
    ; RETURNS : true iff target selected otherwise false
    (define/public (for-test:selected?)  
      selected?)
    
    ; RETURNS : the x coordiate of the mouse for the target
    (define/public (for-test:saved-mx)  
      saved-mx)
    
    ; RETURNS : the y coordiate of the mouse for the target
    (define/public (for-test:saved-my)  
      saved-my)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-square-toy : PosInt PosInt PosInt -> Toy<%>
; GIVEN     : an x and a y position, and a speed
; RETURNS   : a new instance of class SquareToy% with given centers and
;             travelling right at the given speed.

(define (make-square-toy new-x new-y new-speed)
  (new SquareToy%
       [x new-x] [y new-y]
       [speed new-speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a SquareToy is a (new SquareToy%
;                               [x Int] [y Int]
;                               [selected? Boolean]
;                               [off-x NonNegInt]
;                               [off-y NonNegInt]
;                               [speed NonNegInt])
; a SqaureToy represnts a squaretoy in the playground.
; a SquareToy starts travelling rightward at a constant given speed.
; When its edge reaches the edge of the canvas, it executes a Perfect Bounce.
; a SquareToy is draggable and it doesnot respond to KeyEvents

(define SquareToy%
  (class* object% (Toy<%>)
    
    ; Fields for SquareToy
    ; --------------------
    
    ;(init-field x y selected? off-x off-y speed)
    (init-field x y)
    (init-field speed)
    (init-field [selected? false])
    (init-field [off-x 0] [off-y 0])
    
    ; INTERPRETATION :
    ; x         : the x coordinate of the center of the square toy
    ; y         : the y coordinate of the center of the square toy
    ; speed     : the speed at which the square toy moves horizontaly
    ;             (in pixels/tick)
    ; selected? : true if the square toy is selected with the mouse
    ; off-x     : the x coordinate of the mouse if toy is selected otherwise 0
    ; off-y     : the y coordinate of the mouse if toy is selected otherwise 0 

    ; Represents the length of the side of square
    (field [S-LEN 40])
    ; Represents the half of the square's side length
    (field [S-BOUNDARY-MIN (/ S-LEN 2)])
    ; Represents the difference of canvas width and the half
    ; of the square's side length
    (field [S-BOUNDARY-MAX (- CANVAS-WIDTH S-BOUNDARY-MIN)])
    ; parameters used for the image of square toy
    (field [SQUARE-MODE "outline"])
    (field [SQUARE-COLOR "purple"])
    ; Represents the image for displaying square toy
    (field [SQUARE-IMAGE (square S-LEN SQUARE-MODE SQUARE-COLOR)])
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for SquareToy
    ; ---------------------
    
    ; after-tick : -> Toy<%>
    ; GIVEN      : No aruguements
    ; RETURNS    : a state of Toy after a tick
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-tick)  
      (if selected?
          this
          (new SquareToy% [x (update-coordinate)] 
               [y y] 
               [selected? selected?]
               [off-x off-x]
               [off-y off-y]
               [speed (update-speed)])))
    
    ;......................................................................
    
    ; update-coordinate : ->  NonNegInt
    ; RETURNS : the x coordinate of the squaretoy's center based on its speed
    
    (define (update-coordinate)
      (cond
        [(<= (+ x speed) S-BOUNDARY-MIN) S-BOUNDARY-MIN]
        [(>= (+ x speed) S-BOUNDARY-MAX) S-BOUNDARY-MAX]
        [else (+ x speed)]))
    
    ;......................................................................
    
    ; update-speed : -> Int
    ; RETURNS : the speed of the square toy based on its current location
    
    (define (update-speed)
      (if (or (<= (+ x speed) S-BOUNDARY-MIN) 
              (>= (+ x speed) S-BOUNDARY-MAX))
          (- 0 speed)
          speed))
    
    ;......................................................................
    
    ; in-square? : Int Int NonNegInt NonNegInt -> Boolean
    ; GIVEN      : the coordinate of the squaretoy's center and the coordinates
    ;              of the mouse event
    ; RETURNS    : true, iff the mouse click is within the SquareToy else false
    ; STRATEGY   : Combining simpler functions
    
    (define (in-square? x y mx my)
      (and
       (<= (- (toy-x)  S-BOUNDARY-MIN) mx (+ (toy-x) S-BOUNDARY-MIN))
       (<= (- (toy-y)  S-BOUNDARY-MIN) my (+ (toy-y) S-BOUNDARY-MIN))))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN     : the x and y coordinate of the mouse click
    ; RETURNS   : a SquareToy that should follow after the MOUSE-BUTTON-DOWN event
    ; STRATEGY  : Creates a new instance of class SquareToy%
    
    (define/public (after-button-down mx my)
      (new SquareToy% [x x]
           [y y] 
           [selected? (in-square? x y mx my)]
           [off-x (- mx x)]
           [off-y (- my y)]
           [speed speed]))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN     : the x and y coordinate of the mouse click
    ; RETURNS   : a SquareToy that should follow after the MOUSE-BUTTON-UP event
    ; STRATEGY  : Creates a new instance of class SquareToy%
    
    (define/public (after-button-up mx my)
      (new SquareToy% [x x]
           [y y] 
           [selected? false]
           [off-x 0]
           [off-y 0]
           [speed speed]))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%> 
    ; GIVEN      : the x and y coordinate of the mouse click
    ; RETURNS    : a SquareToy that should follow after the MOUSE-DRAG event
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (new SquareToy% [x (+ x off-x)]
               [y (+ y off-y)] 
               [selected? true]
               [off-x (- mx (+ x off-x))]
               [off-y (- my (+ y off-y))]
               [speed speed])
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Toy<%> 
    ; RETURNS   : A SquareToy as it should be after the given key event
    ; DETAILS   : a SquareToy ignores key events
    ; STRATEGY  : Returns the given object as it is, without any changes 
    
    (define/public (after-key-event kev)
      this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with the SquareToy painted on it
    ; STRATEGY : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMAGE x y scene ))
    
    ;......................................................................
    
    ; toy-x : -> Int
    ; RETURNS: the x coordinate of the center of the square toy
    (define/public (toy-x)
      x)
    
    ;......................................................................
    
    ; toy-y : -> Int
    ; RETURNS : teh y coordiante of the center of the square toy
    (define/public (toy-y)
      y)
    
    ;......................................................................
    
    ; toy-data : -> Int
    ; RETURNS : the current speed of the square toy
    (define/public (toy-data)
      speed)
    
    ;......................................................................
    
    ; test-methods
    
    ; -> NonNegInt
    (define/public (for-test:max-boundary)
      S-BOUNDARY-MAX)
    
    ; -> NonNegInt
    (define/public (for-test:min-boundary)  
      S-BOUNDARY-MIN)
    
    ; methods for test
    ; RETURNS : true iff toy selected otherwise false
    (define/public (for-test:selected?)  
      selected?)
    
    ; RETURNS : the x coordiate of the mouse for the toy
    (define/public (for-test:off-x)  
      off-x)
    
    ; RETURNS : the y coordiate of the mouse for the toy
    (define/public (for-test:off-y)  
      off-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: NonNegInt NonNegInt -> Toy<%>
;; GIVEN    : x and y coordinates that represent a position
;; RETURNS  : an object representing a throbber at the given position

(define (make-throbber new-x new-y)
  (new ThrobberToy%
       [x new-x] [y new-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A ThrobberToy is a (new ThrobberToy% [x NonNegInt]
;                                      [y NonNegInt]
;                                      [selected? Boolean]
;                                      [off-x NonNegInt]    
;                                      [off-y NonNegInt]                          
;                                      [r PosInt]
;                                      [r-speed Integer])
; A ThrobberToy represnts a toy which throbs at a given location on the canvas.
; A ThrobberToy starts expanding from the given minimum radius at a constant
; given speed till it reaches a predefined maximum radius and then contracts
; similarly.
; A ThrobberToy is draggable and it doesnot respond to KeyEvents

(define ThrobberToy% 
  (class* object% (Toy<%>)
    
    ; Fields for ThrobberToy
    ; ----------------------
    
    (init-field x y )
    (init-field [selected? false] [off-x 0] [off-y 0]
                [r MIN-T-RADIUS] [r-speed INC-SPEED ]) 
    
    ; INTERPRETATION :
    ; x         : the x coordinate of the center of the throbber toy
    ; y         : the y coordinate of the center of the throbber toy
    ; selected? : true if the throbber toy is selected with the mouse
    ; off-x     : the x coordinate of the mouse if toy is selected otherwise 0
    ; off-y     : the y coordinate of the mouse if toy is selected otherwise 0                 
    ; r         : The current radius of the throbber 
    ; r-speed   : Rate at which the radius of the throbe changes.
    ;             For this case we have considered the speed can be
    ;             either 1 or -1 based on the current radius of the
    ;             throbe

    ; The minimum radius of throbber possible
    (field [INC-SPEED 1])
    ; The maximum radius of throbber possible
    (field [DEC-SPEED -1])
    ; Represents the rate at which throbber's radius increases
    ; in size when it reaches MIN-T-RADIUS
    (field [MIN-T-RADIUS 5])
    ; Represents the rate at which throbber's radius decreases
    ; in size when it reaches MAX-T-RADIUS
    (field [MAX-T-RADIUS 20])
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for ThrobberToy
    ; -----------------------
    
    ; after-tick : -> Toy<%>
    ; GIVEN      : No aruguements
    ; RETURNS    : a state of Toy after a tick
    ; STRATEGY   : Creates a new instance of class ThrobberToy%
    
    (define/public (after-tick)   
      (new ThrobberToy% [x x]
           [y y]
           [selected? selected?]
           [off-x off-x]
           [off-y off-y]
           [r (+ r (update-r-speed MIN-T-RADIUS MAX-T-RADIUS))]
           [r-speed (update-r-speed MIN-T-RADIUS MAX-T-RADIUS)]))
    
    ;......................................................................
    
    ; update-r-speed : PosInt PosInt -> Integer
    ; GIVEN          : Predefined minimum and maximum values of the radius of a
    ;                  throbber toy
    ; RETURNS        : the value by which the radius should change
    ; STRATEGY       : Divide into cases based on whether current radius is greater
    ;                  than maximum radius or smaller than minimum radius or otherwise
    
    (define (update-r-speed min-r max-r)
      (cond
        [(> (+ r-speed r) max-r) DEC-SPEED]  
        [(< (+ r-speed r) min-r) INC-SPEED]   
        [else r-speed]))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN    : the x and y coordinate of the mouse click
    ; RETURNS  : a ThrobberToy that should follow after the MOUSE-BUTTON-DOWN event
    ; STRATEGY : Cases on whether the MOUSE-BUTTON-DOWN is within the boundary
    ;                   of the throbber 
    
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (new ThrobberToy%
               [x x] [y y]
               [selected? true]
               [off-x (- mx x)]
               [off-y (- my y)]
               [r r]
               [r-speed r-speed])
          this))
    
    ;......................................................................
    
    ; in-throbber? : NonNegInt NonNegInt -> Boolean
    ; GIVEN    : the x and y coordinate of the mouse click
    ; RETURNS  : true, iff the mouse click is within the ThrobberToy else false
    ; STRATEGY : Combining simpler functions
    
    (define (in-throbber? mx my)
      (<= (+ (sqr (- x mx)) (sqr (- y my)))  
          (sqr r)))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN    : the x and y coordinate of the mouse click
    ; RETURNS  : a ThrobberToy that should follow after the MOUSE-BUTTON-UP event
    ; STRATEGY : Creates a new instance of class ThrobberToy%
    
    (define/public (after-button-up mx my)
      (new ThrobberToy%
           [x x] [y y]
           [selected? false]
           [off-x off-x]
           [off-y off-y]
           [r r]
           [r-speed r-speed]))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%> 
    ; GIVEN    : the x and y coordinate of the mouse click
    ; RETURNS  : a ThrobberToy that should follow after the MOUSE-DRAG event
    ; STRATEGY : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (new ThrobberToy%
               [x (- mx off-x)]
               [y (- my off-y)]
               [selected? true]
               [off-x off-x]
               [off-y off-y]
               [r r]
               [r-speed r-speed])
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Toy<%> 
    ; RETURNS  : A ThrobberToy as it should be after the given key event
    ; DETAILS  : a ThrobberToy ignores key events
    ; STRATEGY : Returns the given object as it is, without any changes  
    
    (define/public (after-key-event kev)
      this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with the ThrobberToy painted on it
    ; STRATEGY : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (place-image (circle r "solid" "green") x y scene ))
    
    ;......................................................................
    
    ; toy-x : -> Int
    ; RETURNS: the x coordinate of the center of the throbber toy
    (define/public (toy-x)
      x)
    
    ;......................................................................
    
    ; toy-y : -> Int
    ; RETURNS : teh y coordiante of the center of the throbber toy
    (define/public (toy-y)
      y)
    
    ;......................................................................
    
    ; toy-data : -> Int
    ; RETURNS : the current radius of the throbber toy
    (define/public (toy-data)
      r)
    
    ; methods for test
    ; RETURNS : true iff toy selected otherwise false
    (define/public (for-test:selected?)  
      selected?)
    
    ; RETURNS : the x coordiate of the mouse for the toy
    (define/public (for-test:off-x)  
      off-x)
    
    ; RETURNS : the y coordiate of the mouse for the toy
    (define/public (for-test:off-y)  
      off-y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; make-clock : NonNegInt NonNegInt -> Toy<%>
;; GIVEN      : x and y coordinates that represent a position
;; RETURNS    : an object representing a clock at the given position.

(define (make-clock new-x new-y)
  (new ClockToy%
       [x new-x] [y new-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A ClockToy is a (new ClockToy% [x NonNegInt]
;                                [y NonNegInt]
;                                [selected? Boolean]
;                                [off-x NonNegInt]
;                                [off-y NonNegInt]
;                                [c-number NonNegInt])
; A CLockToy represnts a toy which displays the number of ticks since it was created.
; A CLockToy is draggable and it doesnot respond to KeyEvents

(define ClockToy%
  (class* object% (Toy<%>)
    
    ; Fields for ClockToy
    ; -------------------
    
    (init-field x y)
    (init-field [selected? false] [off-x 0] [off-y 0] [c-number 0])
    
    ; INTERPRETATION :
    ; x         : the x coordinate of the center of the throbber toy
    ; y         : the y coordinate of the center of the throbber toy
    ; selected? : true if the throbber toy is selected with the mouse
    ; off-x     : the x coordinate of the mouse if toy is selected otherwise 0
    ; off-y     : the y coordinate of the mouse if toy is selected otherwise 0                 
    ; c-number  : The number representing the current tick of the clock since
    ;             it was created
    
    ; Represents the image of the clock as displayed on the canvas                     
    (field [TEXT-IMAGE (text (number->string c-number) 12 "black")])
    ; Represents the width of the image of the clock
    (field [TEXT-IMAGE-WIDTH (image-width TEXT-IMAGE)])
    ; Represents the height of the image of the clock
    (field [TEXT-IMAGE-HEIGHT (image-height TEXT-IMAGE)])
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for ClockToy
    ; --------------------
    
    ; after-tick : -> Toy<%>
    ; GIVEN      : No aruguements
    ; RETURNS    : a state of ClockToy after a tick
    ; STRATEGY   : Creates a new instance of class ClockToy%
    
    (define/public (after-tick)  
      (new ClockToy% [x x]
           [y y]
           [selected? selected?]
           [off-x off-x]
           [off-y off-y]
           [c-number (+ c-number 1)]))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN      : the x and y coordinate of the mouse click
    ; RETURNS    : a ClockToy that should follow after the MOUSE-BUTTON-DOWN event
    ; STRATEGY   : Cases on whether the MOUSE-BUTTON-DOWN is within the
    ;                   boundary of the clock
    
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (new ClockToy%
               [x x] [y y]
               [selected? true]
               [off-x (- mx x)]
               [off-y (- my y)]               
               [c-number c-number])
          this))
    
    ;......................................................................
    
    ; in-clock? : NonNegInt NonNegInt -> Boolean
    ; GIVEN     : the x and y coordinate of the mouse click
    ; RETURNS   : true, iff the mouse click is within the ClockToy else false
    ; STRATEGY  : Combining simpler functions
    
    (define (in-clock? mx my)
      (and
       (<= (- (toy-x) TEXT-IMAGE-WIDTH) mx (+ (toy-x) TEXT-IMAGE-WIDTH))
       (<= (- (toy-y) TEXT-IMAGE-HEIGHT) my (+ (toy-y) TEXT-IMAGE-HEIGHT))))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN      : the x and y coordinate of the mouse click
    ; RETURNS    : a ClockToy that should follow after the MOUSE-BUTTON-UP event
    ; STRATEGY   : Creates a new instance of class ClockToy%                  
    
    (define/public (after-button-up mx my)
      (new ClockToy%
           [x x] [y y]
           [selected? false]
           [off-x off-x]
           [off-y off-y]
           [c-number c-number]))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%> 
    ; GIVEN      : the x and y coordinate of the mouse click
    ; RETURNS    : a ClockToy that should follow after the MOUSE-DRAG event
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (new ClockToy%
               [x (- mx off-x)]
               [y (- my off-y)]
               [selected? true]
               [off-x off-x]
               [off-y off-y]
               [c-number c-number])
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Toy<%> 
    ; RETURNS  : A ClockToy as it should be after the given key event
    ; DETAILS  : a ClockToy ignores key events
    ; DESIGN STRATEGY : Returns the given object as it is, without any changes
    
    (define/public (after-key-event kev)
      this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with the ClockToy painted on it
    ; DESIGN STRATEGY : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (place-image TEXT-IMAGE x y scene))
    
    ;......................................................................
    
    ; toy-x : -> Int
    ; RETURNS: the x coordinate of the center of the clock toy
    (define/public (toy-x)
      x)
    
    ;......................................................................
    
    ; toy-y : -> Int
    ; RETURNS : teh y coordiante of the center of the clock toy
    (define/public (toy-y)
      y)
    
    ;......................................................................
    
    ; toy-data : -> Int
    ; RETURNS : the current tick of the clock toy
    (define/public (toy-data) 
      c-number)
    
    ; methods for test
    ; RETURNS : true iff toy selected otherwise false
    (define/public (for-test:selected?)  
      selected?)
    
    ; RETURNS : the x coordiate of the mouse for the toy
    (define/public (for-test:off-x)  
      off-x)
    
    ; RETURNS : the y coordiate of the mouse for the toy
    (define/public (for-test:off-y)  
      off-y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;make-football : PosInt PostInt -> Toy<%>
;;GIVEN     : an x and a y position
;;RETURNS   : an instance of class FootballToy% with its x and y set to the
;;            passed arguements

(define (make-football new-x new-y)
  (new FootballToy% [x new-x] [y new-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; A FootballToy is a (new FootballToy% [x NonNegInt]
;                                      [y NonNegInt]
;                                      [selected? Boolean][
;                                      [off-x NonNegInt]
;                                      [off-y NonNegInt]
;                                      [scale-val NonNegInt])
; A FootballToy represnts a toy which is an Official Tom Brady Deflatable
; Football(TM). 
; The TBDF initially appears as an image of a football, but it gets smaller
; with every tick until it reaches size 0.
; A FootballToy is draggable and it doesnot respond to KeyEvents

(define FootballToy%
  (class* object% (Toy<%>)
    
    ; Fields for FootballToy
    ; ----------------------
    
    (init-field x y)
    (init-field [selected? false] [off-x 0] [off-y 0] [scale-val 100])
    ; INTERPRETATION :
    ; x         : the x coordinate of the center of the throbber toy
    ; y         : the y coordinate of the center of the throbber toy
    ; selected? : true if the throbber toy is selected with the mouse else false
    ; off-x     : the x coordinate of the mouse if toy is selected otherwise 0
    ; off-y     : the y coordinate of the mouse if toy is selected otherwise 0                 
    ; scale-val : The number representing the current scale,,,,,,,,, value which
    ;             will act as a numerator to the division by 100, thereby giving
    ;             a scale factor applied on the football image
    
    ; Represents the image of the football toy to be displayed in the scene                        
    (field [FOOTBALL (bitmap "football.png")])
    ; Represents the image of the football after scaling it by the given scale value
    (field [FOOTBALL-IMAGE-NOW (scale (/ scale-val 100) FOOTBALL)])
    ; Represents the width of the image of the football
    ; after scaling it by the given scale value
    (field [FOOTBALL-WIDTH-NOW (image-width FOOTBALL-IMAGE-NOW)])
    ; Represents the height of the image of the football
    ; after scaling it by the given scale value
    (field [FOOTBALL-HEIGHT-NOW (image-height FOOTBALL-IMAGE-NOW)])
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for FootballToy
    ; ----------------------
    
    ; after-tick : -> Toy<%>
    ; GIVEN      : No aruguements
    ; RETURNS    : a state of Toy after a tick
    ; STRATEGY   : Creates a new instance of class FootballToy%
    
    (define/public (after-tick)   
      (new FootballToy% [x x]
           [y y]
           [selected? selected?]
           [off-x off-x]
           [off-y off-y]
           [scale-val (update-scale)]))
    
    ;......................................................................
    
    ; update-scale : -> Boolean
    ; RETURNS : true iff the scale value reduces to 1 otherwise false      
    
    (define (update-scale)
      (if (= scale-val 1)
          scale-val
          (- scale-val 1)))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN             : the x and y coordinate of the mouse click
    ; RETURNS           : a FootballToy that should follow after the
    ;                     MOUSE-BUTTON-DOWN event
    ; DESIGN STRATEGY   : Cases on whether the MOUSE-BUTTON-DOWN is within
    ;                     the boundary of the FootballToy
    
    (define/public (after-button-down mx my)
      (if (in-football? mx my)
          (new FootballToy%
               [x x] [y y]
               [selected? true]
               [off-x (- mx x)]
               [off-y (- my y)]               
               [scale-val scale-val])
          this))
    
    ;......................................................................
    
    ; in-football? : NonNegInt NonNegInt -> Boolean
    ; GIVEN        : the x and y coordinate of the mouse click
    ; RETURNS      : true, iff the mouse click is within the FootballToy else false
    ; STRATEGY     : Combining simpler functions
    
    (define (in-football? mx my)
      (and
       (<= (- (toy-x) FOOTBALL-WIDTH-NOW) mx (+ (toy-x) FOOTBALL-WIDTH-NOW))
       (<= (- (toy-y) FOOTBALL-HEIGHT-NOW) my (+ (toy-y) FOOTBALL-HEIGHT-NOW))))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN           : the x and y coordinate of the mouse click
    ; RETURNS         : a FootballToy that should follow after the MOUSE-BUTTON-UP event
    ; DESIGN STRATEGY : Creates a new instance of class FootballToy%     
    
    (define/public (after-button-up mx my)
      (new FootballToy%
           [x x] [y y]
           [selected? false]
           [off-x off-x]
           [off-y off-y]               
           [scale-val scale-val]))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%> 
    ; GIVEN      : the x and y coordinate of the mouse click
    ; RETURNS    : a FootballToy that should follow after the MOUSE-DRAG event
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (new FootballToy%
               [x (- mx off-x)]
               [y (- my off-y)]
               [selected? true]
               [off-x off-x]
               [off-y off-y]
               [scale-val scale-val])
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Toy<%> 
    ; RETURNS      : A FootballToy as it should be after the given key event
    ; DETAILS      : a FootballToy ignores key events
    ; STRATEGY     : Returns the given object as it is, without any changes
    
    (define/public (after-key-event kev) this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN     : a scene 
    ; RETURNS   : a Scene like the given one with the FootballToy painted on it
    ; STRATEGY  : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (place-image FOOTBALL-IMAGE-NOW x y scene))
    
    ;......................................................................
    
    ; toy-x : -> Int
    ; RETURNS: the x coordinate of the center of the football toy
    (define/public (toy-x)
      x)
    
    ;......................................................................
    
    ; toy-y : -> Int
    ; RETURNS : teh y coordiante of the center of the football toy
    (define/public (toy-y)
      y)
    
    ;......................................................................
    
    ; toy-data : -> Int
    ; RETURNS : the current area of the football toy
    (define/public (toy-data)
      (* FOOTBALL-WIDTH-NOW FOOTBALL-HEIGHT-NOW))
    
    ; methods for test
    ; RETURNS : true iff toy selected otherwise false
    (define/public (for-test:selected?)  
      selected?)
    
    ; RETURNS : the x coordiate of the mouse for the toy
    (define/public (for-test:off-x)  
      off-x)
    
    ; RETURNS : the y coordiate of the mouse for the toy
    (define/public (for-test:off-y)  
      off-y)
    
    ; RETURNS : the scale-value for the toy
    (define/public (for-test:scale-val)
      scale-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; Testing Framework

;;Constants for Target
; the first coordinates of the target
(define TARGET-INITIAL-X1 (/ CANVAS-WIDTH 2)) 
(define TARGET-INITIAL-Y1 (/ CANVAS-HEIGHT 2))

;;Constants for ThrobberToy
; the minimum and maximum radius of the throbe toy 
(define MIN-T-RADIUS1 5)
(define MAX-T-RADIUS1 20)


;; test cases at the world level for target

(define INITIAL-WORLD (make-world 10))

(define INITIAL-WORLD-TARGET-SELECTED (send INITIAL-WORLD after-mouse-event
                                            TARGET-INITIAL-X1
                                            TARGET-INITIAL-Y1
                                            MOUSE-BUTTON-DOWN))

(define INITIAL-WORLD-TARGET-UNSELECTED (send INITIAL-WORLD after-mouse-event
                                              TARGET-INITIAL-X1
                                              TARGET-INITIAL-Y1
                                              MOUSE-BUTTON-UP))

(define INITIAL-WORLD-TARGET-DRAGGED (send INITIAL-WORLD-TARGET-SELECTED
                                           after-mouse-event
                                           250
                                           250
                                           MOUSE-DRAG))

(define INITIAL-WORLD-INVALID-MOUSE-EVENT (send INITIAL-WORLD after-mouse-event
                                                TARGET-INITIAL-X1
                                                TARGET-INITIAL-Y1
                                                INVALID-MOUSE-EVENT))


(define INITIAL-WORLD-TARGET-NOT-DRAGGED (send INITIAL-WORLD-TARGET-UNSELECTED
                                               after-mouse-event
                                               250
                                               250
                                               MOUSE-DRAG))

(define INITIAL-WORLD-TARGET-REMAINS-UNSELECTED (send INITIAL-WORLD-TARGET-UNSELECTED
                                                      after-mouse-event
                                                      300
                                                      300
                                                      MOUSE-BUTTON-DOWN))

(define INITIAL-WORLD-TARGET-SQUARE (send INITIAL-WORLD-TARGET-UNSELECTED
                                          after-key-event
                                          ADD-SQUARE-TOY-KEYEVEMT))

(define INITIAL-WORLD-TARGET-THROBBER (send INITIAL-WORLD-TARGET-UNSELECTED
                                            after-key-event
                                            ADD-THROBBER-TOY-KEYEVEMT))

(define INITIAL-WORLD-TARGET-CLOCK (send INITIAL-WORLD-TARGET-UNSELECTED
                                         after-key-event
                                         ADD-CLOCK-TOY-KEYEVEMT))

(define INITIAL-WORLD-TARGET-FOOTBALL (send INITIAL-WORLD-TARGET-UNSELECTED
                                            after-key-event
                                            ADD-FOOTBALL-TOY-KEYEVEMT))

(define INITIAL-WORLD-TARGET-NOT-VALID-KEYEVENT (send INITIAL-WORLD-TARGET-UNSELECTED
                                                      after-key-event
                                                      NOT-VALID-KEYEVENT))

(define INITIAL-WORLD-AFTER-TICK (send INITIAL-WORLD after-tick))

(define INITIAL-WORLD-AFTER-TICK-WITH-TOY
  (send INITIAL-WORLD-TARGET-SQUARE after-tick))

(define INITIAL-WORLD-AFTER-MOUSE-DOWN-WITH-TOY
  (send INITIAL-WORLD-TARGET-SQUARE after-mouse-event 250 300 MOUSE-BUTTON-DOWN))

(define INITIAL-WORLD-AFTER-MOUSE-UP-WITH-TOY
  (send INITIAL-WORLD-TARGET-SQUARE after-mouse-event 250 250 MOUSE-BUTTON-UP))


(define INITIAL-WORLD-SQUARE-DRAG (send INITIAL-WORLD-TARGET-SQUARE after-mouse-event
                                        250
                                        250
                                        MOUSE-DRAG))

(define TARGET1 (new Target% [x 100] [y 100]
                     [selected? false]
                     [saved-mx 0]
                     [saved-my 0]))

(define INITIAL-TARGET (new Target% [x TARGET-INITIAL-X1] [y TARGET-INITIAL-Y1]
                            [selected? false]
                            [saved-mx 0]
                            [saved-my 0]))


(define TRAGET1-AFTER-KEYEVENT (send TARGET1 after-key-event
                                     ADD-CLOCK-TOY-KEYEVEMT))

(define SCENE-AFTER-INITIAL-WORLD (send INITIAL-WORLD to-scene))

(define IMAGE-OF-SCENE-AFTER-INITIAL-WORLD (place-image
                                            (send INITIAL-TARGET add-to-scene EMPTY-CANVAS)
                                            TARGET-INITIAL-X1
                                            TARGET-INITIAL-Y1
                                            EMPTY-CANVAS))

(define SQUARE9 (new SquareToy% [x TARGET-INITIAL-X1]
                     [y TARGET-INITIAL-Y1] 
                     [selected? true]
                     [off-x 0]
                     [off-y 0]
                     [speed 15]))

(define WORLD-WITH-TARGET-SQUARE (send INITIAL-WORLD after-key-event
                                       ADD-SQUARE-TOY-KEYEVEMT))

(define SCENE-AFTER-TARGET-SQUARE (send WORLD-WITH-TARGET-SQUARE to-scene))

(define WORLD-SCENE-WITH-TARGET-SQUARE (send SQUARE9 add-to-scene
                                             IMAGE-OF-SCENE-AFTER-INITIAL-WORLD))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(begin-for-test
  
  (check-equal? (send INITIAL-WORLD-TARGET-SELECTED target-selected?) true
                "the target should be selected")
  
  (check-equal? (send INITIAL-WORLD-TARGET-DRAGGED target-x) 250 
                "the target should be dragged")
  
  (check-equal? (send INITIAL-WORLD-TARGET-DRAGGED target-y) 250 
                "the target should be dragged")
  
  (check-equal? (send INITIAL-WORLD-TARGET-UNSELECTED target-selected?) false 
                "the target should be unselected")
  
  (check-equal? (send INITIAL-WORLD-TARGET-REMAINS-UNSELECTED target-x) TARGET-INITIAL-X1 
                "the target should have initial coordinates")
  
  (check-equal? (send INITIAL-WORLD-INVALID-MOUSE-EVENT target-x) TARGET-INITIAL-X1
                "the target remains unchanged after an invalid mouse event")
  
  (check-equal? (send INITIAL-WORLD-TARGET-NOT-DRAGGED target-x) TARGET-INITIAL-X1 
                "the target should have initial coordinates")
  
  (check-equal? (send INITIAL-WORLD-TARGET-SQUARE target-x) TARGET-INITIAL-X1 
                "a square should have initial coordinates")
  
  (check-equal? (send INITIAL-WORLD-TARGET-THROBBER target-x) TARGET-INITIAL-X1 
                "a throbber should be created at the target's center")
  
  (check-equal? (send INITIAL-WORLD-TARGET-CLOCK target-x) TARGET-INITIAL-X1 
                "a clock should be created at the target's center")
  
  (check-equal? (send INITIAL-WORLD-TARGET-FOOTBALL target-x) TARGET-INITIAL-X1 
                "a football should be created at the target's center")
  
  (check-equal? (send INITIAL-WORLD-AFTER-TICK target-x) TARGET-INITIAL-X1 
                "the initial world should not change after tick")
  
  (check-equal? (send INITIAL-WORLD-AFTER-TICK-WITH-TOY target-x) TARGET-INITIAL-X1 
                "the target shouldnot change its center after tick")
  
  (check-equal? (send INITIAL-WORLD-AFTER-MOUSE-DOWN-WITH-TOY target-selected?) true 
                "the target should get selected after mouse-down")
  
  (check-equal? (send INITIAL-WORLD-AFTER-MOUSE-UP-WITH-TOY target-selected?) false
                "the target should get unselected after mouse-up")
  
  (check-equal? (send INITIAL-WORLD-TARGET-NOT-VALID-KEYEVENT target-x) TARGET-INITIAL-X1  
                "the target doesnot respond to KeyEvents")
  
  (check-equal? (send INITIAL-WORLD-SQUARE-DRAG target-y) 300 
                "the target should be dragged")
  
  (check-equal? SCENE-AFTER-INITIAL-WORLD IMAGE-OF-SCENE-AFTER-INITIAL-WORLD
                "There is one target in the center of the canvas initially")
  
  (check-equal? SCENE-AFTER-TARGET-SQUARE WORLD-SCENE-WITH-TARGET-SQUARE
                "There is one target in the center of the canvas initially")
  
  #;(check-equal? (send TARGET1 target-y) 250 
                  "the target should be dragged")
  #;(check-equal? (send TRAGET1-AFTER-KEYEVENT after-key-event) 250 
                  "the target should be dragged"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test cases for square toy

(define WORLD-WITH-S1 (send INITIAL-WORLD-TARGET-SELECTED after-key-event
                            "s"))

(define SQUARE1 (send WORLD-WITH-S1 get-toys))

(define SQUARE2 (new SquareToy% [x 250]
                     [y 250] 
                     [selected? true]
                     [off-x 0]
                     [off-y 0]
                     [speed 15]))

(define SQUARE3 (new SquareToy% [x (- CANVAS-WIDTH 1)] 
                     [y TARGET-INITIAL-Y1]
                     [selected? false]
                     [off-x 0]
                     [off-y 0]
                     [speed 15]))

(define SQUARE4 (new SquareToy% [x (- CANVAS-WIDTH (- CANVAS-WIDTH 1))]
                     [y TARGET-INITIAL-Y1]
                     [selected? false]
                     [off-x 0]
                     [off-y 0]
                     [speed 15]))

(define SQUARE-SELECTED (send (first SQUARE1) after-button-down
                              360
                              360))

(define SQUARE-AFTER-TICK (send (first SQUARE1) after-tick))
(define SQUARE-AT-MAX-BOUN-AFTER-TICK (send SQUARE3 after-tick))
(define SQUARE-AT-MIN-BOUN-AFTER-TICK (send SQUARE4 after-tick))
(define SELECTED-SQUARE-AFTER-TICK (send SQUARE2 after-tick))

(define SQUARE-DRAGGED (send SQUARE2 after-drag
                             360
                             360))

(define SQUARE-UNSELECTED (send SQUARE-SELECTED after-button-up
                                100 100))

(define SQUARE-AFTER-KEYEVENT (send SQUARE-SELECTED after-key-event
                                    ADD-SQUARE-TOY-KEYEVEMT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin-for-test
  (check-equal? (send (first SQUARE1) toy-x) TARGET-INITIAL-X1
                "Square toy should have the initial target coordinates")
  
  (check-equal? (send (first SQUARE1) toy-y) TARGET-INITIAL-Y1
                "Square toy should have the initial target coordinates")
  
  (check-equal? (send SQUARE-AFTER-TICK toy-y) TARGET-INITIAL-Y1
                "Square toy should have the initial target coordinates")
  
  (check-equal? (send SQUARE-AFTER-TICK toy-x) (+ TARGET-INITIAL-X1 10)
                "the square toy should move right by its given speed")
  
  (check-equal? (send SELECTED-SQUARE-AFTER-TICK toy-x) 250
                "selected square toy shouldnot move")
  
  (check-equal? (send SQUARE-AT-MIN-BOUN-AFTER-TICK toy-x)
                (send SQUARE-AT-MIN-BOUN-AFTER-TICK for-test:min-boundary)
                "square toy should bounce back")
  
  (check-equal? (send SQUARE-AT-MAX-BOUN-AFTER-TICK toy-x)
                (send SQUARE-AT-MAX-BOUN-AFTER-TICK for-test:max-boundary)
                "square toy should bounce back")
  
  (check-equal? (send SQUARE-AFTER-TICK toy-data) 10 
                "speed should stay constant")
  
  (check-equal? (send SQUARE-UNSELECTED toy-x) TARGET-INITIAL-X1
                "Square toy should have the initial target coordinates")
  
  (check-equal? (send SQUARE-AFTER-KEYEVENT toy-x) TARGET-INITIAL-X1
                "Square toy doesnot respond to keyevent"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test cases for throbber toy

(define THROBBER1 (send INITIAL-WORLD-TARGET-THROBBER get-toys))

(define THROBBER1-SELECTED (send (first THROBBER1) after-button-down
                                 TARGET-INITIAL-X1
                                 TARGET-INITIAL-Y1))

(define THROBBER1-NOT-SELECTED (send (first THROBBER1) after-button-down
                                     400
                                     400))

(define THROBBER1-AFTER-TICK (send (first THROBBER1) after-tick))

(define THROBBER1-DRAGGED (send THROBBER1-SELECTED after-drag
                                360
                                360))

(define UNSEL-THROBBER1-DRAGGED (send THROBBER1-NOT-SELECTED after-drag
                                      360
                                      360))

(define THROBBER1-UNSELECTED (send THROBBER1-SELECTED after-button-up
                                   100 100))

(define THROBBER1-AFTER-KEYEVENT (send THROBBER1-SELECTED after-key-event
                                       ADD-SQUARE-TOY-KEYEVEMT))

(define THROBBER-AT-15-TICKS   ; radius is 20
  (send(send(send
             (send(send(send
                        (send(send(send
                                   (send(send(send
                                              (send (send
                                                     THROBBER1-AFTER-TICK
                                                     after-tick)
                                                    after-tick)
                                              after-tick)
                                             after-tick)
                                        after-tick)
                                   after-tick)
                                  after-tick)
                             after-tick)
                        after-tick)
                       after-tick)
                  after-tick)
             after-tick)
            after-tick)
       after-tick)) 

(define THROBBER-AT-30-TICKS
  (send (send(send(send
                   (send(send(send
                              (send(send(send
                                         (send(send(send
                                                    (send (send
                                                           THROBBER-AT-15-TICKS
                                                           after-tick)
                                                          after-tick)
                                                    after-tick)
                                                   after-tick)
                                              after-tick)
                                         after-tick)
                                        after-tick)
                                   after-tick)
                              after-tick)
                             after-tick)
                        after-tick)
                   after-tick)
                  after-tick)
             after-tick)
        after-tick))

(define THROBBER2 (new ThrobberToy%
                       [x TARGET-INITIAL-X1] [y TARGET-INITIAL-Y1]
                       [selected? false]
                       [off-x 0]
                       [off-y 0]
                       [r 5]
                       [r-speed 1]))

(define WORLD-WITH-TARGET-THROBBER (send INITIAL-WORLD after-key-event
                                         ADD-THROBBER-TOY-KEYEVEMT))

(define SCENE-AFTER-TARGET-THROBBER (send WORLD-WITH-TARGET-THROBBER to-scene))

(define WORLD-SCENE-WITH-TARGET-THROBBER (send THROBBER2 add-to-scene
                                               IMAGE-OF-SCENE-AFTER-INITIAL-WORLD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal? (send (first THROBBER1) toy-x) TARGET-INITIAL-X1
                "Throbber should have the initial target coordinates")
  
  (check-equal? (send (first THROBBER1) toy-y) TARGET-INITIAL-Y1
                "Throbber should have the initial target coordinates")
  
  (check-equal? (send THROBBER1-AFTER-TICK toy-y) TARGET-INITIAL-Y1
                "Throbber shouldnot change its initial coordinates")
  
  (check-equal? (send (first THROBBER1) toy-data) 5  
                "Throbber should have the initial radius")
  
  (check-equal? (send THROBBER1-UNSELECTED toy-y) TARGET-INITIAL-Y1
                "a unselected throbber cannot be selected")
  
  (check-equal? (send THROBBER1-SELECTED toy-y) TARGET-INITIAL-Y1
                "Throbber should have the initial target coordinates")
  
  (check-equal? (send THROBBER1-AFTER-KEYEVENT toy-y) TARGET-INITIAL-Y1 
                "the throbber doesnot repond to keyevents")
  
  (check-equal? (send THROBBER1-NOT-SELECTED toy-x) TARGET-INITIAL-X1
                "the throbber should change its coordinates")
  
  (check-equal? (send UNSEL-THROBBER1-DRAGGED toy-x) TARGET-INITIAL-X1
                "Unselected throbber cannot be dragged")
  
  (check-equal? (send THROBBER-AT-15-TICKS toy-data) MAX-T-RADIUS1
                "the radius should be MAX-T-RADIUS")
  
  (check-equal? (send THROBBER-AT-30-TICKS toy-data) MIN-T-RADIUS1
                "the radius should be MIN-T-RADIUS")
  
  (check-equal? (send (send THROBBER-AT-30-TICKS after-tick) toy-data)
                (+ MIN-T-RADIUS1 1)
                "the radius of throbe should be one more than MIN-T-RADIUS")
  
  (check-equal? (send (send THROBBER-AT-15-TICKS after-tick) toy-data)
                (- MAX-T-RADIUS1 1)
                "the radius of throbe should be one less than MAX-T-RADIUS")
  
  (check-equal? (send (send (send THROBBER-AT-30-TICKS after-tick) after-tick)
                      toy-data) 7
                                "the radius of throbe should be two more than MIN-T-RADIUS")
  
  (check-equal? SCENE-AFTER-TARGET-THROBBER WORLD-SCENE-WITH-TARGET-THROBBER
                "There is one target in the center of the canvas initially"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; test cases for ClockToy

(define CLOCK1 (send INITIAL-WORLD-TARGET-CLOCK get-toys))

(define CLOCK1-SELECTED (send (first CLOCK1) after-button-down
                              TARGET-INITIAL-X1
                              TARGET-INITIAL-Y1))

(define CLOCK1-NOT-SELECTED (send (first CLOCK1) after-button-down
                                  400
                                  400))

(define CLOCK1-AFTER-TICK (send (first CLOCK1) after-tick))

(define CLOCK1-DRAGGED (send CLOCK1-SELECTED after-drag
                             360
                             360))

(define UNSEL-CLOCK1-DRAGGED (send CLOCK1-NOT-SELECTED after-drag 360 360))

(define CLOCK1-UNSELECTED (send CLOCK1-SELECTED after-button-up
                                100 100))

(define CLOCK1-AFTER-KEYEVENT (send CLOCK1-SELECTED after-key-event
                                    ADD-CLOCK-TOY-KEYEVEMT))

(define CLOCK2 (new ClockToy%
                    [x TARGET-INITIAL-X1] [y TARGET-INITIAL-Y1]
                    [selected? false]
                    [off-x 0]
                    [off-y 0]
                    [c-number 0]))
(define WORLD-WITH-TARGET-CLOCK (send INITIAL-WORLD after-key-event
                                      ADD-CLOCK-TOY-KEYEVEMT))

(define SCENE-AFTER-TARGET-CLOCK (send WORLD-WITH-TARGET-CLOCK to-scene))

(define WORLD-SCENE-WITH-TARGET-CLOCK (send CLOCK2 add-to-scene
                                            IMAGE-OF-SCENE-AFTER-INITIAL-WORLD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  
  (check-equal? (send (first CLOCK1) toy-x) TARGET-INITIAL-X1
                "Clock should have the coordinates same as those of initial target")
  
  (check-equal? (send (first CLOCK1) toy-y) TARGET-INITIAL-Y1
                "Clock should have the coordinates same as those of initial target")
  
  (check-equal? (send CLOCK1-AFTER-TICK toy-y) TARGET-INITIAL-Y1
                "Clock should have the coordinates same as those of initial target")
  
  (check-equal? (send (first CLOCK1) toy-data) 0 
                "Clock should have the initial data as 0")
  
  (check-equal? (send (send (first CLOCK1) after-tick) toy-data) 1
                "Clock should have the initial data as 1")
  
  (check-equal? (send CLOCK1-UNSELECTED toy-x) TARGET-INITIAL-X1
                "Clock should have the coordinates same as those of initial target")
  
  (check-equal? (send CLOCK1-AFTER-KEYEVENT toy-y) TARGET-INITIAL-Y1
                "Clock doesnot respond to KeyEvent")
  
  (check-equal? (send UNSEL-CLOCK1-DRAGGED toy-y) TARGET-INITIAL-Y1
                "a unselected clock cannot be dragged")
  
  (check-equal? SCENE-AFTER-TARGET-CLOCK WORLD-SCENE-WITH-TARGET-CLOCK
                "There is one target in the center of the canvas initially"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; test cases for football toy

(define FOOTBALL1 (send INITIAL-WORLD-TARGET-FOOTBALL get-toys))

(define FOOTBALL2 (new FootballToy%
                       [x TARGET-INITIAL-X1]
                       [y TARGET-INITIAL-Y1]
                       [selected? false]
                       [off-x 0]
                       [off-y 0]
                       [scale-val 1]))

(define FOOTBALL1-SELECTED (send (first FOOTBALL1) after-button-down
                                 TARGET-INITIAL-X1
                                 TARGET-INITIAL-Y1))

(define FOOTBALL1-NOT-SELECTED (send (first FOOTBALL1) after-button-down
                                     400
                                     400))

(define FOOTBALL1-AFTER-TICK (send (first FOOTBALL1) after-tick))

(define FOOTBALL2-AFTER-TICK (send FOOTBALL2 after-tick))

(define FOOTBALL1-DRAGGED (send FOOTBALL1-SELECTED after-drag
                                360
                                360))
(define UNSEL-FOOTBALL1-DRAGGED (send FOOTBALL1-NOT-SELECTED after-drag 360 360))

(define FOOTBALL1-UNSELECTED (send FOOTBALL1-SELECTED after-button-up
                                   100 100))

(define FOOTBALL1-AFTER-KEYEVENT (send FOOTBALL1-SELECTED after-key-event
                                       ADD-FOOTBALL-TOY-KEYEVEMT))

(define FOOTBALL-IMAGE (bitmap "football.png"))

(define FOOTBALL3 (new FootballToy%
                       [x TARGET-INITIAL-X1]
                       [y TARGET-INITIAL-Y1]
                       [selected? false]
                       [off-x 0]
                       [off-y 0]
                       [scale-val 100]))
(define WORLD-WITH-TARGET-FOOTBALL (send INITIAL-WORLD after-key-event
                                         ADD-FOOTBALL-TOY-KEYEVEMT))

(define SCENE-AFTER-TARGET-FOOTBALL (send WORLD-WITH-TARGET-FOOTBALL to-scene))

(define WORLD-SCENE-WITH-TARGET-FOOTBALL (send FOOTBALL3 add-to-scene
                                               IMAGE-OF-SCENE-AFTER-INITIAL-WORLD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  
  (check-equal? (send (first FOOTBALL1) toy-x) TARGET-INITIAL-X1
                "Football should have coordinates same as the initial target")
  
  (check-equal? (send (first FOOTBALL1) toy-y) TARGET-INITIAL-Y1
                "Football should have coordinates same as the initial target")
  
  (check-equal? (send FOOTBALL1-AFTER-TICK toy-y) TARGET-INITIAL-Y1
                "Throbber should have the initial target coordinates") 
  
  (check-equal? (send FOOTBALL2-AFTER-TICK toy-data) 0
                "the size of the football should be zero")
  
  (check-equal? (send FOOTBALL1-UNSELECTED toy-x) TARGET-INITIAL-X1
                "Football should have coordinates same as the initial target")
  
  (check-equal? (send FOOTBALL1-AFTER-KEYEVENT toy-y) TARGET-INITIAL-Y1
                "Football doesnot respond to keyevents")
  
  (check-equal? (send UNSEL-FOOTBALL1-DRAGGED toy-y) TARGET-INITIAL-Y1
                "Unselected football cannot be selected")
  
  (check-equal? SCENE-AFTER-TARGET-FOOTBALL WORLD-SCENE-WITH-TARGET-FOOTBALL
                "There is one target in the center of the canvas initially"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Testing Framework for equality of two instances of same class 

; checking equality of target instances

(define (target-equal? tar1 tar2)
  (and
   (= 
    (send tar1 target-x)
    (send tar2 target-x))
   (=
    (send tar1 target-y)
    (send tar2 target-y))
   
   (equal?
    (send tar1 for-test:selected?)
    (send tar2 for-test:selected?))
   
   (=
    (send tar1 for-test:saved-mx)
    (send tar2 for-test:saved-mx))
   (=
    (send tar1 for-test:saved-my)
    (send tar2 for-test:saved-my))))


(begin-for-test 
  (local
    ((define t1 (new Target% [x TARGET-INITIAL-X1][y TARGET-INITIAL-Y1])))
    (check target-equal? 
           (send t1 after-tick)
           t1
           "both the objects should be same")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; checking equality of square-toy instances

(define (square-equal? s1 s2)
  (and
   (=  
    (send s1 toy-x)
    (send s2 toy-x))
   (=
    (send s1 toy-y)
    (send s2 toy-y))
   (=
    (send s1 toy-data)
    (send s2 toy-data))
   (equal?
    (send s1 for-test:selected?)
    (send s2 for-test:selected?))
   (=
    (send s1 for-test:off-x)
    (send s2 for-test:off-x))
   (=
    (send s1 for-test:off-y)
    (send s2 for-test:off-y))))


(begin-for-test 
  (local
    ((define s1 (new SquareToy% [x TARGET-INITIAL-X1][y TARGET-INITIAL-Y1]
                     [speed 15])))
    (check square-equal? 
           (send s1 after-tick)
           (new SquareToy% [x (+ 15 TARGET-INITIAL-X1)][y TARGET-INITIAL-Y1]
                [speed 15]) 
           "both the objects should be same")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; checking equality of clock toy instances

(define (clock-equal? c1 c2)
  (and
   (= 
    (send c1 toy-x)
    (send c2 toy-x))
   (=
    (send c1 toy-y)
    (send c2 toy-y))
   (=
    (send c1 toy-data)
    (send c2 toy-data))
   (equal?
    (send c1 for-test:selected?)
    (send c2 for-test:selected?))
   (=
    (send c1 for-test:off-x)
    (send c2 for-test:off-x))
   (=
    (send c1 for-test:off-y)
    (send c2 for-test:off-y))))


(begin-for-test 
  (local
    ((define c1 (new ClockToy% [x TARGET-INITIAL-X1][y TARGET-INITIAL-Y1])))
    (check clock-equal? 
           (send c1 after-tick)
           (new ClockToy% [x TARGET-INITIAL-X1][y TARGET-INITIAL-Y1] [c-number 1]) 
           "both the objects should be same")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; checking equality of throbber toy instances

(define (throbber-equal? t1 t2)
  (and
   (=(send t1 toy-x)
     (send t2 toy-x))
   (=(send t1 toy-y)
     (send t2 toy-y))
   (equal?
    (send t1 for-test:selected?) 
    (send t2 for-test:selected?))
   (= (send t1 for-test:off-x)
      (send t2 for-test:off-x))
   (= (send t1 for-test:off-y)
      (send t2 for-test:off-y))
   (= (send t1 toy-data) (send t2 toy-data))))

(begin-for-test
  (local
    ((define THROBBER3 (new ThrobberToy%
                            [x TARGET-INITIAL-X1] [y TARGET-INITIAL-Y1]
                            [r 10]))) 
    (check throbber-equal? (send THROBBER3 after-tick)
           (new ThrobberToy%
                [x TARGET-INITIAL-X1] [y TARGET-INITIAL-Y1]
                [r 11]) "both the objects should be same")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; checking equality of football toy instances

(define (football-equal? f1 f2)
  (and
   (=(send f1 toy-x) (send f2 toy-x))
   (=(send f1 toy-y) (send f2 toy-y))
   (equal? (send f1 for-test:selected?) (send f2 for-test:selected?))
   (= (send f1 for-test:off-x) (send f2 for-test:off-x))
   (=(send f1 for-test:off-y) (send f2 for-test:off-y))
   (=(send f1 for-test:scale-val) (send f2 for-test:scale-val))))

(begin-for-test
  (local
    ((define FOOTBALL5 (new FootballToy%
                            [x TARGET-INITIAL-X1]
                            [y TARGET-INITIAL-Y1]
                            [scale-val 100])))
    
    (check football-equal? (send FOOTBALL5 after-tick)
           (new FootballToy%
                [x TARGET-INITIAL-X1]
                [y TARGET-INITIAL-Y1]
                [scale-val 99]) "both the objects should be same")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
