;;;;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 10 : QUESTION 01;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

#lang racket

(check-location "10" "toys.rkt")

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "WidgetWorks.rkt")

(provide
 run
 make-world
 make-playground
 make-square-toy
 make-throbber
 make-clock
 make-football
 make-target
 Toy<%>
 PlaygroundState<%>
 Target<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GLOBAL CONSTANTS :

;;Dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

;;Scene of an empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;Initial Target position
(define INIT-TARGET-X (/ CANVAS-WIDTH 2))
(define INIT-TARGET-Y (/ CANVAS-HEIGHT 2))

;;Constants for KeyEvents
(define ADD-SQUARE-TOY-KEYEVEMT "s")
(define ADD-THROBBER-TOY-KEYEVEMT "t")
(define ADD-CLOCK-TOY-KEYEVEMT "w")
(define ADD-FOOTBALL-TOY-KEYEVEMT "f")
(define NOT-VALID-KEYEVENT "g")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERFACES: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The PlaygroundState<%> interface:

; PlaygroundState<%> implements all the functions of SWidget<%>
; PlaygroundState represensts a canvas which has the target and the toys(if any)

(define PlaygroundState<%>
  (interface(SWidget<%>) 
    
    ;; -> Integer
    ;; RETURN : the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; Is the target selected?
    target-selected?
    
    ;; -> ListOfToy<%>
    get-toys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Toy<%> interface:

; Toy<%> implements all the functions of SWidget<%>
; A Toy represents any live object in the world

(define Toy<%>
  (interface(SWidget<%>)
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Target<%> interface:

; Target<%> implements all the functions of the SWidget<%>
; Target<%> represents the whereabouts of a target in the world

(define Target<%>
  (interface(SWidget<%>)
    
    ; -> Int
    ; RETURNS: the x or y position of the center of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; RETURNS: whether or not the target is selected
    target-selected?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN FUNCTIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; run     : PosNum PosInt -> Void
; GIVEN   : the frame rate (in seconds/tick) and the speed of the square toy
;           (in pixels/tick) that will create and run the world with the square toys
;           travelling horizontally with the given speed
; EFFECT  : runs the simulation of world

(define (run rate speed)
  (local
    ((define world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define playground (make-playground speed)))
    (begin
      (send world add-stateful-widget playground)
      (send world run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-playground : PosInt -> PlaygroundState<%>
; GIVEN      : a PosInt representing the speed of the square toy, in pixels/tick
; RETURNS    : an instance of PlaygroundState% class with a target, but no toys, and in 
;              which any square toys created in the future will travel at the given
;              speed (in pixels/tick)
; STRATEGY   : Create an instance for the class PlaygroundState%

(define (make-playground speed)
  (new PlaygroundState% [sqr-speed speed] [toys empty] [target (make-target)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-target : -> Target<%>
; RETURNS    : an instance of Target% class at the center of the canvas in unselected mode
; WHERE      : the position of this target provides initial position of any toy that is 
;              created in the world
; STRATEGY   : Create an instance for the class Target%

(define (make-target)
  (new Target%
       [x INIT-TARGET-X]
       [y INIT-TARGET-Y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CLASSES:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Target% class

; A Target is a (new Target% [x INIT-TARGET-X]
;                            [y INIT-TARGET-Y]
;                            [selected? Boolean]
;                            [saved-mx Integer]
;                            [saved-my Integer])

; A Target represents a location on canvas where new toys are created.
; The Target% class implemets the Target<%> interface.

(define Target%
  (class* object% (Target<%>)
    
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
    (field [TARGET-MODE "outline"])
    ; Represents the target image
    (field [TARGET-IMAGE (TARGET-SHAPE TARGET-RADIUS TARGET-MODE TARGET-COLOR)])
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for Target
    ; ------------------
    
    ; after-tick : -> Void
    ; GIVEN      : No aruguements
    ; EFFECT     : updates this object to a state that should follow after a tick
    ; DETAILS    : target ignores after-tick event
    ; STRATEGY   : make no changes to the given object 
    
    (define/public (after-tick) 
      this)
    
    ;......................................................................    
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN       : the x and y coordinate of the mouse click
    ; EFFECT      : updates the target after the MOUSE-BUTTON-DOWN event
    ; DETAILS     : selects the target object if the MOUSE-BUTTON-DOWN event is within the
    ;               target
    ; STRATEGY    : Cases on in-target? 
    
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN           : the x and y coordinate of the mouse click
    ; EFFECT          : updates the target after the MOUSE-BUTTON-UP event
    ; STRATEGY        : updates the original instance of class Target%
    
    (define/public (after-button-up mx my) 
      (begin
        (set! selected? false)
        (set! saved-mx 0)
        (set! saved-my 0)))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the target after the MOUSE-DRAG event
    ; DETAILS    : selects the target object and evaluates its x and y coordinates at 
    ;              every instance of time if the MOUSE-DRAG event is within the target
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN      : a KeyEvent
    ; EFFECT     : updates this object to a state that should follow after the given
    ;              KeyEvent
    ; DETAILS    : target ignores after-key-event event
    ; STRATEGY   : make no changes to the given object  
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;The PlaygroundState% class

; A PlaygroundState is a (new PlaygroundState% [sqr-speed PosInt]
;                                              [toys empty]
;                                              [target Target<%>])

; The playgroundstate represents the state of the playground at an instance of time.
; It handles the target and the toys being cretaed in the world.
; The PlaygroundState implements the PlaygroundState<%> interface.

(define PlaygroundState%
  (class* object% (PlaygroundState<%>)
    
    ; Fields for PlaygroundState
    ; --------------------------    
    (init-field sqr-speed toys target)
    
    ; INTERPRETATION :
    ; sqr-speed : the speed of the square toys that may be created in the world
    ; toys      : the list of toys present in the world
    ; target    : the target in the world upon which new toys are created
    
    (super-new)
    
    ;......................................................................
    
    ; after-tick : -> Void
    ; GIVEN      : no arguments
    ; EFFECT     : updates the state of the playground after a tick
    ; STRATEGY   : Use HOF map on LOT 
    
    (define/public (after-tick)
      (begin
        (send target after-tick) 
        (map
         ; Toy<%> -> Void
         ; GIVEN   : an object of a class that implements the Toy<%> interface
         ; EFFECT  : updates the given object after a tick
         (lambda (toy) (send toy after-tick))
         toys)))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN       : the x and y coordinate of the mouse click
    ; EFFECT      : updates the state of the playground after the MOUSE-BUTTON-DOWN event
    ; STRATEGY    : Use HOF map on LOT 
    
    (define/public (after-button-down mx my)
      (begin
        (send target after-button-down mx my) 
        (map
         ; Toy<%> -> Void
         ; GIVEN   : an object of a class that implements the Toy<%> interface
         ; EFFECT  : updates the given object after the MOUSE-BUTTON-DOWN event
         (lambda (toy) (send toy after-button-down mx my))
         toys)))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN           : the x and y coordinate of the mouse click
    ; EFFECT          : updates the state of the playground after the MOUSE-BUTTON-UP event
    ; STRATEGY        : Use HOF map on LOT 
    
    (define/public (after-button-up mx my)
      (begin
        (send target after-button-down mx my) 
        (map
         ; Toy<%> -> Void
         ; GIVEN   : an object of a class that implements the Toy<%> interface
         ; EFFECT  : updates the given object after the MOUSE-BUTTON-UP event
         (lambda (toy) (send toy after-button-up mx my))
         toys)))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the state of the playground after the MOUSE-DRAG event
    ; STRATEGY   : Use HOF map on LOT 
    
    (define/public (after-drag mx my)
      (begin
        (send target after-button-down mx my)
        (map
         ; Toy<%> -> Void
         ; GIVEN   : an object of a class that implements the Toy<%> interface
         ; EFFECT  : updates the given object after the MOUSE-BUTTON-DOWN event
         (lambda (toy) (send toy after-drag mx my))
         toys)))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN      : a KeyEvent
    ; EFFECT     : updates the state of the playground after the given KeyEvent
    ; STRATEGY   : Cases on KeyEvent 
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev ADD-SQUARE-TOY-KEYEVEMT)
         (set! toys (cons (make-square-toy (target-x) (target-y) sqr-speed) toys))]
        
        [(key=? kev ADD-THROBBER-TOY-KEYEVEMT)
         (set! toys (cons (make-throbber (target-x) (target-y)) toys))]
        
        [(key=? kev ADD-CLOCK-TOY-KEYEVEMT)
         (set! toys (cons (make-clock (target-x) (target-y)) toys))]
        
        [(key=? kev ADD-FOOTBALL-TOY-KEYEVEMT)
         (set! toys (cons (make-football (target-x) (target-y)) toys))]
        
        [else this]))
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with all the elements of the playground
    ;            painted on it
    ; STRATEGY : Use HOF foldr on target and toys in this playground 
    
    (define/public (add-to-scene scene)
      (local
        ((define new-scene (send target add-to-scene scene)))
        (foldr
         ; Toy<%> Scene -> Scene 
         ; GIVEN : an object of a class that implements the Toy<%> interface and a scene
         ; WHERE : the scene already contains the target of this playground
         ; RETURNS : the scene like the given one but with the given toy painted on it
         (lambda (toy s) (send toy add-to-scene s))
         new-scene
         toys)))
    
    ;......................................................................
    
    ; target-x : -> Integer
    ; RETURN : the x coordinate of the target of this playground
    (define/public (target-x)
      (send target target-x))
    
    ; target-y : -> Integer
    ; RETURN : the y coordinate of the target of this playground
    (define/public (target-y)
      (send target target-y))
    
    ; target-selected? : -> Boolean
    ; RETURN : the value of selected? field of target of this playground
    ;          (true, if it selected else false)
    (define/public (target-selected?)
      (send target target-selected?))
    
    ; get-toys : -> ListOfToy<%>
    ; RETURN : list of all toys present in the playground else returns empty
    (define/public (get-toys)
      toys)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-square-toy : PosInt PosInt PosInt -> Toy<%>
; GIVEN     : the x and y position where the square toy should be placed, and the speed
;             at which the sqaure toy should move
; RETURNS   : a new instance of class SquareToy% with given center and travelling 
;             right at the given speed.

(define (make-square-toy new-x new-y new-speed)
  (new SquareToy% [x new-x] [y new-y] [speed new-speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    
    ; after-tick : -> Void
    ; GIVEN      : No aruguements
    ; EFFECT     : updates the state of the square toy after a tick
    ; DETAILS    : halts the square toy if it is selected or else updates it
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-tick)  
      (if selected?
          this
          (local
            ((define old-x x)
             (define old-speed speed))
            (begin
              (set! x (update-coordinate old-speed)) 
              (set! speed (update-speed old-x))))))
    
    ;......................................................................
    
    ; update-coordinate : Int -> NonNegInt
    ; GIVEN   : the speed of the square toy before the tick
    ; RETURNS : the x coordinate of the square toy's center based on its speed
    
    (define (update-coordinate old-speed)
      (cond
        [(<= (+ x old-speed) S-BOUNDARY-MIN) S-BOUNDARY-MIN]
        [(>= (+ x old-speed) S-BOUNDARY-MAX) S-BOUNDARY-MAX]
        [else (+ x old-speed)]))
    
    ;......................................................................
    
    ; update-speed : NonNegInt -> Int
    ; GIVEN   : the x-coordinate of the square toy before the tick
    ; RETURNS : the speed of the square toy based on its current location
    
    (define (update-speed old-x)
      (if (or (<= (+ old-x speed) S-BOUNDARY-MIN) 
              (>= (+ old-x speed) S-BOUNDARY-MAX))
          (- 0 speed)
          speed))
    
    ;......................................................................
    
    ; in-square? : NonNegInt NonNegInt NonNegInt NonNegInt -> Boolean
    ; GIVEN      : the coordinate of the squaretoy's center and the coordinates
    ;              of the mouse event
    ; RETURNS    : true, iff the mouse click is within the SquareToy else false
    ; STRATEGY   : Combining simpler functions
    
    (define (in-square? x y mx my)
      (and
       (<= (- (toy-x)  S-BOUNDARY-MIN) mx (+ (toy-x) S-BOUNDARY-MIN))
       (<= (- (toy-y)  S-BOUNDARY-MIN) my (+ (toy-y) S-BOUNDARY-MIN))))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN     : the x and y coordinate of the mouse click
    ; EFFECT    : updates the SquareToy to the state should follow after the
    ;             MOUSE-BUTTON-DOWN event
    ; STRATEGY  : updates the original instance of class SquareToy%
    
    (define/public (after-button-down mx my)
      (begin 
        (set! selected? (in-square? x y mx my))
        (set! off-x (- mx x))
        (set! off-y (- my y))))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN     : the x and y coordinate of the mouse click
    ; EFFECT    : updates the SquareToy to the state should follow after the
    ;             MOUSE-BUTTON-UP event
    ; STRATEGY  : updates the original instance of class SquareToy%
    
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! off-x 0)
        (set! off-y 0)))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the SquareToy to the state should follow after the
    ;              MOUSE-DRAG event
    ; DETAILS    : updates the SqaureToy object if selected or returns a random value
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ mx off-x))
            (set! y (+ my off-y))
            (set! selected? true))
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Void
    ; EFFECT    : updates the SquareToy to the state should follow after the given key event
    ; DETAILS   : a SquareToy ignores key events
    ; STRATEGY  : make no changes to the given object  
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: NonNegInt NonNegInt -> Toy<%>
;; GIVEN    : x and y coordinates that represent a position
;; RETURNS  : an object representing a throbber at the given position

(define (make-throbber new-x new-y)
  (new ThrobberToy% [x new-x] [y new-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; A ThrobberToy is draggable and it does not respond to KeyEvents

(define ThrobberToy% 
  (class* object% (Toy<%>)
    
    ; Fields for ThrobberToy
    ; ----------------------
    
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
    
    
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for ThrobberToy
    ; -----------------------
    
    ; after-tick : -> Void
    ; GIVEN      : No aruguements
    ; EFFECT     : updates the state of the throbber toy after a tick
    ; STRATEGY   : updates the original instance of class ThrobberToy%
    
    (define/public (after-tick)
      (local
        ((define old-r r)
         (define old-r-speed r-speed))
        (begin
          (set! r (+ r (update-r-speed MIN-T-RADIUS MAX-T-RADIUS old-r old-r-speed)))
          (set! r-speed (update-r-speed MIN-T-RADIUS MAX-T-RADIUS old-r old-r-speed)))))
    
    ;......................................................................
    
    ; update-r-speed : PosInt PosInt PosInt Integer -> Integer
    ; GIVEN          : Predefined minimum and maximum values of the radius of a
    ;                  throbber toy, the current radius and the speed of the throbber
    ; RETURNS        : the value by which the radius should change
    ; STRATEGY       : Divide into cases based on whether current radius is greater
    ;                  than maximum radius or smaller than minimum radius or otherwise
    
    (define (update-r-speed min-r max-r r r-speed)
      (cond
        [(> (+ r-speed r) max-r) DEC-SPEED]  
        [(< (+ r-speed r) min-r) INC-SPEED]   
        [else r-speed]))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN    : the x and y coordinate of the mouse click
    ; EFFECT   : updates the ThrobberToy to a state that should follow after the
    ;            MOUSE-BUTTON-DOWN event
    ; DETAILS  : updates the ThrobberToy if the MOUSE-BUTTON-DOWN event is within the toy
    ;            or returns a random value
    ; STRATEGY : Cases on whether the MOUSE-BUTTON-DOWN is within the boundary
    ;            of the ThrobberToy 
    
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! selected? true)
            (set! off-x (- mx x))
            (set! off-y (- my y)))
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
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN    : the x and y coordinate of the mouse click
    ; EFFECT   : updates the ThrobberToy to a state that should follow after the
    ;            MOUSE-BUTTON-UP event
    ; STRATEGY : updates the original instance of class ThrobberToy%
    
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Void 
    ; GIVEN    : the x and y coordinate of the mouse click
    ; EFFECT   : updates the ThrobberToy to a state that should follow after the
    ;            MOUSE-DRAG event
    ; DEATIALS : updates the ThrobberToy if it is selected or returns a random value
    ; STRATEGY : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx off-x))
            (set! y (- my off-y))
            (set! selected? true))
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN    : a KetEvent 
    ; EFFECT   : updates the ThrobberToy to a state that should be after the given KeyEvent
    ; DETAILS  : a ThrobberToy ignores key events
    ; STRATEGY : make no changes to the given object 
    
    (define/public (after-key-event kev)
      this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with the ThrobberToy painted on it
    ; STRATEGY : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (place-image (circle r "solid" "green") x y scene))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clock : NonNegInt NonNegInt -> Toy<%>
;; GIVEN      : x and y coordinates that represent a position
;; RETURNS    : an object representing a clock at the given position.

(define (make-clock new-x new-y)
  (new ClockToy% [x new-x] [y new-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    
    ; after-tick : -> Void
    ; GIVEN      : No aruguements
    ; EFFECT     : updates the ClockToy object to a state that should follow after a tick
    ; STRATEGY   : updates the original instance of class ClockToy%
    
    (define/public (after-tick)  
      (set! c-number (+ c-number 1)))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the ClockToy object to a state that should follow the
    ;              MOUSE-BUTTON-DOWN event
    ; DETAILS    : updates the ClockToy object if the MOUSE-BUTTON-DOWN event is within
    ;              it's boundary or returns a random value
    ; STRATEGY   : Cases on whether the MOUSE-BUTTON-DOWN is within the
    ;              boundary of the clock
    
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (begin
            (set! selected? true)
            (set! off-x (- mx x))
            (set! off-y (- my y)))
          this))
    
    ;......................................................................
    
    ; in-clock? : NonNegInt NonNegInt -> Boolean
    ; GIVEN     : the x and y coordinate of the mouse click
    ; RETURNS   : true, iff the mouse click is within the ClockToy else false
    ; STRATEGY  : Combining simpler functions
    
    (define (in-clock? mx my)
      (local
        ((define TEXT-IMAGE (text (number->string c-number) 12 "black"))
         (define TEXT-IMAGE-WIDTH (image-width TEXT-IMAGE))
         (define TEXT-IMAGE-HEIGHT (image-height TEXT-IMAGE)))
        (and
         (<= (- (toy-x) TEXT-IMAGE-WIDTH) mx (+ (toy-x) TEXT-IMAGE-WIDTH))
         (<= (- (toy-y) TEXT-IMAGE-HEIGHT) my (+ (toy-y) TEXT-IMAGE-HEIGHT)))))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the ClockToy object to a value that should follow the
    ;              MOUSE-BUTTON-UP event
    ; STRATEGY   : updates the original instance of class ClockToy%                  
    
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Void 
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the ClockToy object to a state that should follow the
    ;              MOUSE-DRAG event
    ; DETAILS    : updates the ClockToy object if it is selected or else returns a random
    ;              value
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx off-x))
            (set! y (- my off-y))
            (set! selected? true))
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Void
    ; EFFECT   : updates the ClockToy object to a state that should follow the given
    ;            KeyEvent
    ; DETAILS  : a ClockToy ignores key events
    ; STRATEGY : make no changes to the given object
    
    (define/public (after-key-event kev)
      this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN    : a scene 
    ; RETURNS  : a Scene like the given one with the ClockToy painted on it
    ; DESIGN STRATEGY : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (place-image (text (number->string c-number) 12 "black") x y scene))
    
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
      off-y)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-football : PosInt PostInt -> Toy<%>
;;GIVEN     : an x and a y position
;;RETURNS   : an instance of class FootballToy% with its x and y set to the
;;            passed arguements

(define (make-football new-x new-y)
  (new FootballToy% [x new-x] [y new-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    
    
    ;......................................................................
    (super-new)
    ;......................................................................
    
    ; Methods for FootballToy
    ; ----------------------
    
    ; after-tick : -> Void
    ; GIVEN      : No aruguements
    ; EFFECT     : updates the FootballToy object to a state that should follow after a
    ;              tick
    ; STRATEGY   : updates the original instance of class FootballToy%
    
    (define/public (after-tick)   
      (set! scale-val (update-scale)))
    
    ;......................................................................
    
    ; update-scale : -> Boolean
    ; RETURNS : true iff the scale value reduces to 1 otherwise false      
    
    (define (update-scale)
      (if (= scale-val 1)
          scale-val
          (- scale-val 1)))
    
    ;......................................................................
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN             : the x and y coordinate of the mouse click
    ; EFFECT            : updates the FootballToy object to a state that should follow the
    ;                     MOUSE-BUTTON-DOWN event
    ; DETAILS           : updates the FootballToy object if the MOUSE-BUTTON-DOWN event is
    ;                     within the boundary of the object or else returns a random value 
    ; STRATEGY          : Cases on whether the MOUSE-BUTTON-DOWN is within
    ;                     the boundary of the FootballToy
    
    (define/public (after-button-down mx my)
      (if (in-football? mx my)
          (begin
            (set! selected? true)
            (set! off-x (- mx x))
            (set! off-y (- my y)))
          this))
    
    ;......................................................................
    
    ; in-football? : NonNegInt NonNegInt -> Boolean
    ; GIVEN        : the x and y coordinate of the mouse click
    ; RETURNS      : true, iff the mouse click is within the FootballToy else false
    ; STRATEGY     : Combining simpler functions
    
    (define (in-football? mx my)
      (local
        ((define FOOTBALL-IMAGE-NOW (scale (/ scale-val 100) FOOTBALL))
         (define FOOTBALL-WIDTH-NOW (image-width FOOTBALL-IMAGE-NOW))
         (define FOOTBALL-HEIGHT-NOW (image-height FOOTBALL-IMAGE-NOW)))
        (and
         (<= (- (toy-x) FOOTBALL-WIDTH-NOW) mx (+ (toy-x) FOOTBALL-WIDTH-NOW))
         (<= (- (toy-y) FOOTBALL-HEIGHT-NOW) my (+ (toy-y) FOOTBALL-HEIGHT-NOW)))))
    
    ;......................................................................
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN           : the x and y coordinate of the mouse click
    ; EFFECT          : updates the FootballToy object to a state that should follow the
    ;                   MOUSE-BUTTON-UP event
    ; DESIGN STRATEGY : updates the original instance of class FootballToy%     
    
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;......................................................................
    
    ; after-drag : NonNegInt NonNegInt -> Void 
    ; GIVEN      : the x and y coordinate of the mouse click
    ; EFFECT     : updates the FootballToy object to a state that should follow the
    ;              MOUSE-DRAG event
    ; DETAILS    : updates the FootballToy object if it is selected or returns a random
    ;              value
    ; STRATEGY   : Cases on selected?
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx off-x))
            (set! y (- my off-y))
            (set! selected? true))
          this))
    
    ;......................................................................
    
    ; after-key-event : KeyEvent -> Void
    ; EFFECT       : updates the FootballToy object to a state that should follow the
    ;                given key event
    ; DETAILS      : a FootballToy ignores key events
    ; STRATEGY     : make no changes to the given object
    
    (define/public (after-key-event kev) this)
    
    ;......................................................................
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN     : a scene 
    ; RETURNS   : a Scene like the given one with the FootballToy painted on it
    ; STRATEGY  : Combining simpler functions
    
    (define/public (add-to-scene scene)
      (local
        ((define FOOTBALL-IMAGE-NOW (scale (/ scale-val 100) FOOTBALL)))
        (place-image FOOTBALL-IMAGE-NOW x y scene)))
    
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
      (local
        ((define FOOTBALL-IMAGE-NOW (scale (/ scale-val 100) FOOTBALL))
         (define FOOTBALL-WIDTH-NOW (image-width FOOTBALL-IMAGE-NOW))
         (define FOOTBALL-HEIGHT-NOW (image-height FOOTBALL-IMAGE-NOW)))
        (* FOOTBALL-WIDTH-NOW FOOTBALL-HEIGHT-NOW)))
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing Framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;....................................................................................
;; Tests for Target
;....................................................................................

;; Constants for Target

; The initial coordinates of the target
(define TARGET-INITIAL-X1 (/ CANVAS-WIDTH 2)) 
(define TARGET-INITIAL-Y1 (/ CANVAS-HEIGHT 2))

;....................................................................................
; Testing after-tick on target

(begin-for-test
  (local
    ((define TG (make-target)))
    
    (send TG after-tick)
    
    (check-equal? (send TG target-x)
                  INIT-TARGET-X
                  "the target's x coordinate is not affected after tick as the target
                   ignores the after-tick event")
    (check-equal? (send TG target-y)
                  INIT-TARGET-Y
                  "the target's y coordinate is not affected after tick as the target
                   ignores the after-tick event")
    (check-equal? (send TG target-selected?)
                  false
                  "the initial tareget remains unselected after tick as the target
                   ignores the after-tick event")))

;....................................................................................
; Testing after-button-down on target

(begin-for-test
  (local
    ((define TG (make-target))
     (define TG-SEL (new Target%
                         [x INIT-TARGET-X]
                         [y INIT-TARGET-Y]
                         [selected? true]
                         [saved-mx 0]
                         [saved-my 0]))
     (define PG (make-playground 10)))
    
    (send TG after-button-down 1 1)
    (check-equal? (send TG for-test:selected?)
                  false
                  "The target remains unselected as the button-down is outside it's
                   boundary")
    
    (send TG after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send TG for-test:selected?)
                  true
                  "The target gets selected as the button-down is inside it's boundary")
    
    (send PG after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send TG for-test:selected?)
                  (send PG target-selected?)
                  "The target TG and the target in the playground are both selected
                   after the button-down event")))

;....................................................................................
; Testing after-drag on target

(begin-for-test
  (local
    ((define TG (make-target))
     (define TG-SEL (new Target%
                         [x INIT-TARGET-X]
                         [y INIT-TARGET-Y]
                         [selected? true]
                         [saved-mx 0]
                         [saved-my 0])))
    
    (send TG after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send TG target-x)
                  TARGET-INITIAL-X1
                  "As target TG is unselected,the drag event does not change it's
                   x-coordinate value")
    (check-equal? (send TG target-y)
                  TARGET-INITIAL-Y1
                  "As target TG is unselected,the drag event does not change it's
                   y-coordinate value")
    
    (send TG-SEL after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send TG-SEL target-x)
                  (+ TARGET-INITIAL-X1 5)
                  "As target TG is selected,the drag event changes it's x-coordinate value
                   to the one of the mouse coordinates")
    (check-equal? (send TG-SEL target-y)
                  (+ TARGET-INITIAL-Y1 5)
                  "As target TG is selected,the drag event changes it's y-coordinate value
                   to the one of the mouse coordinates")
    (check-equal? (send TG-SEL for-test:saved-mx)
                  0
                  "The offset of x-coordinate of mouse from center of target
                   remains constant while dragging")
    (check-equal? (send TG-SEL for-test:saved-my)
                  0
                  "The offset of x-coordinate of mouse from center of target
                   remains constant while dragging")))

;....................................................................................
; Testing after-button-up on target

(begin-for-test
  (local
    ((define TG-SEL (new Target%
                         [x INIT-TARGET-X]
                         [y INIT-TARGET-Y]
                         [selected? true]
                         [saved-mx 0]
                         [saved-my 0])))
    
    (send TG-SEL after-button-up TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send TG-SEL for-test:selected?)
                  false
                  "The target TG-SEL gets unselected after button-up mouse event")))

;....................................................................................
; Testing key-event and add-to-scene on target

(begin-for-test
  (local
    ((define TG (make-target)))
    
    (send TG after-key-event ADD-SQUARE-TOY-KEYEVEMT)
    (check-equal? (send TG target-x)
                  TARGET-INITIAL-X1
                  "The x-coordinate of the target remains unchanged after any key event
                   as target ignores all key events")
    
    (check-equal? (send TG add-to-scene EMPTY-CANVAS)
                  (place-image (circle 10 "outline" "red")
                               TARGET-INITIAL-X1 TARGET-INITIAL-Y1 EMPTY-CANVAS)
                  "Image of target is properly placed at the center of the canvas"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for SquareToy and PlaygroundState
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Conatants for Square
(define SQ-LEN 40)
(define SQ-MIN-BOUND (/ SQ-LEN 2))
(define SQ-MAX-BOUND (- CANVAS-WIDTH SQ-MIN-BOUND))
(define SQ-SPEED 10)


(begin-for-test
  (local
    ((define SQ (make-square-toy TARGET-INITIAL-X1 TARGET-INITIAL-Y1 SQ-SPEED))
     (define SQ-SEL (new SquareToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [speed SQ-SPEED]))
     (define SQ-AT-MAX-BOUND (make-square-toy SQ-MAX-BOUND TARGET-INITIAL-Y1 SQ-SPEED))
     (define SQ-AT-MIN-BOUND (make-square-toy SQ-MIN-BOUND TARGET-INITIAL-Y1 (- 0 SQ-SPEED)))
     (define PG (make-playground 10)))
    
    ;......................................................................
    ; testing after-tick on SquareToy and PlaygroundState
    
    (send SQ after-tick)
    (send SQ-SEL after-tick)
    (send SQ-AT-MIN-BOUND after-tick)
    (send SQ-AT-MAX-BOUND after-tick)
    
    (send PG after-key-event "s")
    (send PG after-tick)
    
    (check-equal? (send SQ toy-x)
                  (+ TARGET-INITIAL-X1 10)
                  "the unselected square toys's x coordinate is updated after tick")
    (check-equal? (send SQ-SEL toy-x)
                  TARGET-INITIAL-X1
                  "The value square toy object is not updated as it is selected")
    (check-equal? (send SQ-AT-MIN-BOUND toy-x)
                  (send SQ-AT-MIN-BOUND for-test:min-boundary)
                  "The x-coordinate of square toy is set to minimum boundary when it
                    collides to the west canvas wall")
    (check-equal? (send SQ-AT-MAX-BOUND toy-x)
                  (send SQ-AT-MAX-BOUND for-test:max-boundary)
                  "The x-coordinate of square toy is set to maximum boundary when it
                    collides to the east canvas wall")
    
    (check-equal? (send (first (send PG get-toys)) toy-x)
                  (send SQ toy-x)
                  "The x-coordinate of the playground's square toy created after its valid
                   corresponding KeyEvent is same as the square toy SQ")
    
    (send PG after-key-event "v")
    (send PG after-tick)
    (check-equal? (rest (send PG get-toys))
                  empty
                  "An invalied KeyEvent adds no toy to the playground")))

;......................................................................
; testing after-button-down on SquareToy and PlaygroundState

(begin-for-test
  (local
    ((define SQ (make-square-toy TARGET-INITIAL-X1 TARGET-INITIAL-Y1 SQ-SPEED))
     (define SQ-SEL (new SquareToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [speed SQ-SPEED]))
     (define PG (make-playground 10)))
    
    (send SQ after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send SQ for-test:selected?)
                  true
                  "The square toy is selected as the button-down is inside the toy")
    
    (send PG after-key-event "s")
    (send PG after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send (first (send PG get-toys)) for-test:selected?)
                  (send SQ for-test:selected?)
                  "The square toy of the playground and the square toy SQ both gets
                   selected after the button-down mouse event")))

;......................................................................
; testing after-drag on SquareToy and PlaygroundState

(begin-for-test
  (local
    ((define SQ (make-square-toy TARGET-INITIAL-X1 TARGET-INITIAL-Y1 SQ-SPEED))
     (define SQ-SEL (new SquareToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [speed SQ-SPEED]))
     (define PG (make-playground 10)))
    
    (send SQ after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send SQ toy-x)
                  TARGET-INITIAL-X1
                  "As square toy SQ is unselected, the drag event does not change it's
                   x-coordinate value")
    
    (send SQ-SEL after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send SQ-SEL toy-x)
                  (+ TARGET-INITIAL-X1 5)
                  "As target TG is selected,the drag event changes it's x-coordinate value
                   to the one of the mouse coordinates")
    (check-equal? (send SQ-SEL for-test:off-x)
                  0
                  "The offset of x-coordinate of mouse from center of the square toy
                   remains constant while dragging")
    (check-equal? (send SQ-SEL for-test:off-y)
                  0
                  "The offset of x-coordinate of mouse from center of the square toy
                   remains constant while dragging")
    
    (send PG after-key-event "s")
    (send PG after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send (first (send PG get-toys)) toy-x)
                  (send SQ toy-x)
                  "The x-coordinate of the playground's square toy and the square toy SQ
                   are same after the after-drag event")))

;......................................................................
; testing for button-up

(begin-for-test
  (local
    ((define SQ-SEL (new SquareToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [speed SQ-SPEED]))
     (define PG (make-playground 10)))
    
    (send SQ-SEL after-button-up TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send SQ-SEL for-test:selected?)
                  false
                  "after-button-up event on SQ-SEL unselects the square toy")
    
    (send PG after-key-event "s")
    (send PG after-button-up TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send (first (send PG get-toys)) for-test:selected?)
                  (send SQ-SEL for-test:selected?)
                  "The playground's square toy and the square toy SQ both remain
                   unselected after the button-up event")))

;.......................................................................
; testing key-event, toy-data and add-to-scene of SquareToy and Playground

(begin-for-test
  (local
    ((define SQ (make-square-toy TARGET-INITIAL-X1 TARGET-INITIAL-Y1 SQ-SPEED))
     (define SQ-IMAGE (square SQ-LEN "outline" "purple"))
     (define TG (make-target))
     (define PG (make-playground 10)))
    
    (send SQ after-key-event ADD-SQUARE-TOY-KEYEVEMT)
    (check-equal? (send SQ toy-x)
                  TARGET-INITIAL-X1
                  "The x-coordinate of the target remains unchanged after any key event
                   as the square toy ignores all key events")
    
    (check-equal? (send SQ toy-data)
                  SQ-SPEED
                  "The toy-data of the square toy SQ is its speed")
    
    (check-equal? (send SQ add-to-scene EMPTY-CANVAS)
                  (place-image SQ-IMAGE TARGET-INITIAL-X1 TARGET-INITIAL-Y1 EMPTY-CANVAS)
                  "The square toy SQ is properly placed at the center of the empty canvas")
    
    (send PG after-key-event "s")
    (check-equal? (send PG add-to-scene EMPTY-CANVAS)
                  (send SQ add-to-scene (send TG add-to-scene EMPTY-CANVAS))
                  "The square toy is is properly overlapped on the target in the canvas
                   which is same as how the playground appears after addition of a square
                   toy in it")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for ThrobberToy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Constants for ThrobberToy

; The minimum and maximum radius of the throbe toy 
(define MIN-T-RADIUS 5)
(define MAX-T-RADIUS 20)

;......................................................................
; testing after-tick on ThrobberToy and PlaygroundState

(begin-for-test
  (local
    ((define TH (make-throbber TARGET-INITIAL-X1 TARGET-INITIAL-Y1))
     (define TH-AT-MIN-RAD (new ThrobberToy% [x TARGET-INITIAL-X1]
                                [y TARGET-INITIAL-Y1]
                                [selected? true]
                                [off-x 0]    
                                [off-y 0]                          
                                [r MIN-T-RADIUS]
                                [r-speed -1]))
     (define TH-AT-MAX-RAD (new ThrobberToy% [x TARGET-INITIAL-X1]
                                [y TARGET-INITIAL-Y1]
                                [selected? true]
                                [off-x 0]    
                                [off-y 0]                          
                                [r MAX-T-RADIUS]
                                [r-speed 1]))
     (define PG (make-playground 10)))
    
    (send TH after-tick)
    (send TH-AT-MIN-RAD after-tick)
    (send PG after-key-event "t")
    (send PG after-tick)
    
    (check-equal? (send TH-AT-MIN-RAD toy-data)
                  (+ MIN-T-RADIUS 1)
                  "the throbber toys's radius is increased after tick if it has reached 
                   its minimum allowable radius")
    
    (send TH-AT-MAX-RAD after-tick)
    (check-equal? (send TH-AT-MAX-RAD toy-data)
                  (- MAX-T-RADIUS 1)
                  "the throbber toys's radius is decresed after tick if it has reached its
                   maximum allowable radius")
    
    (check-equal? (send (first (send PG get-toys)) toy-data)
                  (send TH toy-data)
                  "The radius of the throbber toy TH and the playground's throbber
                   toy are same after the tick")))

;......................................................................
; testing after-button-down on ThrobberToy

(begin-for-test
  (local
    ((define TH (make-throbber TARGET-INITIAL-X1 TARGET-INITIAL-Y1)))    
    
    (send TH after-button-down 1 1)
    (check-equal? (send TH for-test:selected?)
                  false
                  "The throbber toy remains unselected as the button-down is outside the
                   boundary of the toy")
    
    (send TH after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send TH for-test:selected?)
                  true
                  "The throbber toy is selected as the button-down is inside the toy")))    
;....................................................................................
; testing after-drag on ThrobberToy

(begin-for-test
  (local
    ((define TH (make-throbber TARGET-INITIAL-X1 TARGET-INITIAL-Y1))
     (define TH-SEL (new ThrobberToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]    
                         [off-y 0]                          
                         [r MIN-T-RADIUS]
                         [r-speed -1])))
    
    (send TH after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send TH toy-x)
                  TARGET-INITIAL-X1
                  "As throbber toy TH is unselected, the drag event does not change 
                      it's x-coordinate value")
    (check-equal? (send TH toy-y)
                  TARGET-INITIAL-Y1
                  "As throbber toy TH is unselected, the drag event does not change 
                      it's y-coordinate value")
    
    (send TH-SEL after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send TH-SEL toy-x)
                  (+ TARGET-INITIAL-X1 5)
                  "As throbber toy TH-SEL is selected,the drag event changes it's 
                      x-coordinate value to the one of the mouse coordinates")
    (check-equal? (send TH-SEL toy-y)
                  (+ TARGET-INITIAL-Y1 5)
                  "As throbber toy TH-SEL is selected,the drag event changes it's 
                      y-coordinate value to the one of the mouse coordinates")
    (check-equal? (send TH-SEL for-test:off-x)
                  0
                  "The offset of x-coordinate of mouse from center of the throbber toy
                      remains constant while dragging")
    (check-equal? (send TH-SEL for-test:off-y)
                  0
                  "The offset of y-coordinate of mouse from center of the throbber toy
                      remains constant while dragging")))

;....................................................................................
; testing after-button-up on Throbbertoy

(begin-for-test
  (local
    ((define TH-SEL (new ThrobberToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]    
                         [off-y 0]                          
                         [r MIN-T-RADIUS]
                         [r-speed 1])))
    
    (send TH-SEL after-button-up TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send TH-SEL for-test:selected?)
                  false
                  "after-button-up event on TH-SEL unselects the throbber toy")))

;....................................................................................
; testing after-key-event, toy-data and add-to-scene on Throbbertoy

(begin-for-test
  (local
    ((define TH (make-throbber TARGET-INITIAL-X1 TARGET-INITIAL-Y1)))
    
    (send TH after-key-event ADD-SQUARE-TOY-KEYEVEMT)
    (check-equal? (send TH toy-x)
                  TARGET-INITIAL-X1
                  "The x-coordinate of the throbber toy TH remains unchanged after any 
                      key event as the throbber toy ignores all key events")
    
    (check-equal? (send TH toy-data)
                  MIN-T-RADIUS
                  "The toy-data of the throbber toy SQ is its radius")
    
    (check-equal? (send TH add-to-scene EMPTY-CANVAS)
                  (place-image (circle MIN-T-RADIUS "solid" "green")
                               TARGET-INITIAL-X1 TARGET-INITIAL-Y1 EMPTY-CANVAS)
                  "The throbber toy TH is properly placed at the center of the empty
                      canvas")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for ClockToy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Conatants for Clock
(define INIT-CL-TICK 0)

;......................................................................
; testing after-tick on ClockToy and PlaygroundState

(begin-for-test
  (local
    ((define CL (make-clock TARGET-INITIAL-X1 TARGET-INITIAL-Y1))
     (define CL-SEL (new ClockToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [c-number INIT-CL-TICK]))
     (define PG (make-playground 10)))
    
    
    (send CL after-tick)
    (send CL-SEL after-tick)
    (send PG after-key-event "w")
    (send PG after-tick)
    
    (check-equal? (send CL-SEL toy-data)
                  (+ INIT-CL-TICK 1)
                  "The tick value of the clock toy CL increases by 1 after the tick")
    (check-equal? (send (first (send PG get-toys)) toy-x)
                  (send CL toy-x)
                  "The tick value of the clock toy CL and the playground's clock
                   toy are same after the tick")))

;....................................................................................
; testing after-button-down on ClockToy

(begin-for-test
  (local
    ((define CL (make-clock TARGET-INITIAL-X1 TARGET-INITIAL-Y1))) 
    
    (send CL after-button-down 1 1)
    (check-equal? (send CL for-test:selected?)
                  false
                  "The clock toy remains unselected as the button-down is outside the
                   boundary of the toy")
    
    (send CL after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send CL for-test:selected?)
                  true
                  "The clock toy is selected as the button-down is inside the toy")))

;....................................................................................
; testing after-drag on ClockToy

(begin-for-test
  (local
    ((define CL (make-clock TARGET-INITIAL-X1 TARGET-INITIAL-Y1))
     (define CL-SEL (new ClockToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [c-number INIT-CL-TICK])))
    
    (send CL after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send CL toy-x)
                  TARGET-INITIAL-X1
                  "As clock toy CL is unselected, the drag event does not change 
                   it's x-coordinate value")
    (check-equal? (send CL toy-y)
                  TARGET-INITIAL-Y1
                  "As throbber toy CL is unselected, the drag event does not change 
                   it's y-coordinate value")
    
    (send CL-SEL after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send CL-SEL toy-x)
                  (+ TARGET-INITIAL-X1 5)
                  "As clock toy CL-SEL is selected,the drag event changes it's 
                   x-coordinate value to the one of the mouse coordinates")
    (check-equal? (send CL-SEL toy-y)
                  (+ TARGET-INITIAL-Y1 5)
                  "As clock toy CL-SEL is selected,the drag event changes it's 
                   y-coordinate value to the one of the mouse coordinates")
    (check-equal? (send CL-SEL for-test:off-x)
                  0
                  "The offset of x-coordinate of mouse from center of the clock toy
                   remains constant while dragging")
    (check-equal? (send CL-SEL for-test:off-y)
                  0
                  "The offset of y-coordinate of mouse from center of the clock toy
                   remains constant while dragging")))

;....................................................................................
; testing after-button-up on Clocktoy

(begin-for-test
  (local
    ((define CL-SEL (new ClockToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [c-number INIT-CL-TICK])))
    (send CL-SEL after-button-up TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send CL-SEL for-test:selected?)
                  false
                  "after-button-up event on CL-SEL unselects the clock toy")))

;....................................................................................
; testing after-key-event, toy-data and add-to-scene on Clocktoy

(begin-for-test
  (local
    ((define CL (make-clock TARGET-INITIAL-X1 TARGET-INITIAL-Y1)))
    
    (send CL after-key-event ADD-SQUARE-TOY-KEYEVEMT)
    (check-equal? (send CL toy-x)
                  TARGET-INITIAL-X1
                  "The x-coordinate of the clock toy CL remains unchanged after any 
                   key event as the throbber toy ignores all key events")
    
    (check-equal? (send CL add-to-scene EMPTY-CANVAS)
                  (place-image (text (number->string (send CL toy-data)) 12 "black")
                               TARGET-INITIAL-X1 TARGET-INITIAL-Y1 EMPTY-CANVAS)
                  "The clock toy CL is properly placed at the center of the empty
                   canvas")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for FootballToy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Conatants for Football
(define INIT-SCALE-VAL 100)
(define LEAST-SCALE-VAL 1)
(define FOOTBALL (bitmap "football.png"))

;......................................................................
; testing after-tick on FootballToy and PlaygroundState

(begin-for-test
  (local
    ((define FB (make-football TARGET-INITIAL-X1 TARGET-INITIAL-Y1))
     (define FB-SEL (new FootballToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [scale-val INIT-SCALE-VAL]))
     (define FB-AT-SCALE-1 (new FootballToy% [x TARGET-INITIAL-X1]
                                [y TARGET-INITIAL-Y1]
                                [selected? true]
                                [off-x 0]
                                [off-y 0]
                                [scale-val LEAST-SCALE-VAL]))
     (define PG (make-playground 10)))
    
    
    (send FB after-tick)
    (send FB-SEL after-tick)
    (send FB-AT-SCALE-1 after-tick)
    (send PG after-key-event "f")
    (send PG after-tick)
    
    (check-equal? (send FB-SEL for-test:scale-val)
                  (- INIT-SCALE-VAL 1)
                  "the football toys's sacle value is decremented after tick")
    
    (check-equal? (send FB-AT-SCALE-1 for-test:scale-val)
                  LEAST-SCALE-VAL
                  "if the football toy is at it's minimum scale value then it is set
                   to that value instead of decrementing")
    
    (check-equal? (send (first (send PG get-toys)) toy-x)
                  (send FB toy-x)
                  "The x-coordinate of the football toy FB and the playground's football
                   toy are same after the tick")))

;....................................................................................
; testing after-button-down on FootballToy

(begin-for-test
  (local
    ((define FB (make-football TARGET-INITIAL-X1 TARGET-INITIAL-Y1)))
    
    (send FB after-button-down 1 1)
    (check-equal? (send FB for-test:selected?)
                  false
                  "The football toy remains unselected as the button-down is outside the
                   boundary of the toy")
    
    (send FB after-button-down TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send FB for-test:selected?)
                  true
                  "The football toy is selected as the button-down is inside the toy")))

;....................................................................................
; testing after-drag on FootballToy

(begin-for-test
  (local
    ((define FB (make-football TARGET-INITIAL-X1 TARGET-INITIAL-Y1))
     (define FB-SEL (new FootballToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [scale-val INIT-SCALE-VAL])))    
    
    (send FB after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send FB toy-x)
                  TARGET-INITIAL-X1
                  "As football toy FB is unselected, the drag event does not change 
                  it's x-coordinate value")
    (check-equal? (send FB toy-y)
                  TARGET-INITIAL-Y1
                  "As football toy FB is unselected, the drag event does not change 
                  it's y-coordinate value")
    
    (send FB-SEL after-drag (+ TARGET-INITIAL-X1 5) (+ TARGET-INITIAL-Y1 5))
    (check-equal? (send FB-SEL toy-x)
                  (+ TARGET-INITIAL-X1 5)
                  "As football toy FB-SEL is selected,the drag event changes it's 
                   x-coordinate value to the one of the mouse coordinates")
    (check-equal? (send FB-SEL toy-y)
                  (+ TARGET-INITIAL-Y1 5)
                  "As football toy FB-SEL is selected,the drag event changes it's 
                   y-coordinate value to the one of the mouse coordinates")
    (check-equal? (send FB-SEL for-test:off-x)
                  0
                  "The offset of x-coordinate of mouse from center of the football toy
                   remains constant while dragging")
    (check-equal? (send FB-SEL for-test:off-y)
                  0
                  "The offset of y-coordinate of mouse from center of the football toy
                   remains constant while dragging")))

;....................................................................................
; testing after-button-up on Footballtoy

(begin-for-test
  (local
    ((define FB-SEL (new FootballToy% [x TARGET-INITIAL-X1]
                         [y TARGET-INITIAL-Y1]
                         [selected? true]
                         [off-x 0]
                         [off-y 0]
                         [scale-val INIT-SCALE-VAL])))
    
    (send FB-SEL after-button-up TARGET-INITIAL-X1 TARGET-INITIAL-Y1)
    (check-equal? (send FB-SEL for-test:selected?)
                  false
                  "after-button-up event on FB-SEL unselects the throbber toy")))

;....................................................................................
; testing after-key-event, toy-data and add-to-scene on Footballtoy

(begin-for-test
  (local
    ((define FB (make-football TARGET-INITIAL-X1 TARGET-INITIAL-Y1)))
    
    (send FB after-key-event ADD-SQUARE-TOY-KEYEVEMT)
    (check-equal? (send FB toy-x)
                  TARGET-INITIAL-X1
                  "The x-coordinate of the football toy FB remains unchanged after any 
                   key event as the football toy ignores all key events")
    
    (define FOOTBALL-IMAGE-NOW (scale (/ (send FB for-test:scale-val) 100) FOOTBALL))
    
    (check-equal? (send FB add-to-scene EMPTY-CANVAS)
                  (place-image FOOTBALL-IMAGE-NOW
                               TARGET-INITIAL-X1 TARGET-INITIAL-Y1 EMPTY-CANVAS)
                  "The football toy FB is properly placed at the center of the empty
                   canvas")
    
    (check-equal? (send FB toy-data)
                  (* (image-width FOOTBALL-IMAGE-NOW) (image-height FOOTBALL-IMAGE-NOW))
                  "The toy-data of the football toy FB is its area")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
