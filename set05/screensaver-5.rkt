;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname screensaver-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET05 : QUESTION 02;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two rectangles move around at a contant velocity in the scene.
;; The user can pause/unpause the rectangles with the space bar.
;; The user can drag the rectangles to a specific position within the canvas.

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-after-mouse-event
 rect-after-mouse-event
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 rect-selected?
 rect-after-key-event
 world-rects
 rect-pen-down?
 world-paused?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA DEFINITIONS;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct myrect (x y vx vy selected? mx my pen?))
;;A Rectangle is a
;;(make-myrect NonNegInt NonNegInt Integer Integer Boolean NonNegInt NonNegInt Boolean)
;;INTERPRETATION:
;;x is the x-coordinate(in pixels) and y is the y-coordinate(in pixels)
;;     such that (x,y) represents the center of the rectangle.
;;vx is the velocity of the center of rectangle in x direction and
;;vy is the velocity of the center of rectangle in y direction
;;selected? describes whether or not the rectangle is selected.
;;mx is the x coordinate determined by the mouse pointer
;;my is the y coordinate determined by the mouse pointer
;;    such that (mx,my) represent the point where mouse is pointing.
;;pen? described whether or not the pen is down.

;; template:
;; myrect-fn : Rectangle -> ??
;(define (myrect-fn r)
;  (... (myrect-x r)
;       (myrect-y r)
;       (myrect-vx r)
;       (myrect-vy r)
;       (myrect-selected? r)
;       (myrect-mx r)
;       (myrect-my r)
;       (myrect-pen? r)
;))

;; ListofRectangles

;; A ListOfRectangles (LOR) is either
;; -- empty
;; -- (cons Rectangle LOR)

;; lor-fn : ListOfRectangles -> ??
;; (define (lor-fn lor)
;;   (cond
;;     [(empty? lor) ...]
;;     [else (...
;;             (myrect-fn (first lor))
;;             (lor-fn (rest lor)))]))

(define-struct world (lor lod paused?))
;; A WorldState is a (make-world LOR LOD Boolean)
;; INTERPRETATION: 
;; LOR is a list of rectangles
;; LOD is a list of pen dots
;; paused? describes whether or not the rectangle is paused.

;; template:
;; world-fn : WorldState -> ??
;(define (world-fn w)
;  (... (world-lor w)
;       (world-lod w)
;       (world-paused? w)))

(define-struct pdots (dx dy))
;;A pdots is (NonNegInt NonNegInt)
;INTERPRETATION:
;dx is the x coordinate of the dot
;dy is the y coordinate of the dot

;; template:
;; pdots-fn : Pendots -> ??
;(define (pdots-fn d)
;  (... (pdots-dx d)
;       (pdots-dy d)))

;; A ListOfDots (LOD) is either
;; -- empty
;; -- (cons Pendots LOD)

;; lod-fn : ListOfDots -> ??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod) ...]
;;     [else (...
;;             (pdots-fn (first lod))
;;             (lod-fn (rest lod)))]))

;;An Axis is either
;;-- X-AXIS
;;-- Y-AXIS

;;TEMPLATE:
#|(define (axis-fn a)
  (cond
    [(string=? a X-AXIS)...]
    [(string=? a Y-AXIS)...]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONSTANTS
;;increase/decrease in velocity
(define INC-VEL 2)
(define DEC-VEL -2)

;;direction of velocity
(define X-AXIS "x")
(define Y-AXIS "y")

;;deimensions of rectangle
(define RECT-HEIGHT 50)
(define RECT-WIDTH 60)

(define SEL-COLOR "red")
(define UNSEL-COLOR "blue")
(define RECT (rectangle RECT-WIDTH RECT-HEIGHT "outline" UNSEL-COLOR))

(define RECT-CLICKED (rectangle RECT-WIDTH RECT-HEIGHT "outline" SEL-COLOR))
(define PEN-DOT (circle 1 "solid" "black"))
(define CIRCLE-AT-MOUSE-POINT (circle 5 "outline" "red"))
(define VEL-FONT 12)

;;Contants to set collide conditions

(define COLLIDE-NORTH-Y (/ RECT-HEIGHT 2))
(define COLLIDE-WEST-X (/ RECT-WIDTH 2))
(define COLLIDE-SOUTH-Y (- 300 COLLIDE-NORTH-Y))
(define COLLIDE-EAST-X (- 400 COLLIDE-WEST-X))

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; Initial state of the rectangles
(define UNSEL-NEW-RECT (make-myrect
                        (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0 false 0 0 #f))
(define SEL-NEW-RECT (make-myrect
                      (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0 true 0 0 #f))
(define SEL-INITIAL-RECT (make-myrect
                          (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0 true 0 0 #f))

;;Rectangles for testing
(define UNSEL-RECT-AT-CENTER (make-myrect (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
                                          5 -6 false 0 0 #f))
(define SEL-RECT-AT-CENTER (make-myrect (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
                                        5 -6 true 0 0 #f))
(define UNSEL-RECT-AT-200-150 (make-myrect 200 150 15 -9 false 0 0 #f))
(define SEL-RECT-AT-200-150 (make-myrect 200 150 15 -9 true 0 0 #f))
(define UNSEL-RECT-AT-200-150-PEN-DOWN (make-myrect 200 150 15 -9 #f 0 0 #t))
(define SEL-RECT-AT-200-150-PEN-DOWN (make-myrect 200 150 15 -9 #t 0 0 #t))

(define SEL-RECT-AT-200-150-AFTER-BUTTON-DOWN (make-myrect 200 150 15 -9 #t 205 145 #t))
(define SEL-NEW-RECT-AFTER-DRAG (make-myrect 405 295 0 0 #true 205 145 #f))
(define UNSEL-NEW-RECT-AFTER-BUTTON-UP (make-myrect 405 295 0 0 #false 205 145 #f))

(define SEL-RECT-AT-CENTER-WITH-DEC-VX (make-myrect 200 150 3 -6 #true 0 0 #f))
(define UNSEL-RECT-AT-CENTER-WITH-INC-VX (make-myrect 200 150 7 -6 #false 0 0 #f))
(define UNSEL-RECT-AT-CENTER-WITH-DEC-VY (make-myrect 200 150 5 -8 #false 0 0 #f))

(define SEL-RECT-AT-CENTER-AFTER-LEFT (make-myrect 200 150 -2 0 #true 0 0 #f))
(define SEL-RECT-AT-CENTER-AFTER-RIGHT (make-myrect 200 150 2 0 #true 0 0 #f))
(define SEL-RECT-AT-CENTER-AFTER-UP (make-myrect 200 150 0 -2 #true 0 0 #f))
(define SEL-RECT-AT-CENTER-AFTER-DOWN (make-myrect 200 150 0 2 #true 0 0 #f))
(define SEL-RECT-AT-CENTER-AFTER-BUTTON-DOWN (make-myrect 200 150 5 -6 #true 205 145 #f))

(define RECT-COLLIDING-NORTH (make-myrect 15 COLLIDE-NORTH-Y -12 -20 #false 0 0 #f))
(define RECT-COLLIDING-SOUTH (make-myrect 15 COLLIDE-SOUTH-Y -12 20 #false 0 0 #f))
(define RECT-COLLIDING-EAST (make-myrect COLLIDE-EAST-X 20 12 20 #false 0 0 #f))
(define RECT-COLLIDING-WEST (make-myrect COLLIDE-WEST-X 15 -12 20 #false 0 0 #f))

(define RECT-NOT-COLLIDING-NORTH (make-myrect 15 COLLIDE-NORTH-Y -12 20 #false 0 0 #f))
(define RECT-NOT-COLLIDING-SOUTH (make-myrect 15 COLLIDE-SOUTH-Y -12 -20 #false 0 0 #f))
(define RECT-NOT-COLLIDING-EAST (make-myrect COLLIDE-EAST-X 20 -12 20 #false 0 0 #f))
(define RECT-NOT-COLLIDING-WEST (make-myrect COLLIDE-WEST-X 15 12 20 #false 0 0 #f))

;;;;Rectangles for testing world-to-scene
(define I-UNSEL-RECT-AT-CENTER (overlay (text "(5,-6)" 12 "blue")
                                        (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue")))
(define I-SEL-RECT-AT-CENTER (overlay (text "(5,-6)" 12 "red")
                                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "red")))

;;World for testing
(define PAUSED-EMPTY-WORLD (make-world '() '() #true))
(define UNPAUSED-WORLD-WITH-ONE-RECT
  (make-world (cons UNSEL-RECT-AT-CENTER empty) '() false))
(define PAUSED-WORLD-WITH-ONE-RECT
  (make-world (cons UNSEL-RECT-AT-CENTER empty) '() true))
(define PAUSED-WORLD-WITH-ONE-SEL-RECT
  (make-world (cons (make-myrect 200 150 5 -6 #true 205 145 #f) '()) '() #true))
(define PAUSED-WORLD-WITH-ONE-SEL-RECT-AFTER-DRAG
  (make-world (cons (make-myrect 245 165 5 -6 #true 250 160 #f) '()) '() #true))
(define PAUSED-WORLD-WITH-ONE-RECT-AFTER-BUTTON-UP
  (make-world (cons (make-myrect 200 150 5 -6 #false 205 145 #f) '()) '() #true))
(define UNPAUSED-WORLD-WITH-TWO-RECTS
  (make-world (cons UNSEL-RECT-AT-CENTER (cons UNSEL-RECT-AT-200-150 empty)) '() false))

(define UNPAUSED-WORLD-WITH-ONE-RECT-AFTER-TICK
  (make-world (cons (make-myrect 205 144 5 -6 #false 0 0 #f) '()) '() #false))
(define UNPAUSED-WORLD-WITH-TWO-RECTS-AFTER-TICK
  (make-world (cons (make-myrect 205 144 5 -6 #false 0 0 #f)
                    (cons (make-myrect 215 141 15 -9 #false 0 0 #f) '())) '() #false))
(define WORLD-WITH-NEW-RECT (make-world (cons UNSEL-NEW-RECT '()) '() #true))

;; examples KeyEvents for testing
(define PAUSE-KEY-EVENT " ")
(define NON-PAUSE-KEY-EVENT "q")
(define N "n")
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
(define PEN-DOWN "d")
(define PEN-UP "u")
(define RANDOM-KEY "p")

;;List of rectangles
(define EMPTY-LOR '())
(define LIST-OF-UNSEL-ONE-RECT (cons UNSEL-RECT-AT-CENTER empty))
(define LIST-OF-UNSEL-RECTS (cons UNSEL-RECT-AT-CENTER (cons UNSEL-RECT-AT-200-150 '())))
(define LIST-WITH-ONE-SEL-RECT (cons SEL-RECT-AT-CENTER (cons UNSEL-RECT-AT-200-150 '())))
(define LIST-WITH-ONE-SEL-RECT-AFTER-LEFT
  (cons SEL-RECT-AT-CENTER-WITH-DEC-VX (cons UNSEL-RECT-AT-200-150 '())))
(define LIST-WITH-TWO-SEL-RECT-AFTER-BUTTON-DOWN
  (cons SEL-RECT-AT-CENTER-AFTER-BUTTON-DOWN
        (cons SEL-RECT-AT-200-150-AFTER-BUTTON-DOWN '())))
(define LOD1 (cons (make-pdots 200 150) empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN FUNCTION.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver simulation-speed)
  (big-bang (initial-world 5)
            (on-tick world-after-tick simulation-speed)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set i.e no rectangles 
;; STATEGY: Call simpler functions
;; EXAMPLE:
;;(initial-world 5)=PAUSED-EMPTY-WORLD

(define (initial-world n)
  (make-world
   empty
   empty
   true))

;;TEST:
(begin-for-test
  (check-equal? (initial-world 5)
                PAUSED-EMPTY-WORLD
                "Initialises world with an empty list of rectangles in paused state"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-pen-down? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: true if the pen is down for the given rectangle
;; STATEGY: Use template for Rectangle on r
;; EXAMPLE
;; (rect-pen-down? UNSEL-NEW-RECT)=#f

(define (rect-pen-down? r)
  (myrect-pen? r))

;TEST:
(begin-for-test
  (check-equal? (rect-pen-down? UNSEL-NEW-RECT)
                #f
                "Pen is up i.e not selected"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; STRATEGY: Use template for WorldState on w
;; EXAMPLES:
;; EXAMPLES:
;;(world-after-tick UNPAUSED-WORLD-WITH-ONE-RECT)
;;    =UNPAUSED-WORLD-WITH-ONE-RECT-AFTER-TICK
;;(world-after-tick PAUSED-WORLD-WITH-ONE-RECT)
;;    =PAUSED-WORLD-WITH-ONE-RECT
;;(world-after-tick UNPAUSED-WORLD-WITH-TWO-RECTS)
;;    =UNPAUSED-WORLD-WITH-TWO-RECTS-AFTER-TICK

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (list-rect-after-tick (world-lor w))
       (list-dots-after-tick (world-lor w)(world-lod w))
       (world-paused? w))))

;;TEST:
(begin-for-test
  (check-equal? (world-after-tick UNPAUSED-WORLD-WITH-ONE-RECT)
                UNPAUSED-WORLD-WITH-ONE-RECT-AFTER-TICK
                "The rectangle moves from (200,150) to (205,144)")
  (check-equal? (world-after-tick PAUSED-WORLD-WITH-ONE-RECT)
                PAUSED-WORLD-WITH-ONE-RECT
                "The rectangle does not move as the world is paused")
  (check-equal? (world-after-tick UNPAUSED-WORLD-WITH-TWO-RECTS)
                UNPAUSED-WORLD-WITH-TWO-RECTS-AFTER-TICK))
;........................................................................
;; list-rect-after-tick: ListOfRectangles -> ListOfRectangles
;; GIVEN: a list of rectangles
;; RETURNS: the list of rectangles that follows the given list of rectangles
;; after a tick.
;; STRATEGY: Use HOF map on lor
;; EXAMPLES:
;; (list-rect-after-tick (cons UNSEL-RECT-AT-200-150 (cons UNSEL-RECT-AT-CENTER '())))
;;     =(cons (make-myrect 215 141 15 -9 #false 0 0 #false)
;;            (cons (make-myrect 205 144 5 -6 #false 0 0 #false) '()))

(define (list-rect-after-tick lor)
  (map rect-after-tick lor))

;TEST
(begin-for-test
  (check-equal? (list-rect-after-tick (cons UNSEL-RECT-AT-200-150 (cons UNSEL-RECT-AT-CENTER '())))
                (cons (make-myrect 215 141 15 -9 #false 0 0 #false)
                      (cons (make-myrect 205 144 5 -6 #false 0 0 #false) '()))
                "Both rectangles change their location based on their respective
                 velocities after one tick"))
;.......................................................................
;; list-dot-after-tick: ListOfRectangles ListOfDots -> ListOfDots
;; GIVEN: a list of rectangles and list of dots
;; RETURNS: the list of dots that follows the given list of dots
;; after a tick.
;; STRATEGY: Use template for ListOfRectangles on lor
;; EXAMPLES:
;; (list-dot-after-tick (cons UNSEL-RECT-AT-200-150 '()) empty))=empty
;; (list-dot-after-tick (cons UNSEL-RECT-AT-200-150-PEN-DOWN '()) empty))
;   =(cons (make-pdots 200 150) '())

(define (list-dots-after-tick lor lod)
  (foldr
   dots-after-tick
   lod
   lor))

;TEST:
(begin-for-test
  (check-equal? (list-dots-after-tick (cons SEL-RECT-AT-200-150 '()) empty)
                empty
                "the rectangle has pen up. So no dots are placed")
  (check-equal? (list-dots-after-tick (cons UNSEL-RECT-AT-200-150-PEN-DOWN '()) empty)
                (cons (make-pdots 200 150) '())
                "The dot is added to the list as for the given rectangle pen is down"))
;;.......................................................................
;; dots-after-tick : Rectangle ListOfDots -> Rectangle
;; Given: a rectangle and a list of dots
;; RETURNS: a list of dots for the given rectangle after a tick
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES:
;; (dots-after-tick SEL-RECT-AT-200-150-PEN-DOWN empty)='()
;; (dots-after-tick UNSEL-RECT-AT-200-150-PEN-DOWN empty)=(list (make-pdots 200 150))

(define (dots-after-tick r lod)
  (if (and (myrect-pen? r) (not (myrect-selected? r)))
      (cons (make-pdots (myrect-x r) (myrect-y r)) lod)
      lod))

;; TESTS:
(begin-for-test
  (check-equal? (dots-after-tick SEL-RECT-AT-200-150-PEN-DOWN empty)
                '()
                "Since the rectangle is selected i.e. held by mouse, it shouldn't make
                 pen dots on the canvas even if the pen is down")
  (check-equal? (dots-after-tick UNSEL-RECT-AT-200-150-PEN-DOWN empty)
                (list (make-pdots 200 150))
                "An unselected rectangle makes pen dots on every tick after the pen
                 is down"))
;........................................................................
;; rect-after-tick : Rectangle -> Rectangle
;; Given: rectangle 1
;; RETURNS: the state of the given rectangle 1 after a tick in an unpaused world
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES:
;;(rect-after-tick UNSEL-RECT-AT-CENTER)=(make-myrect 205 144 5 -6 #false 0 0 #false)
;;(rect-after-tick SEL-RECT-AT-CENTER)=SEL-RECT-AT-CENTER
(define (rect-after-tick r)
  (if (myrect-selected? r)
      r
      (make-myrect
       (update-rect-coordinate (myrect-x r) (myrect-vx r) COLLIDE-WEST-X COLLIDE-EAST-X)
       (update-rect-coordinate (myrect-y r) (myrect-vy r) COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
       (update-rect-velocity (myrect-x r) (myrect-vx r) COLLIDE-WEST-X COLLIDE-EAST-X)
       (update-rect-velocity (myrect-y r) (myrect-vy r) COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
       (myrect-selected? r)
       0
       0
       (myrect-pen? r))))
;TESTS:
(begin-for-test
  (check-equal? (rect-after-tick UNSEL-RECT-AT-CENTER)
                (make-myrect 205 144 5 -6 #false 0 0 #false)
                "The rectangle moves by the specifies velocity after one tick")
  (check-equal? (rect-after-tick SEL-RECT-AT-CENTER)
                SEL-RECT-AT-CENTER
                "The rectangle is hold by mouse and hence it does not move to next step
                 by the specified velocity"))

;.................................................................................
;; HELPER FUNCTIONS for rect-after-tick
;.................................................................................

;; update-rect-coordinate : NonNegInt Integer NonNegInt NonNegInt -> NonNegInt
;; GIVEN: a rectangle's coordinate, rectangle's velocity, minimum and maximum boundary
;;        values
;; RETURNS: the boundary value as rectangle's coordinate if the rectangle collides
;;          the wall otherwise the next coordinate after tick
;; STRATEGY: Cases on boundary conditions
;; EXAMPLES:
;; (update-rect-coordinate 30 -12 COLLIDE-WEST-X COLLIDE-EAST-X)=COLLIDE-WEST-X
;; (update-rect-coordinate 30 12 COLLIDE-WEST-X COLLIDE-EAST-X)=42
;; (update-rect-coordinate 370 -12 COLLIDE-WEST-X COLLIDE-EAST-X)=358
;; (update-rect-coordinate 370 12 COLLIDE-WEST-X COLLIDE-EAST-X)=COLLIDE-EAST-X
;; (update-rect-coordinate 25 20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)=45
;; (update-rect-coordinate 25 -20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)=COLLIDE-NORTH-Y
;; (update-rect-coordinate 275 20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)=COLLIDE-SOUTH-Y
;; (update-rect-coordinate 275 -20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)=255

(define (update-rect-coordinate r-coordinate r-velocity min-wall max-wall)
  (cond
    [(<= (+ r-coordinate r-velocity) min-wall) min-wall]
    [(>= (+ r-coordinate r-velocity) max-wall) max-wall]
    [else (+ r-coordinate r-velocity)]))

;TESTS:
(begin-for-test
  (check-equal? (update-rect-coordinate 30 -12 COLLIDE-WEST-X COLLIDE-EAST-X)
                COLLIDE-WEST-X
                "The rectangle goes beyond the west wall in next tick. So set
                 x-coordinate to west boundry coordinate")
  (check-equal? (update-rect-coordinate 30 12 COLLIDE-WEST-X COLLIDE-EAST-X)
                42
                "The rectangle does not go beyond the west wall in next tick.
                So set x-coordinate by adding the velocity.")
  (check-equal? (update-rect-coordinate 370 -12 COLLIDE-WEST-X COLLIDE-EAST-X)
                358
                "The rectangle does not go beyond the east wall in next tick.
                So set x-coordinate by adding the velocity.")
  (check-equal? (update-rect-coordinate 370 12 COLLIDE-WEST-X COLLIDE-EAST-X)
                COLLIDE-EAST-X
                "The rectangle goes beyond the east wall in next tick. So set
                 x-coordinate to east boundry coordinate")   
  (check-equal? (update-rect-coordinate 25 20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
                45
                "The rectangle does not go beyond the north wall in next tick.
                So set y-coordinate by adding the velocity.")
  (check-equal? (update-rect-coordinate 25 -20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
                COLLIDE-NORTH-Y
                "The rectangle goes beyond the north wall in next tick. So set
                 y-coordinate to north boundry coordinate")
  (check-equal? (update-rect-coordinate 275 20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
                COLLIDE-SOUTH-Y
                "The rectangle goes beyond the south wall in next tick. So set
                 y-coordinate to south boundry coordinate")
  (check-equal? (update-rect-coordinate 275 -20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
                255
                "The rectangle does not go beyond the south wall in next tick.
                So set y-coordinate by adding the velocity."))    
;......................................................................
;; update-rect-velocity :NonNegInt Integer NonNegInt NonNegInt -> Integer
;; GIVEN: a rectangle's coordinate, rectangle's velocity, minimum and maximum boundary
;;        values
;; RETURNS: the velocity of a rectangle after tick
;; STRATEGY: Cases on boundary conditions
;; EXAMPLES:
;;(update-rect-vx RECT-COLLIDING-WEST)=12
;;(update-rect-vx RECT-NOT-COLLIDING-WEST)=12

(define (update-rect-velocity r-coordinate r-velocity min-wall max-wall)
  (if (or (<= (+ r-coordinate r-velocity) min-wall)
          (>= (+ r-coordinate r-velocity) max-wall))
      (- 0 r-velocity)
      r-velocity))

;TESTS:
(begin-for-test
  (check-equal? (update-rect-velocity 30 -12 COLLIDE-WEST-X COLLIDE-EAST-X)
                12
                "The rectangle collides west wall and hence its x velocity is reversed
                 from -12 to 12")
  (check-equal? (update-rect-velocity 30 12 COLLIDE-WEST-X COLLIDE-EAST-X)
                12
                "The rectangle's x velocity remains same as it does not collide west
                 wall i.e does not change direction")
  (check-equal? (update-rect-velocity 25 -20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
                20
                "The rectangle collides north wall and hence its y velocity is reversed
                 from -20 to 20")
  (check-equal? (update-rect-velocity 25 20 COLLIDE-NORTH-Y COLLIDE-SOUTH-Y)
                20
                "The rectangle's y velocity remains same as it does not collide north
                 wall i.e does not change direction"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-rects : WorldState -> ListOfRectangles
;; GIVEN: a world state
;; RETURNS: the specified attribute i.e list of rectangles of the WorldState
;; EXAMPLE:
;; (world-rects UNPAUSED-WORLD-WITH-TWO-RECTS)
;; =LIST-OF-UNSEL-RECTS

(define (world-rects w)
  (world-lor w))

;;TEST:
(begin-for-test
  (check-equal? (world-rects UNPAUSED-WORLD-WITH-TWO-RECTS)
                LIST-OF-UNSEL-RECTS
                "Returns the list of rectangles in the given world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN:  a World State and a Keyevent
;; RETURNS: the World State that should follow the given world state
;;          after the given keyevent
;; STRATEGY: Dividing cases for Keyevent on kev
;; EXAMPLES:
;;(world-after-key-event UNPAUSED-WORLD-WITH-ONE-RECT PAUSE-KEY-EVENT)
;;   =PAUSED-WORLD-WITH-ONE-RECT
;;(world-after-key-event UNPAUSED-WORLD-WITH-ONE-RECT NON-PAUSE-KEY-EVENT)
;;   =UNPAUSED-WORLD-WITH-ONE-RECT
;;(world-after-key-event PAUSED-EMPTY-WORLD "n")
;;   =WORLD-WITH-NEW-RECT

(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ") (world-with-paused-toggled w)]
    [(key=? kev N) (create-rect w)]
    [else (make-world (list-after-key-event (world-lor w) kev)
                      (world-lod w)
                      (world-paused? w))]))
;TESTS:
(begin-for-test
  (check-equal? (world-after-key-event UNPAUSED-WORLD-WITH-ONE-RECT PAUSE-KEY-EVENT)
                PAUSED-WORLD-WITH-ONE-RECT
                "Unpaused world pauses after a valid key event")
  (check-equal? (world-after-key-event UNPAUSED-WORLD-WITH-ONE-RECT NON-PAUSE-KEY-EVENT)
                UNPAUSED-WORLD-WITH-ONE-RECT
                "Invalid key event makes no changes to the world")
  (check-equal? (world-after-key-event PAUSED-EMPTY-WORLD "n")
                WORLD-WITH-NEW-RECT
                "A new rectangle is created after pressing n key"))
;...............................................................................
;; list-after-key-event : ListOfRectangles Keyevent -> ListOfRectangles
;; GIVEN: a list of rectangles and a keyevent
;; RETURNS: list of rectang les following the given keyevent
;; STRATEGY: Use template for ListOfRectangles on lor
;; EXAMPLE:
;; (list-after-key-event EMPTY-LOR LEFT)=EMPTY-LOR
;; (list-after-key-event LIST-OF-UNSEL-RECTS LEFT)=LIST-OF-UNSEL-RECTS
;; (list-after-key-event LIST-WITH-ONE-SEL-RECT LEFT)=LIST-WITH-ONE-SEL-RECT-AFTER-LEFT

(define (list-after-key-event lor kev)
  (map
   ;;Rectangle -> Rectangle
   ;;GIVEN: a rectangle
   ;;RETURNS: the given rectangle after the key event specified by kev
   (lambda (r) (rect-after-key-event r kev)) lor))

;TESTS:
(begin-for-test
  (check-equal? (list-after-key-event EMPTY-LOR LEFT)
                EMPTY-LOR
                "Any key event on empty list of rectangles returns empty list of rectangles")
  (check-equal? (list-after-key-event LIST-OF-UNSEL-RECTS LEFT)
                LIST-OF-UNSEL-RECTS
                "No changes made as no rectangle is selected")
  (check-equal? (list-after-key-event LIST-WITH-ONE-SEL-RECT LEFT)
                LIST-WITH-ONE-SEL-RECT-AFTER-LEFT
                "Decreases the vx from 5 to 3 i.e decrease by 2 pixels"))
;............................................................................
;; rect-after-key-event : Rectangle Keyevent -> Rectangle
;; GIVEN: a rectangle and a keyevent
;; RETURNS: rectangle after the given keyevent
;; STRATEGY: Dividing cases for KeyEvent on kev
;; EXMAPLES:
;; (rect-after-key-event SEL-INITIAL-RECT LEFT)=SEL-RECT-AT-CENTER-AFTER-LEFT
;; (rect-after-key-event SEL-INITIAL-RECT RIGHT)=SEL-RECT-AT-CENTER-AFTER-RIGHT
;; (rect-after-key-event SEL-INITIAL-RECT UP)=SEL-RECT-AT-CENTER-AFTER-UP
;; (rect-after-key-event SEL-INITIAL-RECT DOWN)=SEL-RECT-AT-CENTER-AFTER-DOWN
;;(rect-after-key-event SEL-INITIAL-RECT RANDOM-KEY)=SEL-INITIAL-RECT

(define (rect-after-key-event r kev)
  (if (myrect-selected? r)
      (cond
        [(key=? kev LEFT) (drift-rect X-AXIS r DEC-VEL)]
        [(key=? kev RIGHT) (drift-rect X-AXIS r INC-VEL)]
        [(key=? kev UP) (drift-rect Y-AXIS r DEC-VEL)]
        [(key=? kev DOWN) (drift-rect Y-AXIS r INC-VEL)]
        [(key=? kev PEN-DOWN) (update-pen #t r)]
        [(key=? kev PEN-UP) (update-pen #f r)]
        [else r])
      r))

;TESTS:
(begin-for-test
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT LEFT)
                SEL-RECT-AT-CENTER-AFTER-LEFT
                "vx decreses by 2 pixels")
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT RIGHT)
                SEL-RECT-AT-CENTER-AFTER-RIGHT
                "vx increses by 2 pixels")
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT UP)
                SEL-RECT-AT-CENTER-AFTER-UP
                "vy decrease by 2 pixels")
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT DOWN)
                SEL-RECT-AT-CENTER-AFTER-DOWN
                "vy increses by 2 pixels")
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT PEN-DOWN)
                (make-myrect 200 150 0 0 #true 0 0 #true)
                "vy increses by 2 pixels")
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT PEN-UP)
                (make-myrect 200 150 0 0 #true 0 0 #false)
                "vy increses by 2 pixels")
  (check-equal? (rect-after-key-event SEL-INITIAL-RECT RANDOM-KEY)
                SEL-INITIAL-RECT
                "Not a valid keyevent and hence no change in velocity"))
;...........................................................................
;; world-with-paused-toggled : WorldState -> WorldState
;; GIVEN: a world
;; RETURNS: a world with its paused state toggled
;; STRATEGY: Use template for WorldState on w
;; EXAMPLE: 
;; (world-with-paused-toggled UNPAUSED-WORLD-WITH-ONE-RECT)=PAUSED-WORLD-WITH-ONE-RECT

(define (world-with-paused-toggled w)
  (make-world
   (world-lor w)
   (world-lod w)
   (not (world-paused? w))))

;TEST:
(begin-for-test
  (check-equal? (world-with-paused-toggled UNPAUSED-WORLD-WITH-ONE-RECT)
                PAUSED-WORLD-WITH-ONE-RECT
                "Unpaused world pauses and vice-versa i.e it gets toggles"))
;...........................................................................
;; drift-rect : Axis Rectangle Integer -> Rectangle
;; GIVEN: an axis, a rectangle after key event and increased/decreased value
;;        in the velocity of that rectangle along the given axis
;; RETURNS: the given rectangle with updated velocity
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLE:
;; (drift-rect-x UNSEL-RECT-AT-CENTER INC-VEL)=UNSEL-RECT-AT-CENTER-WITH-INC-VX

(define (drift-rect a r v)
  (make-myrect
   (myrect-x r)
   (myrect-y r)
   (if (string=? a X-AXIS)(+ (myrect-vx r) v) (myrect-vx r)) 
   (if (string=? a Y-AXIS)(+ (myrect-vy r) v) (myrect-vy r)) 
   (myrect-selected? r)
   (myrect-mx r)
   (myrect-my r)
   (myrect-pen? r)))

;TEST:
(begin-for-test
  (check-equal? (drift-rect X-AXIS UNSEL-RECT-AT-CENTER INC-VEL)
                UNSEL-RECT-AT-CENTER-WITH-INC-VX
                "the vx increases by 2 pixels"))
;...........................................................................
;; create-rect : WorldState -> WorldState
;; GIVEN: a world 
;; RETURNS: a world with a new rectangle created at the center of the canvas
;;          with (0,0) velocity
;; STRATEGY: Use template for WorldState on w
;; EXAMPLE:
;; (create-rect PAUSED-EMPTY-WORLD)=WORLD-WITH-NEW-RECT

(define (create-rect w)
  (make-world
   (cons UNSEL-NEW-RECT (world-lor w))
   (world-lod w)
   (world-paused? w)))

;;TEST:
(begin-for-test
  (check-equal? (create-rect PAUSED-EMPTY-WORLD)
                WORLD-WITH-NEW-RECT
                "A new stationary rectangle is created at the center of the scene"))
;..........................................................................
;; update-pen: Boolean Rectangle -> Rectangle
;; GIVEN: value for pen-selected? and rectangle
;; RETURNS: a rectangle with pen down
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLE:
;; (update-pen #t UNSEL-NEW-RECT)=(make-myrect 200 150 0 0 #f 0 0 #t)

(define (update-pen PEN-VAL r)
  (make-myrect
   (myrect-x r)
   (myrect-y r)
   (myrect-vx r)
   (myrect-vy r)
   (myrect-selected? r)
   (myrect-mx r)
   (myrect-my r)
   PEN-VAL))

;TESTS:
(begin-for-test
  (check-equal? (update-pen #t UNSEL-NEW-RECT)
                (make-myrect 200 150 0 0 #f 0 0 #t)
                "Pen is down")
  (check-equal? (update-pen #f UNSEL-NEW-RECT)
                (make-myrect 200 150 0 0 #f 0 0 #f)
                "Pen is up"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event : WorldState NonNegInt NonNegInt MouseEvent -> WorldState
;; GIVEN: A world, the x- and y-coordinates of mouse and the mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; STRATEGY: Use template for WorldState on w
;;EXAMPLES:
;;(world-after-mouse-event PAUSED-WORLD-WITH-ONE-RECT 140 60 "button-down")
;; =PAUSED-WORLD-WITH-ONE-RECT
;;(world-after-mouse-event PAUSED-WORLD-WITH-ONE-RECT 205 145 "button-down")
;; =PAUSED-WORLD-WITH-ONE-SEL-RECT
;;(world-after-mouse-event PAUSED-WORLD-WITH-ONE-SEL-RECT 250 160 "drag")
;; =PAUSED-WORLD-WITH-ONE-SEL-RECT-AFTER-DRAG
;;(world-after-mouse-event PAUSED-WORLD-WITH-ONE-SEL-RECT 205 145 "button-up")
;; =PAUSED-WORLD-WITH-ONE-RECT-AFTER-BUTTON-UP

(define (world-after-mouse-event w mx my mev)
  (make-world
   (list-after-mouse-event (world-lor w) mx my mev)
   (world-lod w)
   (world-paused? w)))

;TESTS:
(begin-for-test
  (check-equal? (world-after-mouse-event PAUSED-WORLD-WITH-ONE-RECT 140 60 "button-down")
                PAUSED-WORLD-WITH-ONE-RECT
                "Button down doesnt happen as the mouse coordinates are outside rectangle")
  (check-equal? (world-after-mouse-event PAUSED-WORLD-WITH-ONE-RECT 205 145 "button-down")
                PAUSED-WORLD-WITH-ONE-SEL-RECT
                "After button down, the rectangle is selected")
  (check-equal? (world-after-mouse-event PAUSED-WORLD-WITH-ONE-SEL-RECT 250 160 "drag")
                PAUSED-WORLD-WITH-ONE-SEL-RECT-AFTER-DRAG
                "The rectangle's center changes to the new location after drag and the mouse
                coordinates are saved")
  (check-equal? (world-after-mouse-event PAUSED-WORLD-WITH-ONE-SEL-RECT 205 145 "button-up")
                PAUSED-WORLD-WITH-ONE-RECT-AFTER-BUTTON-UP
                "the rectangle is now unselected and the mouse coordinates are saved"))
;.........................................................................
;; list-after-mouse-event :
;;     ListOfRectangles NonNegInt NonNegInt MouseEvent -> ListOfRectangles
;; GIVEN: A list of rectangles, the x- and y-coordinates of mouse and the mouse event
;; RETURNS: a list of rectangles after the mouse event
;; STRATEGY: Use HOF map on lor
;; EXAMPLE:
;; (list-after-mouse-event EMPTY-LOR 205 150 "button-up")=EMPTY-LOR
;; (list-after-mouse-event LIST-WITH-ONE-SEL-RECT 205 145 "button-down")
;;    =LIST-WITH-TWO-SEL-RECT-AFTER-BUTTON-DOWN

(define (list-after-mouse-event lor mx my mev)
  (map
   ;;Rectangle -> Rectangle
   ;;GIVEN: a rectangle
   ;;RETURNS: the given rectangle after the mouse event specified by mev
   (lambda (r) (rect-after-mouse-event r mx my mev)) lor))
;..........................................................................
;; rect-after-mouse-event : Rectangle NonNegInt NonNegInt MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of mouse and the mouse event
;; RETURNS: a rectangles after the mouse event
;; STRATEGY: Dividing into cases based on MouseEvent
;; EXAMPLES:
;; (rect-after-mouse-event UNSEL-RECT-AT-CENTER 205 145 "move")=UNSEL-RECT-AT-CENTER
;; (rect-after-mouse-event UNSEL-RECT-AT-CENTER 205 145 "button-down")
;;       =SEL-RECT-AT-CENTER-AFTER-BUTTON-DOWN

(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down r mx my)]
    [(mouse=? mev "drag") (rect-after-drag r mx my)]
    [(mouse=? mev "button-up") (rect-after-button-up r mx my)]
    [else r]))

;TESTS:
(begin-for-test
  (check-equal? (rect-after-mouse-event UNSEL-RECT-AT-CENTER 205 145 "move")
                UNSEL-RECT-AT-CENTER
                "Invalid mouse event leaves the rectangle unchanged")
  (check-equal? (rect-after-mouse-event UNSEL-RECT-AT-CENTER 205 145 "button-down")
                SEL-RECT-AT-CENTER-AFTER-BUTTON-DOWN
                "selects the rectangle after button bown and stores mouse coordinates"))

;..........................................................................
;; rect-after-button-down : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and x and y coordinates of mouse
;; RETURNS: the rectangle following a button-down at the given location.
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES: see tests below

(define (rect-after-button-down r mx my)
  (if (and (in-rect? r mx my) (not (myrect-selected? r))) 
      (make-myrect (myrect-x r)
                   (myrect-y r)
                   (myrect-vx r)
                   (myrect-vy r)
                   true
                   mx
                   my
                   (myrect-pen? r))
      r))

;TESTS:
(begin-for-test
  (check-equal? (rect-after-button-down UNSEL-RECT-AT-CENTER 250 60)
                UNSEL-RECT-AT-CENTER
                "The mouse coordinates are no inside the rectangle, hence no changes are made")
  (check-equal? (rect-after-button-down UNSEL-RECT-AT-CENTER 205 145)
                SEL-RECT-AT-CENTER-AFTER-BUTTON-DOWN
                "The rectangle gets selected after button-down and mouse coordinates
                 are saved"))
;.....................................................................
;; rect-after-drag : Rectangle NonNegInt NonNegInt -> Rectangle
;; GIVEN: a rectangle and x and y coordinates of mouse
;; RETURNS: the rectangle following a drag at the given location
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES:
;;(rect-after-drag UNSEL-RECT-AT-CENTER 205 145)=UNSEL-RECT-AT-CENTER
;;(rect-after-drag SEL-NEW-RECT 205 145)=SEL-NEW-RECT-AFTER-DRAG

(define (rect-after-drag r mx my)
  (if (myrect-selected? r)
      (make-myrect
       (current-coordinate (myrect-x r) (myrect-mx r) mx)
       (current-coordinate (myrect-y r) (myrect-my r) my)
       (myrect-vx r)
       (myrect-vy r)
       true
       mx
       my
       (myrect-pen? r))
      r))

;TESTS:
(begin-for-test
  (check-equal? (rect-after-drag UNSEL-RECT-AT-CENTER 205 145)
                UNSEL-RECT-AT-CENTER
                "The rectangle is not selected and hence drag cannot be performed")
  
  (check-equal? (rect-after-drag SEL-NEW-RECT 205 145)
                SEL-NEW-RECT-AFTER-DRAG
                "the rectangle moves to specified drag position after drag event"))

;................................................................................
;; Helper funtions for drag
;................................................................................
;; current-coordinate : NonNegInt NonNegInt NonNegInt -> NonNegInt
;; GIVEN: a rectangle's x or y coordinate, previous and current x or y mouse coordinates
;;        respectively
;; RETURNS: the rectangle's updated x or y coordinates based on mouse drag
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (current-x 200 0 50)=250
;; (current-x 200 0 -50)=150

(define (current-coordinate r-coordinate pre-m m)
  (if (> m pre-m) 
      (+ r-coordinate (distance-to-drag pre-m m))
      (- r-coordinate (distance-to-drag pre-m m))))

;TESTS:
(begin-for-test
  (check-equal? (current-coordinate 200 0 50) 250
                "New mouse coordinate is greater than previous one, hence it is added to
                 the x-coordinate of the rectangle")
  (check-equal? (current-coordinate 200 0 -50) 150
                "New mouse coordinate is smaller than previous one, hence it is subtracted
                 from the x-coordinate of the rectangle")                        
  (check-equal? (current-coordinate 0 200 150) -50
                "New mouse coordinate is smaller than previous one, hence it is subtracted
                 from the y-coordinate of the rectangle")
  (check-equal? (current-coordinate 0 150 200) 50
                "New mouse coordinate is greater than previous one, hence it is added to
                 the y-coordinate of the rectangle"))   
;...................................................................................
;;distance-to-drag: NonNegInt NonNegInt -> NonNegInt
;GIVEN: previous and current mouse coordinates
;RETURNS: distance between them i.e distance the rectangle is dragged
;STRATEGY: Call simpler functions
;EXAMPLES: (distance-to-drag -100 50)=150
;          (distance-to-drag 20 -10)=30

(define (distance-to-drag p n)
  (abs (- n p)))
;...............................................................................
;; rect-after-button-up : myrect Integer Integer -> Rectangle
;; GIVEN: a rectangle and x and y coordinates of mouse
;; RETURNS: the rectangle following a button-up at the given location
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES:
;; (rect-after-button-up SEL-NEW-RECT-AFTER-DRAG 205 145)
;; UNSEL-NEW-RECT-AFTER-BUTTON-UP

(define (rect-after-button-up r mx my)
  (make-myrect (myrect-x r)
               (myrect-y r)
               (myrect-vx r)
               (myrect-vy r)
               false
               (myrect-mx r)
               (myrect-my r)
               (myrect-pen? r)))
;TEST
(begin-for-test
  (check-equal? (rect-after-button-up SEL-NEW-RECT-AFTER-DRAG 205 145)
                UNSEL-NEW-RECT-AFTER-BUTTON-UP
                "After button-up, rectangle is unselected anf the mouse coordinates
                 are stored"))
;.............................................................................
;; in-rect? : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and mouse coordinates
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given rectangle.
;; STRATEGY: Use template for myrect on r

(define (in-rect? r x y)
  (and
   (>= (+ (myrect-x r) COLLIDE-WEST-X) x)
   (>= (+ (myrect-y r) COLLIDE-NORTH-Y) y)
   (>= y (- (myrect-y r) COLLIDE-NORTH-Y))
   (>= x (- (myrect-x r) COLLIDE-WEST-X))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy) only if unselected
;; STRATEGY: Call simpler function
;; EXMAPLE:
;; (new-rectangle 100 50 12 -10)=(make-myrect 100 50 12 -10)

(define (new-rectangle x y vx vy)
  (make-myrect x y vx vy #f 0 0 #f))

;TEST:
(begin-for-test
  (check-equal? (new-rectangle 100 50 12 -10)
                (make-myrect 100 50 12 -10 #f 0 0 #f)
                "makes a new Rectangle with center at (100,50), velocity (12,-10)
                 which is UNSELECTED and without mouse coordinates"))
;..........................................................................
;; rect-x : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: x-coordinate of the given rectangle
;; STRATEGY: Use template for Rectangle on r
;; EXMAPLE:
;; (rect-x (new-rectangle 100 50 12 -10))=100

(define (rect-x r)
  (myrect-x r))

;TEST:
(begin-for-test
  (check-equal? (rect-x (new-rectangle 100 50 12 -10)) 100 
                "100 is the x-coordinate of the given rectangle"))
;.................................................................................
;; rect-y : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: y-coordinate of the given rectangle
;; STRATEGY: Use template for Rectangle on r
;; EXMAPLE:
;; (rect-y (new-rectangle 100 50 12 -10))=50

(define (rect-y r)
  (myrect-y r))

;TEST:
(begin-for-test
  (check-equal? (rect-y (new-rectangle 100 50 12 -10)) 50 
                "50 is the x-coordinate of the given rectangle"))
;.....................................................................................
;; rect-vx : Rectangle -> Integer
;; GIVEN: a rectangle
;; RETURNS: the velocity of the center of the given rectangle in x-axis direction
;; STRATEGY: Use template for Rectangle on r
;; EXMAPLE:
;; (rect-vx (new-rectangle 100 50 12 -10))=12

(define (rect-vx r)
  (myrect-vx r))

;TEST:
(begin-for-test
  (check-equal? (rect-vx (new-rectangle 100 50 12 -10)) 12 
                "12 is the velocity of the center of the given rectangle in
                x-axis direction"))
;.....................................................................................
;; rect-vy : Rectangle -> Integer
;; GIVEN: a rectangle
;; RETURNS: the velocity of the center of the given rectangle in y-axis direction
;; STRATEGY: Use template for Rectangle on r
;; EXMAPLE:
;; (rect-vy (new-rectangle 100 50 12 -10))=-10

(define (rect-vy r)
  (myrect-vy r))

;TEST:
(begin-for-test
  (check-equal? (rect-vy (new-rectangle 100 50 12 -10)) -10 
                "-10 is the velocity of the center of the given rectangle in
                y-axis direction"))
;.......................................................................
;; rect-selected? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: true if the given rectangle is selected.
;; STRATEGY: USe template for Rectangle on r
;; EXAMPLE:
; (rect-selected? (make-myrect 100 50 12 -10 #true 0 0))=#true

(define (rect-selected? r)
  (myrect-selected? r))

;TEST
(begin-for-test
  (check-equal? (rect-selected? (make-myrect 100 50 12 -10 #t 0 0 #t))
                #true
                "The rectangle is selected"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene : WorldState -> Scene
;; GIVEN: a world state
;; RETURNS: tranlation of the given world state into a scene
;; STRATEGY: Use template for WorldState on w
;; EXAMPLES:
;; (world-to-scene PAUSED-WORLD-WITH-ONE-RECT)
;; =(place-image I-UNSEL-RECT-AT-CENTER (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) EMPTY-CANVAS)

(define (world-to-scene w)
  (display-world (world-lor w) (world-lod w)))

;TEST
(begin-for-test
  (check-equal? (world-to-scene PAUSED-WORLD-WITH-ONE-RECT)
                (place-image I-UNSEL-RECT-AT-CENTER
                             (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) EMPTY-CANVAS)
                "A stationary rectagle is created at the center of the canvas"))
;....................................................................................
;; display-world : ListOfRectangles ListOfDots -> Scene
;; GIVEN: a list of rectangles and a list of dots
;; RETURNS: tranlation of the given world state into a scene
;; STRATEGY: Use HOF foldr on lor
(define (display-world lor lod)
  (foldr
   place-rect
   (pass-dots lod)
   lor))
;..........................................................................
;; pass-rect : ListOfRectangles -> Scene
;; GIVEN: a list of rectangles
;; RETURNS: tranlation of the list of rectangles into a scene
;; STRATEGY: Use HOF foldr on lor

(define (pass-rect lor)
  (foldr
   place-rect
   EMPTY-CANVAS
   lor))

;;TESTS:
(begin-for-test
  (check-equal? (pass-rect empty) EMPTY-CANVAS
                "No rectangles are placed on the canvas")
  (check-equal? (pass-rect LIST-OF-UNSEL-ONE-RECT)
                (place-image I-UNSEL-RECT-AT-CENTER (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
                             EMPTY-CANVAS)
                "Places the rectangle(s) in list on the canvas"))
;...........................................................................
;; pass-dots : ListOfDots -> Scene
;; GIVEN: a list of dots
;; RETURNS: tranlation of the list of rectangles into a scene
;; STRATEGY: Use HOF foldr on lod

(define (pass-dots lod)
  (foldr
   place-dots
   EMPTY-CANVAS
   lod))

;;TESTS:
(begin-for-test
  (check-equal? (pass-dots empty) EMPTY-CANVAS
                "No rectangles are placed on the canvas")
  (check-equal? (pass-dots LOD1)
                (place-image PEN-DOT (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
                             EMPTY-CANVAS))
  "A dot is placed at the center of the canvas")
;...........................................................................
;; place-dots : Pendot Scene -> Scene
;; GIVEN: a pen dot and a scene
;; RETURNS: a scene similar to given one but with the given dot placed in it
;; STRATEGY:Use template for ListOfDots on lod
(define (place-dots d s)
  (place-image PEN-DOT (pdots-dx d) (pdots-dy d) s))

;..................................................................................
;..................................................................................
;; place-rect : Rectangle Scene -> Scene
;; GIVEN: a rectangle and a scene
;; RETURNS: a scene similar to given one but with the given rectangle placed in it
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES:
;; (place-rect SEL-RECT-AT-CENTER EMPTY-CANVAS)
;; =(place-image I-SEL-RECT-AT-CENTER (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) EMPTY-CANVAS)

(define (place-rect r s)
  (if (myrect-selected? r)
      (place-selected-rect r s)
      (place-unselected-rect r s)))

;TESTS
(begin-for-test
  (check-equal? (place-rect SEL-RECT-AT-CENTER EMPTY-CANVAS)
                (place-image CIRCLE-AT-MOUSE-POINT 0 0
                             (place-image I-SEL-RECT-AT-CENTER
                                          (/ CANVAS-WIDTH 2)
                                          (/ CANVAS-HEIGHT 2)
                                          EMPTY-CANVAS))
                "A selected i.e red rectangle is placed at the center of the canvas"))
;..................................................................................
;; place-selected-rect : Rectangle Scene -> Scene
;; GIVEN: a rectangle that is selected and the scene
;; RETURNS: a scene similar to given one but with the given rectangle placed in it
;; STRATEGY: Use template for Rectangle on r
(define (place-selected-rect r s)
  (place-image CIRCLE-AT-MOUSE-POINT
               (myrect-mx r)
               (myrect-my r)
               (place-image (append-vel-text r RECT-CLICKED SEL-COLOR)
                            (myrect-x r)
                            (myrect-y r)
                            s)))
;..................................................................................
;; place-unselected-rect : Rectangle Scene -> Scene
;; GIVEN: a rectangle that is not selected and the scene
;; RETURNS: a scene similar to given one but with the given rectangle placed in it
;; STRATEGY: Use template for Rectangle on r
(define (place-unselected-rect r s)
  (place-image (append-vel-text r RECT UNSEL-COLOR)
               (myrect-x r)
               (myrect-y r)
               s))
;..................................................................................
;; append-vel-text : Rectangle Image String-> Image
;; GIVEN: a rectangle, an image of the rectangle and the color based on whether rectangle
;;        is selected or not
;; RETURNS: an image of the rectangle with its velocity printed above it
;; STRATEGY: Combine simpler functions
(define (append-vel-text r rec c)
  (overlay (text (get-string-vel r) VEL-FONT c) rec))
;..................................................................................
;; get-string-vel : Rectangle -> String
;; GIVEN: a rectangle
;; RETURNS: the string (vx,vy) describing the velocity of the center of the rectangle
;; STRATEGY: Use template for Rectangle on r
(define (get-string-vel r)
  (string-append "(" (number->string (myrect-vx r)) "," (number->string (myrect-vy r)) ")"))
