;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Screensaver.  
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
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA DEFINITIONS;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct myrectangle (x y vx vy selected? mx my))
;;A Myrectangle is a (make-myrectangle Integer Integer Integer Integer Boolean Integer Integer)
;;INTERPRETATION:
;;x is the x-coordinate(in pixels) and y is the y-coordinate(in pixels)
;;     such that (x,y) represents the center of the rectangle.
;;vx is the velocity of the center of rectangle in x direction and
;;vy is the velocity of the center of rectangle in y direction
;; selected? describes whether or not the rectangle is selected.
;;mx is the x coordinate determined by the mouse pointer
;;my is the y coordinate determined by the mouse pointer
;;    such that (mx,my) represent the point where mouse is pointing.

;; template:
;; myrectangle-fn : Myrectangle -> ??
;(define (myrectangle-fn r)
;  (... (myrectangle-x r)
;       (myrectangle-y r)
;       (myrectangle-vx r)
;       (myrectangle-vy r)
;       (myrectangle-selected? r)
;       (myrectangle-mx r)
;       (myrectangle-my r)
;))


(define-struct world (rect1 rect2 paused?))
;; A World is a (make-world Myrectangle Myrectangle Boolean)
;; INTERPRETATION: 
;; rect1 and rect2 are the two rectangles
;; paused? describes whether or not the rectangle is paused.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-rect1 w)
;       (world-rect2 w)
;       (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONSTANTS
;;deimensions of rectangle
(define RECT-HEIGHT 50)
(define RECT-WIDTH 60)
(define RECT1 (overlay (text "(-12,20)" 12 "blue")
                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue")))
(define RECT2 (overlay (text "(23,-14)" 12 "blue")
                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue")))

(define RECT1-CLICKED (overlay (text "(-12,20)" 12 "red")
                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "red")))
(define RECT2-CLICKED (overlay (text "(23,-14)" 12 "red")
                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "red")))

;;Information about rectangle 1

(define RECT1-I-x 200)
(define RECT1-I-y 100)

(define RECT1-I-vx -12)
(define RECT1-I-vy 20)

;;Information about rectangle 2

(define RECT2-I-x 200)
(define RECT2-I-y 200)

(define RECT2-I-vx 23)
(define RECT2-I-vy -14)

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
(define INITIAL-RECT1 (make-myrectangle RECT1-I-x RECT1-I-y RECT1-I-vx RECT1-I-vy
                                        false 0 0))
(define INITIAL-RECT2 (make-myrectangle RECT2-I-x RECT2-I-y RECT2-I-vx RECT2-I-vy
                                        false 0 0))
(define SELECTED-INITIAL-RECT1 (make-myrectangle RECT1-I-x RECT1-I-y RECT1-I-vx RECT1-I-vy
                                        true 0 0))
(define SELECTED-INITIAL-RECT2 (make-myrectangle RECT2-I-x RECT2-I-y RECT2-I-vx RECT2-I-vy
                                        true 0 0))


;; examples of myrectangle, for testing
(define rect1-at-original INITIAL-RECT1)
(define rect2-at-original INITIAL-RECT2)

(define unselected-rect1-at-next-state (make-myrectangle 188 120 -12 20 false 0 0))
(define unselected-rect2-at-next-state (make-myrectangle 223 186 23 -14 false 0 0))

;; examples of worlds, for testing
(define paused-world-at-original
  (make-world
    rect1-at-original
    rect2-at-original
    true))

(define unpaused-world-at-original
  (make-world
    rect1-at-original
    rect2-at-original
    false))

(define unpaused-world-at-first-tick
  (make-world
    unselected-rect1-at-next-state
    unselected-rect2-at-next-state
    false))

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN FUNCTION.

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver simulation-speed)
  (big-bang (make-world INITIAL-RECT1 INITIAL-RECT2 true)
            (on-tick world-after-tick simulation-speed)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; STATEGY: Call simpler functions
;; EXAMPLE:
;; (initial-world 3)=(make-world (make-myrectangle 200 100 -12 20 false 0 0)
;                                (make-myrectangle 200 200 23 -14 false 0 0)
;                                #true)
(define (initial-world n)
  (make-world INITIAL-RECT1 INITIAL-RECT2 true)
  )

;TEST:
(begin-for-test
  (check-equal? (initial-world 3)
                (make-world (make-myrectangle 200 100 -12 20 false 0 0)
                             (make-myrectangle 200 200 23 -14 false 0 0)
                             #true)
                "Initialises the rectangle at its initial coordinates and
                 constant velocity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; STRATEGY: Use template for World on w
;; EXAMPLES:
;; (world-after-tick paused-world-at-original)
; =(make-world (make-myrectangle 200 100 -12 20 #false 0 0)
;              (make-myrectangle 200 200 23 -14 #false 0 0) #true)
;; (world-after-tick unpaused-world-at-original)
; =(make-world (make-myrectangle 188 120 -12 20 #false 0 0)
;              (make-myrectangle 223 186 23 -14 #false 0 0) #false)
;; (world-after-tick unpaused-world-at-first-tick)
; =(make-world (make-myrectangle 176 140 -12 20 #false 0 0)
;              (make-myrectangle 246 172 23 -14 #false 0 0) #false)

(define (world-after-tick w)
(if (world-paused? w)
    w
    (make-world
     (rect-after-tick (world-rect1 w))
     (rect-after-tick (world-rect2 w))
     (world-paused? w))))

;TESTS:
(begin-for-test
  (check-equal? (world-after-tick paused-world-at-original)
    (make-world (make-myrectangle 200 100 -12 20 #false 0 0)
                (make-myrectangle 200 200 23 -14 #false 0 0) #true)
    "Paused world stays paused in same state even after tick")
  (check-equal? (world-after-tick unpaused-world-at-original)
   (make-world (make-myrectangle 188 120 -12 20 #false 0 0)
               (make-myrectangle 223 186 23 -14 #false 0 0) #false)
   "Unpaused world at original transits to next state after one tick")
  (check-equal? (world-after-tick unpaused-world-at-first-tick)
   (make-world (make-myrectangle 176 140 -12 20 #false 0 0)
               (make-myrectangle 246 172 23 -14 #false 0 0) #false)
   "Unpaused world transits to next state after one tick"))
;........................................................................
;; rect-after-tick : Myrectangle -> Myrectangle
;; Given: rectangle 1
;; RETURNS: the state of the given rectangle 1 after a tick in an unpaused world
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-after-tick rect1-at-original)=(make-myrectangle 188 120 -12 20 #false 0 0)
;(rect-after-tick (make-myrectangle 176 140 -12 20 true 180 150))
;=(make-myrectangle 176 140 -12 20 #true 180 150)

(define (rect-after-tick r)
(if (myrectangle-selected? r)
    r
  (cond
    ((rect-collides-corner? r) (rect-proceeds (rect-bounces-from-corner r)))
    ((rect-collides-y? r) (rect-proceeds (rect-bounces-y r)))
    ((rect-collides-x? r) (rect-proceeds (rect-bounces-x r)))
    (else (rect-proceeds r)))))

;TESTS
(begin-for-test
  (check-equal? (rect-after-tick rect1-at-original)
                (make-myrectangle 188 120 -12 20 #false 0 0)
                "Rectangle changes its coordinates (200,100) to (188,120)
                 after a tick")
  (check-equal? (rect-after-tick unselected-rect1-at-next-state)
                (make-myrectangle 176 140 -12 20 #false 0 0)
                "Rectangle changes its coordinates (176,140) to (164,160)
                 after a tick"))           

;...........................................................................
;; rect-collides-corner? : Myrectangle -> Boolean
;; Given: a rectangle to be moved at next tick
;; RETURNS: true if the rectangle collides with the corner of canvas
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
;; (rect-collides-corner? (make-myrectangle 500 500 -12 20 #false 0 0))=#true
;; (rect-collides-corner? (make-myrectangle COLLIDE-NORTH-Y COLLIDE-EAST-X -12 20
;                         #true 30 370))
;   =#true
;; (rect-collides-corner? (make-myrectangle 176 140 -12 20 #false 0 0))=#false

(define (rect-collides-corner? r)
  (or
   (and (>= (myrectangle-x r) COLLIDE-EAST-X) (>= (myrectangle-y r) COLLIDE-SOUTH-Y))
   (and (<= (myrectangle-x r) COLLIDE-WEST-X) (>= (myrectangle-y r) COLLIDE-SOUTH-Y))
   (and (<= (myrectangle-x r) COLLIDE-WEST-X) (<= (myrectangle-y r) COLLIDE-NORTH-Y))
   (and (>= (myrectangle-x r) COLLIDE-EAST-X) (<= (myrectangle-y r) COLLIDE-NORTH-Y))
   ))

;TESTS:
 (begin-for-test
  (check-equal? (rect-collides-corner? (make-myrectangle 500 500 -12 20 #false 0 0))
                #true
                "The rectangle collides/goes out of the canvas")
  (check-equal? (rect-collides-corner?(make-myrectangle COLLIDE-NORTH-Y COLLIDE-EAST-X
                                      -12 20 #true 30 370))
                #true
                "The rectangle collides with the canvas")
  (check-equal? (rect-collides-corner? (make-myrectangle 176 140 -12 20 #false 0 0))
                #false
                "The rectangle does not collide and hence is in the canvas"))
;............................................................................
;; rect-collides-y? : Myrectangle -> Boolean
;; Given: a rectangle to be moved at next tick
;; RETURNS: true if the rectangle collides with the x-axis boundries i.e vertically
;;          collides the canvas
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
;; (rect-collides-y? (make-myrectangle 15 COLLIDE-NORTH-Y -12 20 #false 0 0))=#true
;; (rect-collides-y? (make-myrectangle 15 (+ COLLIDE-NORTH-Y 1) -12 20 #false 0 0))=#false
;; (rect-collides-y? (make-myrectangle 15 COLLIDE-SOUTH-Y -12 20 #false 0 0))=#true
;; (rect-collides-y? (make-myrectangle 15 (- COLLIDE-SOUTH-Y 1) -12 20 #false 0 0))=#false

(define (rect-collides-y? r)
  (or (>= (myrectangle-y r) COLLIDE-SOUTH-Y)
      (<= (myrectangle-y r) COLLIDE-NORTH-Y)))

;TESTS:
(begin-for-test
  (check-equal? (rect-collides-y? (make-myrectangle 15 COLLIDE-NORTH-Y -12 20 #false 0 0))
                #true
                "The rectangle collides with the north wall of canvas")
  (check-equal? (rect-collides-y? (make-myrectangle 15 (+ COLLIDE-NORTH-Y 1) -12 20
                                  #false 0 0))
                #false
                "The rectangle does not collide with the north wall of canvas")
  (check-equal? (rect-collides-y? (make-myrectangle 15 COLLIDE-SOUTH-Y -12 20 #false 0 0))
                #true
                "The rectangle collides with the south wall of canvas")
  (check-equal? (rect-collides-y? (make-myrectangle 15 (- COLLIDE-SOUTH-Y 1) -12 20
                                   #false 0 0))
                #false
                "The rectangle does not collide with the south wall of canvas"))

;............................................................................
;; rect-collides-x? : Myrectangle -> Boolean
;; Given: a rectangle to be moved at next tick
;; RETURNS: true if the rectangle collides with the y-axis boundries i.e horizontally
;;          collides the canvas
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
;; (rect-collides-x? (make-myrectangle COLLIDE-WEST-X 15 -12 20 #false 0 0))=#true
;; (rect-collides-x? (make-myrectangle (+ COLLIDE-WEST-X 1) 15 -12 20 #false 0 0))=#false
;; (rect-collides-x? (make-myrectangle COLLIDE-EAST-X 20 -12 20 #false 0 0))=#true
;; (rect-collides-x? (make-myrectangle (- COLLIDE-EAST-X 1) 20 -12 20 #false 0 0))=#false

(define (rect-collides-x? r)
  (or (<= (myrectangle-x r) COLLIDE-WEST-X)
      (>= (myrectangle-x r) COLLIDE-EAST-X)))

;TESTS:
(begin-for-test
  (check-equal? (rect-collides-x? (make-myrectangle COLLIDE-WEST-X 15 -12 20 #false 0 0))
                #true
                "The rectangle collides with the west wall of canvas")
  (check-equal? (rect-collides-x? (make-myrectangle (+ COLLIDE-WEST-X 1) 15 -12 20
                                   #false 0 0))
                #false
                "The rectangle does not collide with the west wall of canvas")
  (check-equal? (rect-collides-x? (make-myrectangle COLLIDE-EAST-X 20 -12 20 #false 0 0))
                #true
                "The rectangle collides with the east wall of canvas")
  (check-equal? (rect-collides-x? (make-myrectangle (- COLLIDE-EAST-X 1) 20 -12 20
                                  #false 0 0))
                #false
                "The rectangle does not collide with the east wall of canvas"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-bounces-from-corner : Myrectangle ->  Myrectangle 
;; Given: a rectangle that collides any corner of the canvas
;; RETURNS: a rectangle that bounces back after collision
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-bounces-from-corner (make-myrectangle COLLIDE-NORTH-Y COLLIDE-EAST-X -12 20
;                           #false 0 0))=(make-myrectangle 25 370 12 -20)
(define (rect-bounces-from-corner r)
  (make-myrectangle
    (myrectangle-x r)
    (myrectangle-y r)
    (- (myrectangle-vx r))
    (- (myrectangle-vy r))
    (myrectangle-selected? r)
    (myrectangle-mx r)
    (myrectangle-my r)))

;TEST:
(begin-for-test
  (check-equal? (rect-bounces-from-corner (make-myrectangle COLLIDE-NORTH-Y
                                           COLLIDE-EAST-X -12 20 #false 0 0))
                (make-myrectangle 25 370 12 -20 #false 0 0)
                "The rectangle bounces back from northeast corner i.e the vx and vy
                velocities are inversed"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-bounces-y : Myrectangle ->  Myrectangle 
;; Given: a rectangle that collides with the x-axis boundaries of the canvas
;; RETURNS: a rectangle that bounces back in vertical direction after collision
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-bounces-y (make-myrectangle COLLIDE-NORTH-Y 15 -12 20 #false 0 0))
; =(make-myrectangle 25 15 -12 -20)
; (rect-bounces-y (make-myrectangle COLLIDE-SOUTH-Y 15 23 -14 #false 0 0))
; =(make-myrectangle 275 15 23 14)

(define (rect-bounces-y r)
  (make-myrectangle
    (myrectangle-x r)
    (myrectangle-y r)
    (myrectangle-vx r)
    (- (myrectangle-vy r))
    (myrectangle-selected? r)
    (myrectangle-mx r)
    (myrectangle-my r)))

;TESTS:
(begin-for-test
  (check-equal? (rect-bounces-y (make-myrectangle COLLIDE-NORTH-Y 15 -12 20 #false 0 0))
                (make-myrectangle 25 15 -12 -20 #false 0 0)
                "The rectangle bounces back from north wall i.e the vy velocity
                is reversed")
  (check-equal? (rect-bounces-y (make-myrectangle COLLIDE-SOUTH-Y 15 23 -14 #false 0 0))
                (make-myrectangle 275 15 23 14 #false 0 0)
                "The rectangle bounces back from south wall i.e the vy velocity
                is reversed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-bounces-x : Myrectangle ->  Myrectangle 
;; Given: a rectangle that collides with the y-axis boundaries of the canvas
;; RETURNS: a rectangle that bounces back in horizontal direction after collision
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-bounces-x (make-myrectangle 15 COLLIDE-WEST-X -12 20 #false 0 0))
; =(make-myrectangle 15 30 12 20 #false 0 0)
; (rect-bounces-x (make-myrectangle 20 COLLIDE-EAST-X 23 -14 #false 0 0))
; =(make-myrectangle 20 370 -23 -14 #false 0 0)

(define (rect-bounces-x r)
  (make-myrectangle
    (myrectangle-x r)
    (myrectangle-y r)
    (- (myrectangle-vx r))
    (myrectangle-vy r)
    (myrectangle-selected? r)
    (myrectangle-mx r)
    (myrectangle-my r)))
  

;TESTS:
(begin-for-test
  (check-equal? (rect-bounces-x (make-myrectangle 15 COLLIDE-WEST-X -12 20 #false 0 0))
                (make-myrectangle 15 30 12 20 #false 0 0)
                "The rectangle bounces back from west wall i.e the vx velocity
                is reversed")
  (check-equal? (rect-bounces-x (make-myrectangle 20 COLLIDE-EAST-X 23 -14 #false 0 0))
                (make-myrectangle 20 370 -23 -14 #false 0 0)
                "The rectangle bounces back from east wall i.e the vx velocity
                is reversed"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-proceeds : Myrectangle ->  Myrectangle 
;; Given: a rectangle 
;; RETURNS: a rectangle that proceeds in the direction of its velocity
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-proceeds (make-myrectangle 15 10 -12 20 #false 0 0))
; =(make-myrectangle 3 30 -12 20#false 0 0)
; (rect-proceeds (make-myrectangle 15 COLLIDE-WEST-X -12 20 #false 0 0))
; =(make-myrectangle 3 50 -12 20 #false 0 0)
(define (rect-proceeds r)
  (make-myrectangle
   (+ (myrectangle-x r) (myrectangle-vx r))
   (+ (myrectangle-y r) (myrectangle-vy r))
   (myrectangle-vx r)
   (myrectangle-vy r)
   (myrectangle-selected? r)
   (myrectangle-mx r)
    (myrectangle-my r)))

 ;TESTS:
(begin-for-test
  (check-equal? (rect-proceeds (make-myrectangle 15 10 -12 20 #false 0 0))
                (make-myrectangle 3 30 -12 20 #false 0 0)
                "Rectangle proceeds from (15,10) to (3,30)")
  (check-equal? (rect-proceeds (make-myrectangle 15 COLLIDE-WEST-X -12 20 #false 0 0))
                (make-myrectangle 3 50 -12 20 #false 0 0)
                "Rectangle proceeds from (15,30) to (3,50)"))   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN:  a World State and a Keyevent
;; RETURNS: the World State that should follow the given worldstate
;; after the given keyevent
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;  (world-after-key-event unpaused-world-at-first-tick pause-key-event)
;=(make-world (make-myrectangle 188 120 -12 20 #false 0 0)
;             (make-myrectangle 223 186 23 -14 #false 0 0) #true)
;  (world-after-key-event unpaused-world-at-first-tick non-pause-key-event)
;=(make-world (make-myrectangle 188 120 -12 20 #false 0 0)
;             (make-myrectangle 223 186 23 -14 #false 0 0) #false)


(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

;TEST:
(begin-for-test
  (check-equal? (world-after-key-event unpaused-world-at-first-tick pause-key-event)
                (make-world (make-myrectangle 188 120 -12 20 #false 0 0)
                (make-myrectangle 223 186 23 -14 #false 0 0) #true)
                "unpaused world pauses after a valid keyevent")
  (check-equal? (world-after-key-event unpaused-world-at-first-tick non-pause-key-event)
                (make-world (make-myrectangle 188 120 -12 20 #false 0 0)
                (make-myrectangle 223 186 23 -14 #false 0 0) #false)
                "unpaused world does not paused after an invalid keyevent"))
;........................................................................................

;; world-with-paused-toggled : World -> World
;; GIVEN: a world
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template for World on w
;; EXAMPLE:
;(world-with-paused-toggled unpaused-world-at-first-tick)
; =(make-world (make-myrectangle 188 120 -12 20 #false 0 0)
;              (make-myrectangle 223 186 23 -14 #false 0 0) #true)
;(world-with-paused-toggled paused-world-at-original)
; =(make-world (make-myrectangle 200 100 -12 20 #false 0 0)
;              (make-myrectangle 200 200 23 -14 #false 0 0) #false)

(define (world-with-paused-toggled w)
  (make-world
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w))))

;TESTS:
(begin-for-test
  (check-equal? (world-with-paused-toggled unpaused-world-at-first-tick)
  (make-world (make-myrectangle 188 120 -12 20 #false 0 0)
              (make-myrectangle 223 186 23 -14 #false 0 0) #true)
  "unpaused world returns true after being paused")
  (check-equal? (world-with-paused-toggled paused-world-at-original)
  (make-world (make-myrectangle 200 100 -12 20 #false 0 0)
              (make-myrectangle 200 200 23 -14 #false 0 0) #false)
  "paused world returns false after being unpaused"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event
;; WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; STRATEGY: Use template for WorldState on w

(define (world-after-mouse-event w mx my mev)
  (make-world
    (rect-after-mouse-event (world-rect1 w) mx my mev)
    (rect-after-mouse-event (world-rect2 w) mx my mev)
    (world-paused? w)))
;..............................................................................
;; rect-after-mouse-event :  Myrectangle Int Int MouseEvent -> Myrectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event
;; strategy: Cases on mouse event mev
(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down r mx my)]
    [(mouse=? mev "drag") (rect-after-drag r mx my)]
    [(mouse=? mev "button-up") (rect-after-button-up r mx my)]
    [else r]))
;..............................................................................
;; rect-after-button-down : Myrectangle Integer Integer -> Myrectangle
;; GIVEN: a rectangle and x and y coordinates of mouse
;; RETURNS: the rectangle following a button-down at the given location.
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES: see tests below
(define (rect-after-button-down r mx my)
  (if (in-rect? r mx my)
      (make-myrectangle (myrectangle-x r) (myrectangle-y r)
                        (myrectangle-vx r) (myrectangle-vy r) true mx my)
      r))

;TESTS:
(begin-for-test

  ;; button-down:
  ;; button-down inside rect1
  (check-equal?
    (world-after-mouse-event 
      (make-world
        INITIAL-RECT1
        INITIAL-RECT2
        false)
        201 105    ;; a coordinate inside rect1
      "button-down")
    (make-world (make-myrectangle 200 100 -12 20 #true 201 105)
                (make-myrectangle 200 200 23 -14 #false 0 0)
                #false)
    "button down inside rect1 selects it")

  ;; button-down inside rect2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        INITIAL-RECT1
        INITIAL-RECT2
        false)
        201 210    ;; a coordinate inside rect2
      "button-down")
    (make-world
     (make-myrectangle 200 100 -12 20 #false 0 0)
     (make-myrectangle 200 200 23 -14 #true 201 210)   ;selected rect2
     #false)
    "button down inside rect2 selects it")

  ;; button-down not inside any cat
  (check-equal?
    (world-after-mouse-event 
      (make-world
        INITIAL-RECT1
        INITIAL-RECT2
        false)
        250 25    ;; a coordinate not inside rect1 and rect2
      "button-down")
    (make-world (make-myrectangle 200 100 -12 20 #false 0 0)
                (make-myrectangle 200 200 23 -14 #false 0 0)
                #false)
    "button down outside any rectangle leaves world unchanged"))
;................................................................................ 
;; rect-after-drag : Myrectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and x and y coordinates of mouse
;; RETURNS: the rectangle following a drag at the given location
;; STRATEGY: Use template for Rectangle on r
;; EXAMPLES: see tests below
(define (rect-after-drag r mx my)
  (if (myrectangle-selected? r)
      (make-myrectangle
       (current-x (myrectangle-x r) (myrectangle-mx r) mx)
       (current-y (myrectangle-y r) (myrectangle-my r) my)
       (myrectangle-vx r)
       (myrectangle-vy r)
        true
        mx
        my)
      r))

;TESTS:
(begin-for-test
 ;; no rect selected: drag should not change anything
  (check-equal?
    (world-after-mouse-event
      (make-world
        INITIAL-RECT1
        INITIAL-RECT2
        false)
        250 30    ;; a large motion
      "drag")
    (make-world
     (make-myrectangle 200 100 -12 20 #false 0 0)
     (make-myrectangle 200 200 23 -14 #false 0 0)
     #false)
    "drag with no rect selected leaves world unchanged")
    
;; rect1 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        SELECTED-INITIAL-RECT1
        INITIAL-RECT2
        false)
        201 105       ;; a large motion
      "drag")
    (make-world (make-myrectangle 401 205 -12 20 #true 201 105)
                (make-myrectangle 200 200 23 -14 #false 0 0)
                #false)
    "drag when rect1 is selected should just move rect1")

  ;; rect2 selected in unpaused world
  (check-equal?
    (world-after-mouse-event
      (make-world
        INITIAL-RECT1
        SELECTED-INITIAL-RECT2
        true)
        210 215
      "drag")
    (make-world (make-myrectangle 200 100 -12 20 #false 0 0)
                (make-myrectangle 410 415 23 -14 #true 210 215)
                #true)
    "drag when rect2 is selected should just move rect2"))
;................................................................................
;Helper funtions for drag

;; current-x : Myrectangle Integer Integer Integer-> Myrectangle
;; GIVEN: a rectangle with its x-coordinate, previous and current mouse coordinates
;; RETURNS: the rectangle's updated x-coordinates based on mouse drag
;; STRATEGY: Combine simpler functions 
(define (current-x x pre-mx mx)
  (if (> mx pre-mx) 
  (+ x (distance-to-drag pre-mx mx))
  (- x (distance-to-drag pre-mx mx))))

;;distance-to-drag: Integer Integer -> PosInt
;GIVEN: previous and current mouse coordinates
;RETURNS: distance between them i.r distance the rectangle is dragged
;STRATEGY: Call simpler functions
(define (distance-to-drag p n)
  (abs (- n p)))

;; current-y : Myrectangle Integer Integer Integer-> Myrectangle
;; GIVEN: a rectangle with its y-coordinate, previous and current mouse coordinates
;; RETURNS: the rectangle's updated y-coordinates based on mouse drag
;; STRATEGY: Combine simpler functions 
  (define (current-y y pre-my my)
  (if(> my pre-my)
  (+ y (distance-to-drag pre-my my))
  (- y (distance-to-drag pre-my my))))

;...................................................................................
;; rect-after-button-up : Myrectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and x and y coordinates of mouse
;; RETURNS: the rectangle following a button-up at the given location
;; STRATEGY: Use template for Rectangle on r
;;EXAMPLES: see tests below
(define (rect-after-button-up r mx my)
      (make-myrectangle (myrectangle-x r) (myrectangle-y r)
                        (myrectangle-vx r) (myrectangle-vy r)
                        false (myrectangle-mx r) (myrectangle-my r))
      )
;TESTS:
  ;; button-up always unselects both rects
(begin-for-test
  ;; unselect rect1
  (check-equal?
    (world-after-mouse-event
      (make-world
        SELECTED-INITIAL-RECT1
        INITIAL-RECT2
        true)
        201 105    ;; arbitrary location
      "button-up")
   (make-world (make-myrectangle 200 100 -12 20 #false 0 0)
               (make-myrectangle 200 200 23 -14 #false 0 0)
               #true)
    "button-up unselects rect1")

  ;; unselect rect2
  (check-equal?
    (world-after-mouse-event
      (make-world
        INITIAL-RECT1
        SELECTED-INITIAL-RECT2
        true)
        201 105    ;; arbitrary location
      "button-up")
    (make-world (make-myrectangle 200 100 -12 20 #false 0 0)
                (make-myrectangle 200 200 23 -14 #false 0 0)
                #true)
    "button-up unselects rect2"))
;......................................................................................
;; in-rect? : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and mouse coordinates
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given rectangle.
;; STRATEGY: Use template for Myrectangle on r
(define (in-rect? r x y)
  (and
    (>= (+ (myrectangle-x r) COLLIDE-WEST-X) x)
    (>= (+ (myrectangle-y r) COLLIDE-NORTH-Y) y)
    (>= y (- (myrectangle-y r) COLLIDE-NORTH-Y))
    (>= x (- (myrectangle-x r) COLLIDE-WEST-X))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Myrectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy)only if unselected
;; STRATEGY: Call simpler function
;; EXMAPLE:
;; (new-rectangle 100 50 12 -10)=(make-myrectangle 100 50 12 -10)

(define (new-rectangle x y vx vy)
  
  (make-myrectangle x y vx vy #false 0 0))

;TEST:
(begin-for-test
  (check-equal? (new-rectangle 100 50 12 -10)
                (make-myrectangle 100 50 12 -10 #false 0 0)
                "makes a new Myrectangle with center at (100,50), velocity (12,-10)
                 which is UNSELECTED and without mouse coordinates"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-selected? : Myrectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: true iff the given rectangle is selected.
;; STRATEGY: USe template for Myrectangle on r
;; EXAMPLE:
; (rect-selected? (make-myrectangle 100 50 12 -10 #true 0 0))=#true

(define (rect-selected? r)
  (myrectangle-selected? r))

;TEST
(begin-for-test
  (check-equal? (rect-selected? (make-myrectangle 100 50 12 -10 #true 0 0))
                #true
                "The rectangle is selected"))
;..........................................................................
;; rect-x : Myrectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: x-coordinate of the given rectangle
;; STRATEGY: Use template for Myrectangle on r
;; EXMAPLE:
;; (rect-x (new-rectangle 100 50 12 -10))=100
(define (rect-x r)
  (myrectangle-x r))
;TEST:
(begin-for-test
  (check-equal? (rect-x (new-rectangle 100 50 12 -10)) 100 
                "100 is the x-coordinate of the given rectangle"))
;.................................................................................
;; rect-y : Myrectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: y-coordinate of the given rectangle
;; STRATEGY: Use template for Myrectangle on r
;; EXMAPLE:
;; (rect-y (new-rectangle 100 50 12 -10))=50

(define (rect-y r)
  (myrectangle-y r))
;TEST:
(begin-for-test
  (check-equal? (rect-y (new-rectangle 100 50 12 -10)) 50 
                "50 is the x-coordinate of the given rectangle"))
;.....................................................................................
;; rect-vx : Myrectangle -> Int
;; GIVEN: a rectangle
;; RETURNS: the velocity of the center of the given rectangle in x-axis direction
;; STRATEGY: Use template for Myrectangle on r
;; EXMAPLE:
;; (rect-vx (new-rectangle 100 50 12 -10))=12

;; rect-vx : Rectangle -> Int
(define (rect-vx r)
  (myrectangle-vx r))
;TEST:
(begin-for-test
  (check-equal? (rect-vx (new-rectangle 100 50 12 -10)) 12 
                "12 is the velocity of the center of the given rectangle in
                x-axis direction"))
;.....................................................................................
;; rect-vy : Myrectangle -> Int
;; GIVEN: a rectangle
;; RETURNS: the velocity of the center of the given rectangle in y-axis direction
;; STRATEGY: Use template for Myrectangle on r
;; EXMAPLE:
;; (rect-vx (new-rectangle 100 50 12 -10))=-10
;; rect-vy : Rectangle -> Int
(define (rect-vy r)
  (myrectangle-vy r))

;TEST:
(begin-for-test
  (check-equal? (rect-vy (new-rectangle 100 50 12 -10)) -10 
                "-10 is the velocity of the center of the given rectangle in
                y-axis direction"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene : WorldState -> Scene
;; GIVEN: a world state
;; RETURNS: tranlation of the given world state into a scene
;; STRATEGY: Combine simpler functions
(define (world-to-scene w)
  (place-rect1 (world-rect1 w)
              (place-rect2 (world-rect2 w)
                          EMPTY-CANVAS)))

;..................................................................................
;; place-rect1 : Myrectangle Scene -> Scene
;; GIVEN: 1st rectangle and the scene
;; RETURNS: a scene similar to given one but with the given rectangle placed in it
;; STRATEGY: Use template for Myrectangle on r
(define (place-rect1 r s)
  (if (myrectangle-selected? r)
  (place-image (circle 5 "outline" "red") (myrectangle-mx r) (myrectangle-my r)
               (place-image RECT1-CLICKED (myrectangle-x r) (myrectangle-y r) s))   
  (place-image RECT1 (myrectangle-x r) (myrectangle-y r) s)))
;.....................................................................................
;; place-rect2 : Myrectangle Scene -> Scene
;; GIVEN: 2nd rectangle and the scene
;; RETURNS: a scene similar to given one but with the given rectangle placed in it
;; STRATEGY: Use template for Myrectangle on r

(define (place-rect2 r s)
  (if (myrectangle-selected? r)
  (place-image (circle 5 "outline" "red") (myrectangle-mx r) (myrectangle-my r) (place-image RECT2-CLICKED (myrectangle-x r) (myrectangle-y r) s))
  (place-image RECT2 (myrectangle-x r) (myrectangle-y r) s)))
