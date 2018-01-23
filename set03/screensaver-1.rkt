;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a3q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Screensaver.  
;; Two rectangles move around at a contant velocity in the scene.
;; The user can pause/unpause the rectangles with the space bar.

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA DEFINITIONS;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct myrectangle (x y vx vy))
;;A Myrectangle is a (make-myrectangle Integer Integer Integer Integer)
;;INTERPRETATION:
;;x is the x-coordinate(in pixels) and y is the y-coordinate(in pixels)
;;     such that (x,y) represents the center of the rectangle.
;;vx is the velocity of the center of rectangle in x direction and
;;vy is the velocity of the center of rectangle in y direction


;; template:
;; myrectangle-fn : Myrectangle -> ??
;(define (myrectangle-fn r)
;  (... (myrectangle-x r)
;       (myrectangle-y r)
;       (myrectangle-vx r)
;       (myrectangle-vy r)))


(define-struct world (rect1 rect2 paused?))
;; A World is a (make-world Myrectangle Myrectangle Boolean)
;; INTERPRETATION: 
;; rect1 and rect2 are the two Rectangles
;; paused? describes whether or not the rectangle is paused.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-rect1 w)
;       (world-rect2 w)
;       (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;deimensions of rectangle
(define RECT-HEIGHT 50)
(define RECT-WIDTH 60)
(define RECT1 (overlay (text "(-12,20)" 12 "blue")
                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue")))
(define RECT2 (overlay (text "(23,-14)" 12 "blue")
                      (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue")))

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

;;Initial rectangles:
(define INITIAL-RECT1 (make-myrectangle RECT1-I-x RECT1-I-y RECT1-I-vx RECT1-I-vy))
(define INITIAL-RECT2 (make-myrectangle RECT2-I-x RECT2-I-y RECT2-I-vx RECT2-I-vy))

;; examples of myrectangle, for testing
(define rect1-at-original INITIAL-RECT1)
(define rect2-at-original INITIAL-RECT2)

(define rect1-at-next-state (make-myrectangle 188 120 -12 20))
(define rect2-at-next-state (make-myrectangle 223 186 23 -14))

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
    rect1-at-next-state
    rect2-at-next-state
    false))

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            (on-draw world-to-scene)))

;.............................................................................

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; STATEGY: Call simpler functions
;; EXAMPLE:
;; (initial-world 3)=(make-world (make-myrectangle 200 100 -12 20)
;                                (make-myrectangle 200 200 23 -14)
;                                #true)

(define (initial-world n)
  (make-world INITIAL-RECT1 INITIAL-RECT2 true)
  )

;TEST:
(begin-for-test
  (check-equal? (initial-world 3)
                (make-world (make-myrectangle 200 100 -12 20)
                             (make-myrectangle 200 200 23 -14)
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
; =(make-world (make-myrectangle 200 100 -12 20) (make-myrectangle 200 200 23 -14) #true)
;; (world-after-tick unpaused-world-at-original)
; =(make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #false)
;; (world-after-tick unpaused-world-at-first-tick)
; =(make-world (make-myrectangle 176 140 -12 20) (make-myrectangle 246 172 23 -14) #false)


(define (world-after-tick w)
(if (world-paused? w)
    w
    (make-world
     (rect-after-tick (world-rect1 w))
     (rect-after-tick (world-rect2 w))
     (world-paused? w))))

(begin-for-test
  (check-equal? (world-after-tick paused-world-at-original)
    (make-world (make-myrectangle 200 100 -12 20) (make-myrectangle 200 200 23 -14) #true)
    "Paused world stays paused in same state even after tick")
  (check-equal? (world-after-tick unpaused-world-at-original)
   (make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #false)
   "Unpaused world at original transits to next state after one tick")
  (check-equal? (world-after-tick unpaused-world-at-first-tick)
   (make-world (make-myrectangle 176 140 -12 20) (make-myrectangle 246 172 23 -14) #false)
   "Unpaused world transits to next state after one tick"))
;........................................................................

;; rect-after-tick : Myrectangle -> Myrectangle
;; Given: rectangle 1
;; RETURNS: the state of the given rectangle after a tick in an unpaused world
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-after-tick rect1-at-original)=(make-myrectangle 188 120 -12 20)
;(rect-after-tick (make-myrectangle 176 140 -12 20))=(make-myrectangle 164 160 -12 20)

(define (rect-after-tick r)
  (cond
    ((rect-collides-corner? r) (rect-proceeds (rect-bounces-from-corner r)))
    ((rect-collides-y? r) (rect-proceeds (rect-bounces-y r)))
    ((rect-collides-x? r) (rect-proceeds (rect-bounces-x r)))
    (else (rect-proceeds r))))

(begin-for-test
  (check-equal? (rect-after-tick rect1-at-original)
                (make-myrectangle 188 120 -12 20)
                "Rectangle changes its coordinates (200,100) to (188,120)
                 after a tick")
  (check-equal? (rect-after-tick rect1-at-original)
                (make-myrectangle 188 120 -12 20)
                "Rectangle changes its coordinates (176,140) to (164,160)
                 after a tick"))
                

;...........................................................................
;; rect-collides-corner? : Myrectangle -> Boolean
;; Given: a rectangle to be moved at next tick
;; RETURNS: true if the rectangle collides with the corner of canvas
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
;; (rect-collides-corner? (make-myrectangle 500 500 -12 20))=#true
;; (rect-collides-corner? (make-myrectangle COLLIDE-NORTH-Y COLLIDE-EAST-X -12 20))
;   =#true
;; (rect-collides-corner? (make-myrectangle 176 140 -12 20))=#false

(define (rect-collides-corner? r)
  (or
   (and (>= (myrectangle-x r) COLLIDE-EAST-X) (>= (myrectangle-y r) COLLIDE-SOUTH-Y))
   (and (<= (myrectangle-x r) COLLIDE-WEST-X) (>= (myrectangle-y r) COLLIDE-SOUTH-Y))
   (and (<= (myrectangle-x r) COLLIDE-WEST-X) (<= (myrectangle-y r) COLLIDE-NORTH-Y))
   (and (>= (myrectangle-x r) COLLIDE-EAST-X) (<= (myrectangle-y r) COLLIDE-NORTH-Y))
   ))

(begin-for-test
  (check-equal? (rect-collides-corner? (make-myrectangle 500 500 -12 20))
                #true
                "The rectangle collides/goes out of the canvas")
  (check-equal? (rect-collides-corner? (make-myrectangle COLLIDE-NORTH-Y
                                                         COLLIDE-EAST-X -12 20))
                #true
                "The rectangle collides with the canvas")
  (check-equal? (rect-collides-corner? (make-myrectangle 176 140 -12 20))
                #false
                "The rectangle does not collide and hence is in the canvas"))
 
;............................................................................
;; rect-collides-y? : Myrectangle -> Boolean
;; Given: a rectangle to be moved at next tick
;; RETURNS: true if the rectangle collides with the x-axis boundries i.e vertically
;;          collides the canvas
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
;; (rect-collides-y? (make-myrectangle 15 COLLIDE-NORTH-Y -12 20))=#true
;; (rect-collides-y? (make-myrectangle 15 (+ COLLIDE-NORTH-Y 1) -12 20))=#false
;; (rect-collides-y? (make-myrectangle 15 COLLIDE-SOUTH-Y -12 20))=#true
;; (rect-collides-y? (make-myrectangle 15 (- COLLIDE-SOUTH-Y 1) -12 20))=#false

(define (rect-collides-y? r)
  (or (>= (myrectangle-y r) COLLIDE-SOUTH-Y)
      (<= (myrectangle-y r) COLLIDE-NORTH-Y)))


(begin-for-test
  (check-equal? (rect-collides-y? (make-myrectangle 15 COLLIDE-NORTH-Y -12 20))
                #true
                "The rectangle collides with the north wall of canvas")
  (check-equal? (rect-collides-y? (make-myrectangle 15 (+ COLLIDE-NORTH-Y 1) -12 20))
                #false
                "The rectangle does not collide with the north wall of canvas")
  (check-equal? (rect-collides-y? (make-myrectangle 15 COLLIDE-SOUTH-Y -12 20))
                #true
                "The rectangle collides with the south wall of canvas")
  (check-equal? (rect-collides-y? (make-myrectangle 15 (- COLLIDE-SOUTH-Y 1) -12 20))
                #false
                "The rectangle does not collide with the south wall of canvas"))
;............................................................................

;; rect-collides-x? : Myrectangle -> Boolean
;; Given: a rectangle to be moved at next tick
;; RETURNS: true if the rectangle collides with the y-axis boundries i.e horizontally
;;          collides the canvas
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
;; (rect-collides-x? (make-myrectangle COLLIDE-WEST-X 15 -12 20))=#true
;; (rect-collides-x? (make-myrectangle (+ COLLIDE-WEST-X 1) 15 -12 20))=#false
;; (rect-collides-x? (make-myrectangle COLLIDE-EAST-X 20 -12 20))=#true
;; (rect-collides-x? (make-myrectangle (- COLLIDE-EAST-X 1) 20 -12 20))=#false
(define (rect-collides-x? r)
  (or (<= (myrectangle-x r) COLLIDE-WEST-X)
      (>= (myrectangle-x r) COLLIDE-EAST-X)))

(begin-for-test
  (check-equal? (rect-collides-x? (make-myrectangle COLLIDE-WEST-X 15 -12 20))
                #true
                "The rectangle collides with the west wall of canvas")
  (check-equal? (rect-collides-x? (make-myrectangle (+ COLLIDE-WEST-X 1) 15 -12 20))
                #false
                "The rectangle does not collide with the west wall of canvas")
  (check-equal? (rect-collides-x? (make-myrectangle COLLIDE-EAST-X 20 -12 20))
                #true
                "The rectangle collides with the east wall of canvas")
  (check-equal? (rect-collides-x? (make-myrectangle (- COLLIDE-EAST-X 1) 20 -12 20))
                #false
                "The rectangle does not collide with the east wall of canvas"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-bounces-from-corner : Myrectangle ->  Myrectangle 
;; Given: a rectangle that collides any corner of the canvas
;; RETURNS: a rectangle that bounces back after collision
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-bounces-from-corner (make-myrectangle COLLIDE-NORTH-Y COLLIDE-EAST-X -12 20))
; =(make-myrectangle 25 370 12 -20)

(define (rect-bounces-from-corner r)
  (make-myrectangle
    (myrectangle-x r)
    (myrectangle-y r)
    (- (myrectangle-vx r))
    (- (myrectangle-vy r))
    ))

;TEST:
(begin-for-test
  (check-equal? (rect-bounces-from-corner (make-myrectangle COLLIDE-NORTH-Y
                                                            COLLIDE-EAST-X -12 20))
                (make-myrectangle 25 370 12 -20)
                "The rectangle bounces back from northeast corner i.e the vx and vy
                velocities are inversed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-bounces-y : Myrectangle ->  Myrectangle 
;; Given: a rectangle that collides with the x-axis boundaries of the canvas
;; RETURNS: a rectangle that bounces back in vertical direction after collision
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-bounces-y (make-myrectangle COLLIDE-NORTH-Y 15 -12 20))
; =(make-myrectangle 25 15 -12 -20)
; (rect-bounces-y (make-myrectangle COLLIDE-SOUTH-Y 15 23 -14))
; =(make-myrectangle 275 15 23 14)
(define (rect-bounces-y r)
  (make-myrectangle
    (myrectangle-x r)
    (myrectangle-y r)
    (myrectangle-vx r)
    (- (myrectangle-vy r))
    )
  )

;TESTS:
(begin-for-test
  (check-equal? (rect-bounces-y (make-myrectangle COLLIDE-NORTH-Y 15 -12 20))
                (make-myrectangle 25 15 -12 -20)
                "The rectangle bounces back from north wall i.e the vy velocity
                is reversed")
  (check-equal? (rect-bounces-y (make-myrectangle COLLIDE-SOUTH-Y 15 23 -14))
                (make-myrectangle 275 15 23 14)
                "The rectangle bounces back from south wall i.e the vy velocity
                is reversed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-bounces-x : Myrectangle ->  Myrectangle 
;; Given: a rectangle that collides with the y-axis boundaries of the canvas
;; RETURNS: a rectangle that bounces back in horizontal direction after collision
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-bounces-x (make-myrectangle 15 COLLIDE-WEST-X -12 20))
; =(make-myrectangle 15 30 12 20)
; (rect-bounces-x (make-myrectangle 20 COLLIDE-EAST-X 23 -14))
; =(make-myrectangle 20 370 -23 -14)
(define (rect-bounces-x r)
  (make-myrectangle
    (myrectangle-x r)
    (myrectangle-y r)
    (- (myrectangle-vx r))
    (myrectangle-vy r)
    )
  )

;TESTS:
(begin-for-test
  (check-equal? (rect-bounces-x (make-myrectangle 15 COLLIDE-WEST-X -12 20))
                (make-myrectangle 15 30 12 20)
                "The rectangle bounces back from west wall i.e the vx velocity
                is reversed")
  (check-equal? (rect-bounces-x (make-myrectangle 20 COLLIDE-EAST-X 23 -14))
                (make-myrectangle 20 370 -23 -14)
                "The rectangle bounces back from east wall i.e the vx velocity
                is reversed"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-proceeds : Myrectangle ->  Myrectangle 
;; Given: a rectangle 
;; RETURNS: a rectangle that proceeds in the direction of its velocity
;; STRATEGY: Use template for Myrectangle on r
;; EXAMPLES:
; (rect-proceeds (make-myrectangle 15 10 -12 20))=(make-myrectangle 3 30 -12 20)
; (rect-proceeds (make-myrectangle 15 COLLIDE-WEST-X -12 20))
; =(make-myrectangle 3 50 -12 20)
(define (rect-proceeds r)
  (make-myrectangle
   (+ (myrectangle-x r) (myrectangle-vx r))
   (+ (myrectangle-y r) (myrectangle-vy r))
   (myrectangle-vx r)
   (myrectangle-vy r)
   ))

;TESTS:
(begin-for-test
  (check-equal? (rect-proceeds (make-myrectangle 15 10 -12 20))
                (make-myrectangle 3 30 -12 20)
                "Rectangle proceeds from (15,10) to (3,30)")
  (check-equal? (rect-proceeds (make-myrectangle 15 COLLIDE-WEST-X -12 20))
                (make-myrectangle 3 50 -12 20)
                "Rectangle proceeds from (15,30) to (3,50)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN:  a World State and a Keyevent
;; RETURNS: the World State that should follow the given worldstate
;; after the given keyevent
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;  (world-after-key-event unpaused-world-at-first-tick pause-key-event)
;=(make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #true)
;  (world-after-key-event unpaused-world-at-first-tick non-pause-key-event)
;=(make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #false)

(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

(begin-for-test
  (check-equal? (world-after-key-event unpaused-world-at-first-tick pause-key-event)
  (make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #true)
  "An unpaused world pauses after a valid keyevent is given")
  (check-equal? (world-after-key-event unpaused-world-at-first-tick non-pause-key-event)
  (make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #false)
  "The unpaused worls does not pause if an invalid keyevent is passed"))

;........................................................................................

;; world-with-paused-toggled : World -> World
;; GIVEN: a world
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template for World on w
;; EXAMPLE:
;(world-with-paused-toggled unpaused-world-at-first-tick)
; =(make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #true)
;(world-with-paused-toggled paused-world-at-original)
; =(make-world (make-myrectangle 200 100 -12 20) (make-myrectangle 200 200 23 -14) #false)

(define (world-with-paused-toggled w)
  (make-world
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w))))

;TESTS:
(begin-for-test
  (check-equal? (world-with-paused-toggled unpaused-world-at-first-tick)
  (make-world (make-myrectangle 188 120 -12 20) (make-myrectangle 223 186 23 -14) #true)
  "unpaused world returns true after being paused")
  (check-equal? (world-with-paused-toggled paused-world-at-original)
  (make-world (make-myrectangle 200 100 -12 20) (make-myrectangle 200 200 23 -14) #false)
  "paused world returns false after being unpaused"))


;; world-rect1 : WorldState -> Myrectangle
;; world-rect2 : WorldState -> Myrectangle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Myrectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; STRATEGY: Call simpler function
;; EXMAPLE:
;; (new-rectangle 100 50 12 -10)=(make-myrectangle 100 50 12 -10)

(define (new-rectangle x y vx vy)
  (make-myrectangle x y vx vy))

;TEST:
(begin-for-test
  (check-equal? (new-rectangle 100 50 12 -10)
                (make-myrectangle 100 50 12 -10)
                "makes a new Myrectangle with center at (100,50) and velocity (12,-10)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (place-image RECT1 (myrectangle-x r) (myrectangle-y r) s))

;..................................................................................
;; place-rect2 : Myrectangle Scene -> Scene
;; GIVEN: 2nd rectangle and the scene
;; RETURNS: a scene similar to given one but with the given rectangle placed in it
;; STRATEGY: Use template for Myrectangle on r
(define (place-rect2 r s)
  (place-image RECT2 (myrectangle-x r) (myrectangle-y r) s))
