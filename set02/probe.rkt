;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 02 - QUESTION 04;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;probe.rkt;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-location "02" "probe.rkt")

(require rackunit)
(require "extras.rkt")

(provide
 probe-at
 probe-turned-left
 probe-turned-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?
  )

;CONSTANTS:

(define NORTH "north")
(define SOUTH "south")
(define EAST "east")
(define WEST "west")

(define PROBE-RADIUS 20)
(define TRAP-SIDE 347)
(define TRAP-QUADRANT-LENGTH (floor (/ TRAP-SIDE 2)))
;;trap quadrant length happens to be 173.5 but since probe moves at
;;steps that are integer (1 cm), we take quadrant length of trap to be 173
;;in order to prevent probe from crashing into the walls of the trap

(define EXTREME-DIST (- TRAP-QUADRANT-LENGTH 20))
(define EXTREME-NEG  (- 0 EXTREME-DIST))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA-DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct probe (x-coordinate y-coordinate direction))

;;A Probe is a (make-probe NonNegInt NonNegInt String)

;;INTERPRETATION:
;;x-coordinate and y-coordinate together represent a point (x,y) which describes
;;                              the location of center of the probe in trap
;;direction = the direction in which probe is facing in the trap

;;probe : Probe -> ??
;;(define (probe-fn p)
;; (...
;;    (probe-x-coordinate p)
;;    (probe-y-coordinate p)
;;    (probe-direction p)))
;...............................................................................

;;Direction is one of
;;      NORTH
;;      SOUTH
;;      EAST
;;      WEST

;;TEMPLATE:
#|(define (direction-fn d)
  (cond
    [(string=? NORTH d)...]
    [(string=? SOUTH d)...]
    [(string=? EAST d)...]
    [(string=? WEST d)...]))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
probe-at : Integer Integer -> Probe
GIVEN: an x-coordinate and a y-coordinate
WHERE: these coordinates leave the robot entirely inside the trap
RETURNS: a probe with its center at those coordinates, facing north.
STRATEGY: Combine simpler function

EXAMPLE: a set of coordinates that put the probe in contact with the
wall is not consistent with the contract.  Note that this means that
the behavior of probe-at in this situation is unspecified; you don't
need to check for this.

;; (probe-at 0 0)=(make-probe 0 0 "north")
;; (probe-at -5 6)=(make-probe -5 6 "north")

|#

(define (probe-at x-coordinate y-coordinate)
  (make-probe
   x-coordinate
   y-coordinate
   NORTH
   )
  )

;;TESTS:
(begin-for-test
  (check-equal? (probe-at 0 0) (make-probe 0 0 "north") "probe at (0,0)")
  (check-equal? (probe-at -5 6) (make-probe -5 6 "north") "probe at (-5,6)")
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
probe-turned-left : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but turned 90 degrees left
STRATEGY:  Use template for Probe on p
EXAMPLES:
;;(probe-turned-left (make-probe 0 0 NORTH))=(make-probe 0 0 "west")
;;(probe-turned-left (make-probe 1 1 SOUTH))=(make-probe 1 1 "east")
;;(probe-turned-left (make-probe 1 1 EAST))=(make-probe 1 1 "north")
;;(probe-turned-left (make-probe -1 -1 WEST))=(make-probe -1 -1 "south")
|#

(define (probe-turned-left p)
  (cond
    [(string=? NORTH (probe-direction p)) (turn-to-west p)]
    [(string=? SOUTH (probe-direction p)) (turn-to-east p)]
    [(string=? EAST (probe-direction p)) (turn-to-north p)]
    [(string=? WEST (probe-direction p)) (turn-to-south p)]
    [else p]
    )
  )

;;TESTS:
(begin-for-test
  (check-equal? (probe-turned-left (make-probe 0 0 NORTH))
                (make-probe 0 0 "west")
                "probe turned to west")
  (check-equal? (probe-turned-left (make-probe 1 1 SOUTH))
                (make-probe 1 1 "east")
                "probe turned to east")
  (check-equal? (probe-turned-left (make-probe 1 1 EAST))
                (make-probe 1 1 "north")
                "probe turned to north")
  (check-equal? (probe-turned-left (make-probe -1 -1 WEST))
                (make-probe -1 -1 "south")
                "probe turned to south")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
probe-turned-right : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but turned 90 degrees right.
STRATEGY: Use template for Probe on p
EXAMPLES:
;;(probe-turned-right (make-probe 0 0 NORTH))=(make-probe 0 0 "east")
;;(probe-turned-right (make-probe 1 1 SOUTH))=(make-probe 1 1 "west")
;;(probe-turned-right (make-probe -1 -1 EAST))=(make-probe -1 -1 "south")
;;(probe-turned-right (make-probe 1 1 WEST))=(make-probe 1 1 "north")
|#

(define (probe-turned-right p)
  (cond
    [(string=? NORTH (probe-direction p)) (turn-to-east p)]
    [(string=? SOUTH (probe-direction p)) (turn-to-west p)]
    [(string=? EAST (probe-direction p)) (turn-to-south p)]
    [(string=? WEST (probe-direction p)) (turn-to-north p)]
    [else p]
    )
  )

;;TESTS:
(begin-for-test
  (check-equal? (probe-turned-right (make-probe 0 0 NORTH))
                (make-probe 0 0 "east")
                "probe turned to east")
  (check-equal? (probe-turned-right (make-probe 1 1 SOUTH))
                (make-probe 1 1 "west")
                "probe turned to west")
  (check-equal? (probe-turned-right (make-probe -1 -1 EAST))
                (make-probe -1 -1 "south")
                "probe turned to southth")
  (check-equal? (probe-turned-left (make-probe 1 1 WEST))
                (make-probe 1 1 "south")
                "probe turned to south")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
turn-to-west : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but facing to west.
STRATEGY: Use template for Probe on p
EXAMPLES:
;;(turn-to-west (make-probe 0 0 NORTH))=(make-probe 0 0 "west")
;;(turn-to-west (make-probe -1 -1 EAST))=(make-probe -1 -1 "west")
|#

(define (turn-to-west p)
  (make-probe
   (probe-x-coordinate p)
   (probe-y-coordinate p)
   WEST))

;;TESTS:
(begin-for-test
  (check-equal? (turn-to-west (make-probe 0 0 NORTH))
                (make-probe 0 0 "west")
                "probe turns to west from north")
  (check-equal? (turn-to-west (make-probe -1 -1 EAST))
                (make-probe -1 -1 "west")
                "probe turns to west from east")
)

;.......................................................................

#|
turn-to-east : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but facing to east.
STRATEGY: Use template for Probe on p
EXAMPLES:
;;(turn-to-east (make-probe 0 0 NORTH))=(make-probe 0 0 "east")
;;(turn-to-east (make-probe -1 -1 SOUTH))=(make-probe -1 -1 "east")
|#

(define (turn-to-east p)
  (make-probe
   (probe-x-coordinate p)
   (probe-y-coordinate p)
   EAST))

;;TESTS:
(begin-for-test
  (check-equal? (turn-to-east (make-probe 0 0 NORTH))
                (make-probe 0 0 "east")
                "probe turns to east from north")
  (check-equal? (turn-to-east (make-probe -1 -1 SOUTH))
                (make-probe -1 -1 "east")
                "probe turns to east from south")
)
;..................................................................

#|
turn-to-north : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but facing to north.
STRATEGY: Use template for Probe on p
EXAMPLES:
;;(turn-to-east (make-probe 2 -3 EAST))=(make-probe 2 -3 "north")
;;(turn-to-east (make-probe -1 -1 SOUTH))=(make-probe -1 -1 "north")
|#

(define (turn-to-north p)
  (make-probe
   (probe-x-coordinate p)
   (probe-y-coordinate p)
   NORTH))

;TESTS:
(begin-for-test
  (check-equal? (turn-to-north (make-probe 2 -3 EAST))
                (make-probe 2 -3 "north")
                "probe turns to north from east")
  (check-equal? (turn-to-north (make-probe -1 -1 SOUTH))
                (make-probe -1 -1 "north")
                "probe turns to north from south")
)

;..............................................................................

#|
turn-to-south : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but facing to south.
STRATEGY: Use template for Probe on p
EXAMPLES:
;;(turn-to-south (make-probe 2 -3 EAST))=(make-probe 2 -3 "south")
;;(turn-to-south (make-probe -1 -1 WEST))=(make-probe -1 -1 "south")
|#

(define (turn-to-south p)
  (make-probe
   (probe-x-coordinate p)
   (probe-y-coordinate p)
   SOUTH))

;TESTS:
(begin-for-test
  (check-equal? (turn-to-south (make-probe 2 -3 EAST))
                (make-probe 2 -3 "south")
                "probe turns to south from east")
  (check-equal? (turn-to-south (make-probe -1 -1 WEST))
                (make-probe -1 -1 "south")
                "probe turns to south from west")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
probe-forward : Probe PosInt -> Probe
GIVEN: a probe and a distance
RETURNS: a probe like the given one, but moved forward by the
specified distance.  If moving forward the specified distance would
cause the probe to hit any wall of the trap, then the probe should 
move as far as it can inside the trap, and then stop.
STRATEGY: Combine simpler functions
EXAMPLES:
;;(probe-forward (make-probe -2 2 NORTH) 10)=(make-probe -2 -8 "north")
;;(probe-forward (make-probe 0 0 EAST) 100)=(make-probe 100 0 "east")
;;(probe-forward (make-probe 5 -4 WEST) 100)=(make-probe -95 -4 "west")
|#

(define (probe-forward p dist)
  (cond
     [(probe-north? p) (probe-move-north p dist)]
     [(probe-south? p) (probe-move-south p dist)]
     [(probe-east? p) (probe-move-east p dist)]
     [(probe-west? p) (probe-move-west p dist)]
   )
 )

;;TESTS:
(begin-for-test
  (check-equal? (probe-forward (make-probe -2 2 NORTH) 10)
                (make-probe -2 -8 "north")
                "probe moves from (-2,2) to (-2,-8) i.e. 10 steps towards north")
  (check-equal? (probe-forward (make-probe 0 0 EAST) 100)
                (make-probe 100 0 "east")
                "probe moves from (0,0) to (100,0) i.e. 100 steps towards east")
  (check-equal? (probe-forward (make-probe 5 -4 WEST) 100)
                (make-probe -95 -4 "west")
                "probe moves from (5,-4) to (-95,-4) i.e. 100 steps towards west")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probe-move-north : Probe NonNegInt-> Probe
;; GIVEN: a probe and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in north direction if within trap
;;          or stopped before crashing into northern wall
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (probe-move-north (make-probe 1 1 NORTH) 100)=(make-probe 1 -99 "north")
;; (probe-move-north (make-probe 1 1 NORTH) 200)=(make-probe 1 -153 "north")

(define (probe-move-north p dist)
  (if (<= (+ dist (abs(probe-y-coordinate p))) EXTREME-DIST)
      (proceed-north p dist)
      (stop-at-north-wall p))     
)

;TESTS:
(begin-for-test
  (check-equal? (probe-move-north (make-probe 1 1 NORTH) 100)
                (make-probe 1 -99 "north")
                "probe moves 100 steps to north")
  (check-equal? (probe-move-north (make-probe 1 1 NORTH) 200)
                (make-probe 1 -153 "north")
                "probe stoped before chrashing into wall")
)
;.............................................................................

;; proceed-north : Probe NonNegInt-> Probe
;; GIVEN: a probe and and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in north direction
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (proceed-north (make-probe 1 1 NORTH) 100)=(make-probe 1 -99 "north")

(define (proceed-north p dist)
  (make-probe
       (probe-x-coordinate p)
       (- (probe-y-coordinate p) dist)
       (probe-direction p))
)

;TESTS:
(begin-for-test
  (check-equal? (proceed-north (make-probe 1 1 NORTH) 100)
                (make-probe 1 -99 "north")
                "probe moved 100 steps in north direction")
)

;................................................................................
;; stop-at-north-wall : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe stopped before crashing into the northern wall
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (stop-at-north-wall (make-probe 1 1 NORTH))=(make-probe 1 -153 "north")

(define (stop-at-north-wall p)
  (make-probe
       (probe-x-coordinate p)
       EXTREME-NEG
       (probe-direction p))
)

;TESTS
(begin-for-test
  (check-equal? (stop-at-north-wall (make-probe 1 1 NORTH))
                (make-probe 1 -153 "north")
                "probe stopped before crashing into the northen wall")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-move-south : Probe NonNegInt-> Probe
;; GIVEN: a probe and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in south direction if within trap
;;          or stopped before crashing into southern wall
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (probe-move-south (make-probe 1 1 SOUTH) 100)=(make-probe 1 101 "south")
;; (probe-move-south (make-probe 1 1 SOUTH) 200)=(make-probe 1 153 "south")

(define (probe-move-south p dist)
  (if (<= (+ dist (abs(probe-y-coordinate p))) EXTREME-DIST)
      (proceed-south p dist)
      (stop-at-south-wall p)
))

;TESTS:
(begin-for-test
  (check-equal? (probe-move-south (make-probe 1 1 SOUTH) 100)
                (make-probe 1 101 "south")
                "probe moved 100 steps towards south")
   (check-equal? (probe-move-south (make-probe 1 1 SOUTH) 200)
                (make-probe 1 153 "south")
                "probe stopped before crashing into the southern wall")
)
;.........................................................................

;; proceed-south : Probe NonNegInt-> Probe
;; GIVEN: a probe and and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in south direction
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (proceed-south (make-probe 1 1 SOUTH) 100)=(make-probe 1 101 "south")

(define (proceed-south p dist)
  (make-probe
       (probe-x-coordinate p)
       (+ (probe-y-coordinate p) dist)
       (probe-direction p)))

;TESTS:
(begin-for-test
  (check-equal? (proceed-south (make-probe 1 1 SOUTH) 100)
                (make-probe 1 101 "south")
                "probe moved 100 steps towards south")
)
;.............................................................................
;; stop-at-south-wall : Probe NonNegInt-> Probe
;; GIVEN: a probe 
;; RETURNS: a probe stopped before crashing into the southern wall
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (stop-at-south-wall (make-probe 1 1 SOUTH))=(make-probe 1 153 "south")
                

(define (stop-at-south-wall p)
  (make-probe
       (probe-x-coordinate p)
       EXTREME-DIST
       (probe-direction p)))

;TESTS:
(begin-for-test
   (check-equal? (stop-at-south-wall (make-probe 1 1 SOUTH))
                (make-probe 1 153 "south")
                "probe stopped before crashing into the southern wall")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-move-west : Probe NonNegInt-> Probe
;; GIVEN: a probe and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in west direction if within trap
;;          or stopped before crashing into western wall
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (probe-move-west (make-probe 1 1 WEST) 100)=(make-probe -99 1 "west")
;; (probe-move-west (make-probe 1 1 WEST) 200)=(make-probe -153 1 "west")

(define (probe-move-west p dist)
  (if (<= (abs (- dist (probe-x-coordinate p))) EXTREME-DIST)
      (proceed-west p dist)
      (stop-at-west-wall p)
))

;TESTS:
(begin-for-test
  (check-equal? (probe-move-west (make-probe 1 1 WEST) 100)
                (make-probe -99 1 "west")
                "probe moved 100 steps towards west")
   (check-equal? (probe-move-west (make-probe 1 1 WEST) 200)
                (make-probe -153 1 "west")
                "probe stopped before crashing into the western wall")
)
;.........................................................................
;; proceed-west : Probe NonNegInt-> Probe
;; GIVEN: a probe and and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in west direction
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (proceed-west (make-probe 1 1 WEST) 100)=(make-probe -99 1 "west")

(define (proceed-west p dist)
  (make-probe
       (- (probe-x-coordinate p) dist)
       (probe-y-coordinate p)
       (probe-direction p)
      ))

;TESTS:
(begin-for-test
  (check-equal? (proceed-west (make-probe 1 1 WEST) 100)
                (make-probe -99 1 "west")
                "probe moved 100 steps towards west"))

;.......................................................................
;; stop-at-west-wall : Probe NonNegInt-> Probe
;; GIVEN: a probe 
;; RETURNS: a probe stopped before crashing into the western wall
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (stop-at-west-wall (make-probe 1 1 WEST))=(make-probe -153 1 "west")
  
(define (stop-at-west-wall p)
  (make-probe
       EXTREME-NEG
       (probe-y-coordinate p)
       (probe-direction p)))

;TESTS:
(begin-for-test
  (check-equal? (stop-at-west-wall (make-probe 1 1 WEST))
                (make-probe -153 1 "west")
                "probe stopped before crashing into the western wall"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-move-east : Probe NonNegInt-> Probe
;; GIVEN: a probe and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in east direction if within trap
;;          or stopped before crashing into eastern wall
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (probe-move-east (make-probe 1 1 EAST) 100)=(make-probe 101 1 "east")
;; (probe-move-east (make-probe 1 1 EAST) 200)=(make-probe 153 1 "east")

(define (probe-move-east p dist)
  (if (<= (+ dist (abs(probe-x-coordinate p))) EXTREME-DIST)
      (proceed-east p dist)
      (stop-at-east-wall p)
))

;TESTS:
(begin-for-test
  (check-equal? (probe-move-east (make-probe 1 1 EAST) 100)
                (make-probe 101 1 "east")
                "probe moved 100 steps towards east")
   (check-equal? (probe-move-east (make-probe 1 1 EAST) 200)
                (make-probe 153 1 "east")
                "probe stopped before crashing into the eastern wall")
)
;............................................................................
;; proceed-east : Probe NonNegInt-> Probe
;; GIVEN: a probe and and number of steps for probe to move
;; RETURNS: a probe moved given number of steps in east direction
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (proceed-east (make-probe 1 1 EAST) 100)=(make-probe 101 1 "east")

(define (proceed-east p dist)
  (make-probe
       (+ (probe-x-coordinate p) dist)
       (probe-y-coordinate p)
       (probe-direction p)
      ))

;TESTS:
(begin-for-test
  (check-equal? (proceed-east (make-probe 1 1 EAST) 100)
                (make-probe 101 1 "east")
                "probe moved 100 steps towards east"))
;.........................................................................
;; stop-at-east-wall : Probe -> Probe
;; GIVEN: a probe 
;; RETURNS: a probe stopped before crashing into the western wall
;; STRATEGY: Use template for Probe on p
;; EXAMPLES:
;; (stop-at-east-wall (make-probe 1 1 EAST))=(make-probe 153 1 "east")

(define (stop-at-east-wall p)
  (make-probe
        EXTREME-DIST
       (probe-y-coordinate p)
       (probe-direction p)))

;TESTS:
(begin-for-test
  (check-equal? (stop-at-east-wall (make-probe 1 1 EAST))
                (make-probe 153 1 "east")
                "probe stopped before crashing into the eastern wall"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;probe-north? : Probe -> Boolean
;;GIVEN: a probe
;;ANSWERS: whether the probe is facing in the north direction.
;;STRATEGY: Use template for Probe on p
;;EXAMPLES
;;(probe-north? (make-probe 1 1 EAST))=#false
;;(probe-north? (probe-at 0 2)=#true

(define (probe-north? p)
  (cond
    [(string=? NORTH (probe-direction p)) #t]
    [else #f]
    ))

;TESTS:
(begin-for-test
  (check-equal? (probe-north? (make-probe 1 1 EAST))
                #false
                "probe is not facing north")
  (check-equal? (probe-north? (probe-at 0 2))
                #true
                "probe is facing north")
)

;.................................................................
;;probe-south? : Probe -> Boolean
;;GIVEN: a probe
;;ANSWERS: whether the probe is facing in the south direction.
;;STRATEGY: Use template for Probe on p
;;EXAMPLES
;;(probe-south? (make-probe 1 1 EAST))=#false
;;(probe-south? (make-probe 0 2 SOUTH))=#true

(define (probe-south? p)
  (cond
    [(string=? SOUTH (probe-direction p)) #t]
    [else #f]
    ))

;TESTS:
(begin-for-test
  (check-equal? (probe-south? (make-probe 1 1 EAST))
                #false
                "probe is not facing south")
  (check-equal? (probe-south? (make-probe 0 2 SOUTH))
                #true
                "probe is facing south")
)
;....................................................................

;;probe-east? : Probe -> Boolean
;;GIVEN: a probe
;;ANSWERS: whether the probe is facing in the east direction.
;;STRATEGY: Use template for Probe on p
;;EXAMPLES
;;(probe-east? (make-probe 1 1 EAST))=#true
;;(probe-east? (make-probe 0 2 NORTH))=#false


(define (probe-east? p)
  (cond
    [(string=? EAST (probe-direction p)) #t]
    [else #f]
    ))

;TESTS:
(begin-for-test
  (check-equal? (probe-east? (make-probe 1 1 EAST))
                #true
                "probe is facing east")
  (check-equal? (probe-east? (make-probe 0 2 NORTH))
                #false
                "probe is not facing east")
)
;.....................................................................

;;probe-west? : Probe -> Boolean
;;GIVEN: a probe
;;ANSWERS: whether the probe is facing in the west direction.
;;STRATEGY: Use template for Probe on p
;;EXAMPLES
;;(probe-west? (make-probe 1 1 WEST))=#true
;;(probe-west? (probe-at 0 2))=#false

(define (probe-west? p)
  (cond
    [(string=? WEST (probe-direction p)) #t]
    [else #f]
    ))

;TESTS:
(begin-for-test
  (check-equal? (probe-west? (make-probe 1 1 WEST))
                #true
                "probe is facing west")
  (check-equal? (probe-west? (probe-at 0 2))
                #false
                "probe is not facing west as initially it is always facing north")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;