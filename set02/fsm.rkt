;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 02 - QUESTION 02;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;fsm.rkt;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-location "02" "fsm.rkt")

(require rackunit)
(require "extras.rkt")

(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  )

;;CONSTANTS

(define INITIAL "initial")
(define INTERMEDIATE "intermediate")
(define FINAL "final")
(define ERROR "error")

(define A "a")
(define B "b")
(define C "c")
(define D "d")
(define E "e")
(define F "f")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An FAState is one of
;;    INITIAL
;;    INTERMEDIATE
;;    FINAL
;;    ERROR

;; INTERPRETATION:
;; INITIAL = the start state of the finite automata
;; INTERMEDIATE = the interim state of the finite automata when the pattern
;;                  is still incomplete i.e no reached accepted state or error state
;; FINAL = the final/stop state achievable when the given sequesnce of characters
;;              form a valid string
;; ERROR = state achievable when the given sequesnce of characters do not
;;              form a valid string


#|(define (fa-state-fn state)
  (cond
    [(string=? INITIAL state)...]
    [(string=? INTERMEDIATE state)...]
    [(string=? FINAL state)...]
    [(string=? ERROR state)...]))
|#
;.................................................................................

;;A MachineInput is one of
;;    A
;;    B
;;    C
;;    D
;;    E
;;    F
;;INTRPRETATION: self-evident

#|(define (machine-input-fn state)
  (cond
    [(key=? A mi)...]
    [(key=? B mi)...]
    [(key=? C mi)...]
    [(key=? D mi)...]
    [(key=? E mi)...]
    [(key=? F mi)...]))
|#
;..................................................................................

;;EXAMPLES:
;;(next-state(next-state (initial-state 5) A) B)="initial"
;;(next-state (initial-state 5) C)="intermediate"
;;(next-state(next-state(next-state(next-state (initial-state 5) A) C) D) F)="final"
;;(next-state(next-state(next-state(next-state (initial-state 5) A) C) D) B)="error"
;;(accepting-state?(next-state(next-state(next-state(next-state(next-state
;;                 (next-state(next-state (initial-state 5) A) A) C) B) D) F) F))=#true
;;(accepting-state? (next-state (next-state (initial-state 5) C) D))=#true
;;(accepting-state? (next-state (next-state (initial-state 5) A) C))=#false
;;(error-state? (next-state(next-state (next-state (initial-state 5) C) D) F))=#false
;;(error-state? (next-state(next-state (next-state (initial-state 5) C) D) A))=#true


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|initial-state : Number -> State
GIVEN: a number
RETURNS: a representation of the initial state
of your machine.  The given number is ignored.
STATEGY: Combine simple functions
EXAMPLE:
(initial-state 5)="initial"
|#

(define (initial-state number)
  INITIAL
)
#|

;...............................................................

next-state : State MachineInput -> State
GIVEN: a state of the machine and a valid machine input
RETURNS: the state that should follow the given input.
STATEGY: 
STRATEGY: Use template for FAState on state
EXAMPLE:
(next-state (initial-state 5) A)="initial"
(next-state (next-state (initial-state 5) A) C)="intermediate"
(next-state (next-state (initial-state 5) C) D)="final"
|#

(define (next-state state mi)
  (cond
    [(string=? INITIAL state) (accept-at-initial mi)]
    [(string=? INTERMEDIATE state) (accept-at-intermediate mi)]
    [(string=? FINAL state) (accept-at-final mi)]
    [else ERROR]
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accept-at-initial : MachineInput -> FAState
;; GIVEN: a machine input at initial state
;; RETURNS: state based on machine input
;; STRATEGY: Combine simpler functions
;; EXAMPLES: Use template for MachineInput on mi
;; (accept-at-initial A)="initial"
;; (accept-at-initial C)="intermediate"
;; (accept-at-initial D)="error"

(define (accept-at-initial mi)
  (cond
    [(key=? A mi) INITIAL]
    [(key=? B mi) INITIAL]
    [(key=? C mi) INTERMEDIATE]
    [else ERROR] )
)

;; accept-at-intermediate : 
;; GIVEN: a machine input at intermediate state
;; RETURNS: state based on machine input
;; STRATEGY: Use template for MachineInput on mi
;; EXAMPLES:
;; (accept-at-intermediate A)="intermediate"
;; (accept-at-intermediate D)="final"
;; (accept-at-intermediate E)="error"

(define (accept-at-intermediate mi)
 (cond
    [(key=? A mi) INTERMEDIATE]
    [(key=? B mi) INTERMEDIATE]
    [(key=? D mi) FINAL]
    [else ERROR])
 )

;; accept-at-final : 
;; GIVEN: a machine input at final state
;; RETURNS: state based on machine input
;; STRATEGY: Use template for MachineInput on mi 
;; EXAMPLES:
;; (accept-at-final E)="final"
;; (accept-at-final F)="final"
;; (accept-at-final A)="error"

(define (accept-at-final mi)
  (cond
    [(key=? E mi) FINAL]
    [(key=? F mi) FINAL]
    [else ERROR] )
)

#|
accepting-state? : State -> Boolean
GIVEN: a state of the machine
RETURNS: true iff the given state is a final (accepting) state
STRATEGY: Use template for FAState on state
EXAMPLES:
(accepting-state? (next-state (next-state (initial-state 5) C) D))=#true
(accepting-state? (next-state (next-state (initial-state 5) A) C))=#false
|#

(define (accepting-state? state)
  (if (string=? FINAL state)
     #t
     #f))

#|
error-state? : State -> Boolean
GIVEN: a state of the machine
RETURNS: true iff there is no path (empty or non-empty) from the given
state to an accepting state
STRATEGY: Use template for FAState on state
EXAMPLES:
(error-state? (next-state (next-state (initial-state 5) C) D))=#false
(error-state? (next-state (next-state (initial-state 5) A) C))=#true
|#

(define (error-state? state)
  (if (string=? ERROR state)
     #t
     #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal? (next-state(next-state (initial-state 5) A) B)
                "initial"
                "String ab leads to initial state")
  (check-equal? (next-state (initial-state 5) C)
                "intermediate"
                "String c leads to intermediate state")
  (check-equal? (next-state(next-state(next-state(next-state (initial-state 5) A) C) D) F)
                "final"
                "String acdf leads to final state")
  (check-equal? (next-state(next-state(next-state(next-state (initial-state 5) A) C) D) B)
                "error"
                "String acdb leads to error state")
  (check-equal? (accepting-state?(next-state(next-state(next-state(next-state(next-state
                (next-state(next-state (initial-state 5) A) A) C) B) D) F) F)) #true
                "String aacbdff is accepted")                                                              
  (check-equal? (accepting-state? (next-state (next-state (initial-state 5) C) D))
                #true
                "String cd is accepted")
  (check-equal? (accepting-state? (next-state (next-state (initial-state 5) A) C))
                #false
                "String ac is not accepted")
  (check-equal? (error-state? (next-state(next-state (next-state (initial-state 5) C) D)
                 F))
                #false
                "String cdf is not erronous i.e string is valid")
  (check-equal? (error-state? (next-state(next-state (next-state (initial-state 5) C) D)
                 A))
                #true
                "String cda is erronous i.e not valid"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;