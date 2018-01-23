#lang racket

(require "extras.rkt")
(require "Interfaces.rkt")
(require rackunit)
(require "PerfectBounce.rkt")

(provide Model%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DATA DEFINITIONS :

; a ListOfController<%> (LOC<%>) can be either :
; -- empty
; (cons Controller<%> LOC<%>)
; INTERPRETATION :
; empty : represents an empty list
; (cons Controller<%> LOC<%>) : represents an list whose first
;      element is a Controller<%> and the rest is a LOC<%>
; TEMPLATE :
#;(define (loc-fn LOC)
    (..(first LOC))
    (...(loc (rest LOC))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the model consists of a particle, bouncing with its center from x=0
;; to x=200.  It accepts commands and reports when its status changes

; A Model is a (new Model% [lo Integer] [hi Integer] [ylo Integer] [yhi Integer]
;                          [vx Integer] [vy Integer] [selected? Boolean]
;                          [controllers LOC])

; a Model is the model layer in the MVC architecture is responsible for
; all operations on the data and sendind it back to the controller.
; a Model receives signals from controllers and reports when its status
; changes
; It implemets the Model<%>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Model%
  (class* object% (Model<%>)
    
    ;; boundaries of the field
    (init-field [lo 0])
    (init-field [hi 150])
    (init-field [ylo 0])
    (init-field [yhi 100])
    
    ;; position and velocity of the object
    (field [x (/ (+ lo hi) 2)])
    (field [y (/ (+ ylo yhi) 2)])
    (init-field [vx 0])
    (init-field [vy 0])
    (init-field [is-selected? false])
    
    ; ListOfController<%>
    (init-field [controllers empty])
    
    ;................................................................................
    
    ;; INTERPRETATION:
    ;--------------------------------------------------------------------------------
    ;; lo            : The lower boundary value of a controller along x direction
    ;; hi            : The upper boundary value of a controller along x direction
    ;; ylo           : The lower boundary value of a controller along y direction
    ;; yhi           : The upper boundary value of a controller along y direction
    ;; vx            : The velocity of the particle along x direction
    ;; vy            : The velocity of the particle along y direction
    ;; is-selected?  : Checks whether the controller is selected or not
    ;; controllers   : List of controllers in the model
    ;--------------------------------------------------------------------------------
    
    (super-new)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; (after-tick) : -> Void
    ;; GIVEN  : no argument
    ;; EFFECT :sets the x, y, vx, vy fields which depends 
    ;;              on selected-val? field of object.
    ;;              if the resulting x is >= 150 or <= 0
    ;;              reports x at ever tick
    ;;              reports velocity when selected-val? is true.
    
    (define/public (after-tick)
      (if is-selected?
          (begin
            (publish-position)
            (publish-velocity))
          (let
              ((particle (particle-after-tick
                          (make-particle x y vx vy)
                          (make-rect 0 150 0 100))))
            (set! x (particle-x particle))
            (set! y (particle-y particle))
            (set! vx (particle-vx particle))
            (set! vy (particle-vy particle))
            (publish-position)
            (publish-velocity)
            )))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; within-limits : Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi] 
    
    (define (within-limits lo val hi)
      (max lo (min val hi)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; register  : Controller<%> -> Void
    ;; GIVEN     : A Controller<%>, c
    ;; EFFECT    : register the new controller and send it some data
    
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x y))
        (send c receive-signal (make-report-velocity vx vy))
        (send c receive-signal (make-report-selected is-selected?))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; execute-command : Command -> Void
    ;; GIVEN           : A command 'cmd'
    ;; EFFECT          : decodes the command, executes it, updates the data in this
    ;;                   object and sends updates to the controllers through signals.
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (if (> x 150)
               (set! x 150)
               (if (< x 0)
                   (set! x 0)
                   (set! x (set-position-pos-x cmd))))
           (if (> y 100)
               (set! y 100)
               (if (< y 0)
                   (set! y 0)
                   (set! y (set-position-pos-y cmd))))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! vx (+ vx (incr-velocity-dv-x cmd)))
           (set! vy (+ vy (incr-velocity-dv-y cmd)))
           (publish-velocity))]
        [(set-selected? cmd)
         (begin
           (set! is-selected? (set-selected-sel cmd)))]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; publish-position   :  -> Void
    ;; GIVEN              : No Argument
    ;; EFFECT             : report position  to each controller
    (define (publish-position)
      (let ((msg (make-report-position x y)))
        (for-each
         (lambda (obs) (send obs receive-signal msg))
         controllers)))    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; publish-velocity   :  -> Void
    ;; GIVEN              : No Argument
    ;; EFFECT             : report velocity to each controller
    (define (publish-velocity)
      (let ((msg (make-report-velocity vx vy)))
        (for-each
         (lambda (obs) (send obs receive-signal msg))
         controllers))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





