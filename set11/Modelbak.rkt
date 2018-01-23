#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=200.  It accepts commands and reports when its status changes

(require "extras.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")

(provide Model%)

(define Model%
  (class* object% (Model<%>)
    
    ;; boundaries of the field
    (field [x-lo 0])
    (field [x-hi 150])
    (field [y-lo 0])
    (field [y-hi 100])

    (init-field [R (make-rect x-lo x-hi y-lo y-hi)])

    ;; position and velocity of the object
    (init-field [x (/ (+ x-lo x-hi) 2)])
    (init-field [y (/ (+ y-lo y-hi) 2)])
    (init-field [vx 0])
    (init-field [vy 0])

    (init-field [P (make-particle x y vx vy)])

    ; ListOfController<%>
    (init-field [controllers empty])   

    (super-new)

    ;; -> Void
    ;; moves the object by v.
    ;; if the resulting x is >= 200 or <= 0
    ;; reports x at ever tick
    ;; reports velocity only when it changes

;    (define/public (after-tick)
;      (begin
;      (set! x (within-limits x-lo (+ x vx) x-hi))
;      (set! y (within-limits y-lo (+ y vy) y-hi))
;      (publish-position)
;      (if (or (= x hi) (= x lo))
;        (begin
;          (set! v (- v))
;          (publish-velocity))
;        "model.rkt after-tick"))
;
;      (set! P (particle-after-tick P R)))

    (define/public (after-tick)
      (set! x (within-limits x-lo (+ x vx) x-hi))
      (set! y (within-limits y-lo (+ y vy) y-hi))
      (set! P (make-particle x y vx vy))
      (publish-position)
      (cond
        [(or (= x x-hi) (= x x-lo))
         (begin
           (set! P (make-particle x
                                  y
                                  (- vx)
                                  vy))
          (publish-velocity))]
        [(or (= y y-lo) (= y y-hi))
         (begin
           (set! P (make-particle x
                                  y
                                  vx
                                  (- vy)))
           (publish-velocity))])
      (particle-after-tick
       (make-particle x y vx vy)
      (make-rect 0 150 0 100)))

    (define (within-limits lo val hi)
      (max lo (min val hi)))

   

    ;; Controller -> Void
    ;; register the new controller and send it some data
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position (particle-x P) (particle-y P)))
        (send c receive-signal (make-report-velocity (particle-vx P) (particle-vy P)))))

    ;; Command -> Void
    ;; decodes the command, executes it, and sends updates to the
    ;; controllers. 
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! P (make-particle (set-position-pos-x cmd) (set-position-pos-y cmd)
                                  (particle-vx P) (particle-vy P)))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! P (make-particle (particle-x P) (particle-y P)
                                  (+ (particle-vx P) (incr-velocity-dv-x cmd))
                                  (+ (particle-vy P) (incr-velocity-dv-y cmd))))
           (publish-velocity))]))

    ;; report position or velocity to each controller:

    (define (publish-position)
      (let ((msg (make-report-position (particle-x P) (particle-y P))))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    (define (publish-velocity)
      (let ((msg (make-report-velocity (particle-vx P) (particle-vy P))))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ))




    

    