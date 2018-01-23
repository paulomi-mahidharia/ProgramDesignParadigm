#lang racket

(require "Model.rkt")
(require "extras.rkt")
(require rackunit)
(require "ParticleWorld.rkt")
(require "ControllerFactory.rkt")
(require "ModelTest.rkt")

(provide run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

; Represents the width of the canvas
(define CANVAS-WIDTH 600)

; Represents the height of the canvas
(define CANVAS-HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;run : PosReal -> Void
;;GIVEN: a frame rate, in sec/tick
;;EFFECT: Creates and runs the MVC simulation with the given frame rate.

(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      (send w add-widget
        (new ControllerFactory% [m m][w w]))
      (send w run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;