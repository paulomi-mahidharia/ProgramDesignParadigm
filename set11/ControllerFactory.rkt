#lang racket

(require "Interfaces.rkt")
(require "Model.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "ParticleWorld.rkt")
(require "xController.rkt")
(require "yController.rkt")
(require "xyController.rkt")
(require 2htdp/universe)

(provide ControllerFactory%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CONSTANTS

; constants for various keyevents
(define VELOCITY-CONTROLLER "v")
(define POSITION-CONTROLLER "p")
(define X-CONTROLLER "x")
(define Y-CONTROLLER "y")
(define XY-CONTROLLER "z")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a ControllerFactory is
;      (new ControllerFactort% [w <World%>] [m Model<%>]

(define ControllerFactory%
  (class* object% (SWidget<%>)
    
    ; the world in which the controllers will live
    (init-field w)   ; World<%>
    
    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>
    
    (super-new)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : a KeyEvent
    ; EFFECT : creates an instance of controller based on the
    ;          KeyEvent
    ; STRATEGY : Cases on KeyEvents
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev VELOCITY-CONTROLLER) (add-viewer VelocityController%)]
        [(key=? kev POSITION-CONTROLLER) (add-viewer PositionController%)]
        [(key=? kev X-CONTROLLER) (add-viewer xController%)]
        [(key=? kev Y-CONTROLLER) (add-viewer yController%)]
        [(key=? kev XY-CONTROLLER) (add-viewer xyController%)]
        ))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; add-viewer : Controller<%> -> Void
    ;; GIVEN : A controller
    ;; EFFECT : adds a new controller on the world
    ;; STRATEGY : Combining simple functions
    
    (define/public (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define/public (add-to-scene s) s)
    (define/public (after-tick) 'controller-factory-after-tick-trap)    
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; testcases:
(define M (new Model%)) 
(define F (new ControllerFactory% [w (make-world M 600 500)] [m M]))
(send F after-key-event "x")
(send F after-key-event "y")
(send F after-key-event "z")
(send F after-key-event "v")
(send F after-key-event "p")

