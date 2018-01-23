#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "SuperController.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "extras.rkt")

(provide ImageController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define RECT-WIDTH 150)
(define RECT-HEIGHT 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The ImageController% class :

;; An ImageController% is a (new ImageController% [model Model<%>])

;; An ImageController% is a Controller<%> that changes that deals with the view and
;; handling of the particle in x, y and xy direction

;; It is the super class for the xController%, yController% and xyController%

(define ImageController%
  (class* SuperController% (Controller<%>)
    
    (field [width-controller-data-image RECT-WIDTH])
    (field [height-controller-data-image RECT-HEIGHT])
    
    ;................................................................................
    
    ;; INTERPRETATION:
    ;--------------------------------------------------------------------------------
    ;; width-controller-data-image : The width of the data image (particle-window)
    ;;                               of the controller
    ;; height-controller-data-image: The height of the data image (particle-window)
    ;;                               of the controller
    ;--------------------------------------------------------------------------------
    
    ; the inherited fields from the super class 
    ; -----------------------------------------
    
    ;; the model
    (inherit-field model)  
    
    ; represents the position of the center of the controller
    (inherit-field x y)   
    
    ; represents the dimensions of the controller
    (inherit-field width height)
    
    ; represents half dimensions of the controller
    (inherit-field half-width half-height)
    
    ; represents the coordinates of center of the particle
    (inherit-field particle-x particle-y)
    
    ; represents the velocity of the particle in x, y direction
    (inherit-field particle-vx particle-vy) 
    
    ;; fields for dragging
    ;; It there has ever been a button-down in this object, then these
    ;; contain the position of last button-down relative to
    ;; center of viewer. Else any value
    
    ; set to true only if mouse click is within the handle of controller
    ; else false
    (inherit-field handle-selected?)
    
    ; set to true only if mouse click is within the controller
    ; else false
    (inherit-field controller-selected?)
    
    ; the coordiantes of the mouse click wrt the handle of controller
    (inherit-field saved-mx)  
    (inherit-field saved-my)  
    
    ;...............................................................................
    
    (super-new)
    
    ;................................................................................
    
    ;; Registering this object to the model to fetch latest position and velocity values
    ;; of the particle
    (send model register this)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: A keyevent 'kev'
    ;; EFFECT: ImageController object ignores all key events
    (define/override (after-key-event kev)
      this)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; perform-button-down : Integer Integer -> Void
    ;; GIVEN : The location 'l' of the click of the mouse
    ;; EFFECT: Updates the controller's selected field to true
    
    (define/override (perform-button-down mx my)
      (begin
        (set! saved-mx (- mx particle-x))
        (set! saved-my (- my particle-y))
        (set! controller-selected? true)
        (send model execute-command
              (make-set-selected controller-selected?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;