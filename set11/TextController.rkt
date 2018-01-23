#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "Interfaces.rkt")
(require "SuperController.rkt")
(require "extras.rkt")
(require "Model.rkt")

(provide TextController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL CONSTANTS :

(define SELECTED-COLOR "red")
(define UNSELECTED-COLOR "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The TextController% class :

;; An TextController% is a (new TextController% [model Model<%>])

;; An TextController% is a Controller<%> that changes the values of the position and
;; velocity of a particle in the controller

;; It is the super class for the PositionController% and VelocityController%

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TextController%
  (class* SuperController% (Controller<%>)
    
    ;; The height and width of the data image of the controller 
    (field [height-data-image 0])
    (field [width-data-image  0])
    
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
    
    ;................................................................................
    ;; fields for key events :
    
    (field [UP "up"])
    (field [DOWN "down"])
    (field [LEFT "left"])
    (field [RIGHT "right"])
    (field [INVALID "n"])
    
    ;...............................................................................
    
    (super-new) 
    
    ;................................................................................
    
    ;; Registering this object to the model to fetch latest position and velocity values
    ;; of the particle
    (send model register this)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; perform-button-down : Integer Integer -> Void
    ;; GIVEN : The location 'l' of the click of the mouse
    ;; EFFECT: Updates the controller's selected field to true
    
    (define/override (perform-button-down mx my)
      (set! controller-selected? true))
    
    ;................................................................................
    
    ;; update-view-controller : -> String
    ;; RETURNS : the color of controller's data based on its selection
    (define/override (update-view-controller)   
      (if controller-selected? SELECTED-COLOR UNSELECTED-COLOR))
    ))
