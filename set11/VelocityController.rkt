#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "Interfaces.rkt")
(require "TextController.rkt")
(require "extras.rkt")
(require "Model.rkt")

(provide VelocityController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The VelocityController% class :

;; a VelocityController% is a (new VelocityController% [model Model<%>])

;; displays as an outline rectangle with text showing the x and y
;; coordinate and vx and vy velocity of the particle.

;; the rectangle is draggable

;; the arrow keys increments or decrements velocity of the particle by 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define VelocityController%
  (class* TextController% (Controller<%>)
    
    ; height and width of the data-image 
    (inherit-field height-data-image)
    (inherit-field width-data-image)
    
    ; the model
    (inherit-field model)
    
    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)  
    
    ;; the velocity of the particle
    (inherit-field particle-vx)
    (inherit-field particle-vy)   
    
    ; set to true only if mouse click is within the controller,
    ; else false
    (inherit-field controller-selected?)
    
    ; set to true only if mouse click is within the controller,
    ; else false
    (inherit-field handle-selected?)
    
    ; The padding for display in the controller
    (inherit-field SPACE)
    
    ;; the KeyEvents possible on the controller
    (inherit-field UP DOWN LEFT RIGHT INVALID)
    
    ;................................................................................
    
    (super-new)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Registering this object to the model to fetch latest position and velocity values
    ;; of the particle
    (send model register this)    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    
    ;; KeyEvent -> Void
    ;; interpret +,- as commands to the model
    ;; +/- alter velocity of the particle
    (define/override (after-key-event kev)   ;********
      (if controller-selected?
          (cond
            [(key=? UP kev)
             (send model execute-command (incr-velocity 0 5))]
            [(key=? DOWN kev)
             (send model execute-command (incr-velocity 0 -5))]
            [(key=? LEFT kev)
             (send model execute-command (incr-velocity -5 0))]
            [(key=? RIGHT kev)
             (send model execute-command (incr-velocity 5 0))]
            [else 2345])
          2345))
    
    ;; assemble the image of the viewer
    (define/override (viewer-image)   ;********
      (let ((the-data-image (data-image)))
        (begin
          (set! width-data-image (image-width the-data-image))
          (set! height-data-image (image-height the-data-image))
          (send this final-controller-width width-data-image)
          (send this final-controller-height height-data-image)
          the-data-image)))
    
    
    (define/override (data-image)     ;******** 
      (above
       (text "Arrow keys change velocity" 10 (update-view-controller))
       (text (string-append
              "X = "
              (real->decimal-string particle-x)
              "Y = "
              (real->decimal-string particle-y)) 11 (update-view-controller))
       (text (string-append
              " VX = "
              (real->decimal-string particle-vx)
              " VY = "
              (real->decimal-string particle-vy)) 11 (update-view-controller))))
    
    
    (define/override (update-view-controller)
      (super update-view-controller))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods for testing
    
    (define/public (for-test:get-particle-vx)
      particle-vx)
    
    (define/public (for-test:get-particle-vy)
      particle-vy)
    
    (define/public (for-test:controller-selected?)
      controller-selected?)
    
    (define/public (for-test:handle-selected?)
      handle-selected?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST-CASE :


(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] ))
     
     (define DATA-IMAGE-HEIGHT (image-height (send VEL-CONTROL data-image)))
     (define DATA-IMAGE-WIDTH (image-width (send VEL-CONTROL data-image)))
     (define VIEWR-IMAGE (send VEL-CONTROL viewer-image))
     (define MOUSE-X DATA-IMAGE-WIDTH)
     (define MOUSE-Y DATA-IMAGE-HEIGHT))
    (check-equal? (send VEL-CONTROL in-this?
                        (+ DATA-IMAGE-WIDTH 5) DATA-IMAGE-HEIGHT) false)
    (check-equal? (send VEL-CONTROL in-handle?
                        (+ DATA-IMAGE-WIDTH 5) DATA-IMAGE-HEIGHT) false)
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 990 890)
                    (send VEL-CONTROL after-key-event "up")
                    
                    (send VEL-CONTROL for-test:get-particle-vx)) 0)
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 90 90)
                    (send VEL-CONTROL after-key-event "up")
                    (send VEL-CONTROL after-key-event "down")
                    (send VEL-CONTROL after-key-event "left")
                    (send VEL-CONTROL after-key-event "right")
                    (send VEL-CONTROL for-test:get-particle-vx)) 0)))



(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL after-key-event "right")
                    (send VEL-CONTROL for-test:get-particle-vx)) 5)))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL after-key-event "left")
                    (send VEL-CONTROL for-test:get-particle-vx)) -5)))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL after-key-event "up")
                    (send VEL-CONTROL for-test:get-particle-vy)) 5)))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL after-key-event "down")
                    (send VEL-CONTROL for-test:get-particle-vy)) -5)))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL after-key-event "n")
                    (send VEL-CONTROL for-test:get-particle-vx)) 0)))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL for-test:controller-selected?)) true)
    (check-equal? (begin
                    (send VEL-CONTROL after-button-up 300 250)
                    (send VEL-CONTROL for-test:controller-selected?)) false)))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 230 205)
                    (send VEL-CONTROL for-test:handle-selected?))
                  true
                  "Button down is within the handle")))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 300 250)
                    (send VEL-CONTROL for-test:handle-selected?))
                  false
                  "Button down is not within the handle")))

(begin-for-test
  (local
    ((define VEL-CONTROL (new VelocityController% [model (new Model%)] )))
    (check-equal? (begin
                    (send VEL-CONTROL after-button-down 225 200)
                    (send VEL-CONTROL for-test:handle-selected?)
                    (send VEL-CONTROL after-drag 226 199)
                    (send VEL-CONTROL for-test:get-particle-vx)) 0)))




