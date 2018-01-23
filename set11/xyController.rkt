;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xyController.rkt:                                                          ;;
;; The controller for the particle used to visualise the position at which    ;;
;; the particle is in the x and ydirection                                    ;;
;; The xyController is draggable by holding on the handler which              ;;
;; appears on the left top corner of the controller                           ;;
;; The particle appears as a red circle and is draggable inside the bounds    ;;
;; of the controller. The particle follows the mouse pointer after the mouse  ;;
;; is clicked anywhere inside the controller.                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "ImageController.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "extras.rkt")

(provide xyController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; constants for the display of controller
(define RADIUS-SMALL 2 )
(define RADIUS-BIG 10)
(define STYLE-MODE-CIRCLE "solid")
(define STYLE-MODE-RECTANGLE "outline")
(define COLOR-RECTANGLE "blue")
(define COLOR-SMALL-CIRCLE "black")
(define COLOR-BIG-CIRCLE "red")
(define HANDLE-LENGTH 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The xyController% class :

;; A xyController% is  a
;;          (new xyController% [height-data-image NonNegInt]
;;                             [width-data-image NonNegInt])

;; A xyController% is a (new xyController% [model Model<%>]) ???

;; xyController% is a subclass of ImageController% class and it implements the
;; Controller<%> interface.

;; An XY controller, which shows a representation of the particle bouncing in the
;; rectangle.

;; The controller can de dragged by mouse drag KeyEvent if the mouse click is
;; within the handler which appears on the left top corner of the controller

;; The particle, represented as a circle, can be dragged if the event
;; mouse drag is inside the controller and the drag alters the x and y
;; coordinate of particle's center

;; The mouse button down pauses the particle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define xyController%
  (class* ImageController% (Controller<%>)
    
    ;; The width of the image of the controller
    ;; The height of the image of the controller
    (inherit-field width-controller-data-image)
    (inherit-field height-controller-data-image)
    
    ; the model
    (inherit-field model) 
    
    ; represents the position of the center of the controller
    (inherit-field x y)   
    
    ; represents the dimensions of the controller
    (inherit-field width height)
    
    ; represents half dimensions of the controller
    (inherit-field half-width )
    (inherit-field half-height)
    
    ; represents the coordinates of center of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)
    
    ; represents the velocity of the particle in x, y direction
    (inherit-field particle-vx) 
    (inherit-field particle-vy)
    
    ; set to true only if mouse click is within the handle of controller,
    ; else false
    (inherit-field handle-selected?)
    
    ; set to true only if mouse click is within the controller,
    ; else false
    (inherit-field controller-selected?)
    
    ; the coordiantes of the mouse click wrt the handle of controller,
    (inherit-field saved-mx)  
    (inherit-field saved-my)
    
    ; The padding for display in the controller
    (inherit-field SPACE)
    
    ;................................................................................
    
    (super-new)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Registering this object to the model to fetch latest position and velocity values
    ;; of the particle
    (send model register this)    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; update-view-controller -> String
    ;; GIVEN: No arguments
    ;; RETURNS: the colour of the controller based on whether it is selected or
    ;;          not.
    
    (define/override (update-view-controller mx my)
      (begin
        (send this set-particle (- mx saved-mx) (- my saved-my))))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag : Integer Integer
    ;; GIVEN: The location 'l' of the click of the mouse
    ;; EFFECT: Updates the particles position after mouse drags
    (define/override (after-drag mx my)
      (cond
        [controller-selected?
         (update-view-controller mx my)]
        [handle-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; viewer-image:  -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: Assembles the image of the viewer
    (define/override (viewer-image)
      (let ((the-data-image (data-image)))
        (begin
          (set! width-controller-data-image (image-width the-data-image))
          (set! height-controller-data-image (image-height the-data-image))
          (send this final-controller-width width-controller-data-image)
          (send this final-controller-height height-controller-data-image)
          the-data-image)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; data-image : -> Image
    ;; GIVEN : The location 'l' of the click of the mouse
    ;; EFFECT: Updates the controller's selected field to true
    (define/override (data-image) 
      (overlay 
       (place-image
        (overlay (circle RADIUS-SMALL STYLE-MODE-CIRCLE COLOR-SMALL-CIRCLE)
                 (circle RADIUS-BIG STYLE-MODE-CIRCLE COLOR-BIG-CIRCLE))
        particle-x particle-y
        (rectangle width-controller-data-image height-controller-data-image STYLE-MODE-RECTANGLE COLOR-RECTANGLE))
       (rectangle width-controller-data-image height-controller-data-image STYLE-MODE-RECTANGLE COLOR-RECTANGLE)
       ))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; set-handle : bool -> Void
    ; GIVEN : Boolean
    ; EFFECT : Updates the handle-selected?
    (define/public (set-handle bool)
      (set! handle-selected? bool))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    
    ; set-controller-selected : bool -> Void
    ; GIVEN : Boolean
    ; EFFECT : Updates the controller-selected?
    
    (define/public (set-controller-selected bool)
      (set! controller-selected? bool))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TEST CASES :

(define xyController1 (new xyController% [model (new Model%)] ))
(define xyController2 (new xyController% [model (new Model%)] ))
(define XY-CONTROL (new xyController% [model (new Model%)] ))
(define DATA-IMAGE-HEIGHT (image-height (send XY-CONTROL data-image)))
(define DATA-IMAGE-WIDTH (image-width (send XY-CONTROL data-image)))
(define VIEWR-IMAGE (send XY-CONTROL viewer-image)) 

(define MOUSE-X 300)
(define MOUSE-Y 250) 
(define MOUSE-X1 225)
(define MOUSE-Y1 350)
(define MOUSE-X2 220)
(define MOUSE-Y2 195)

(define xyCon-AFTER-KEY-EVENT (send xyController1 after-key-event "left"))
(define xyCon-AFTER-MOUSE-BUTTON-DOWN (send xyController1 after-button-down
                                            MOUSE-X MOUSE-Y))
(define xyCon-AFTER-MOUSE-DRAG1 (send xyController1 after-drag (+ MOUSE-X 2)
                                      (+ MOUSE-Y 2)))


(define xyCon-AFTER-MOUSE-BUTTON-DOWN2 (send xyController2 after-button-down
                                             MOUSE-X2 MOUSE-Y2))
(define xyCon-AFTER-MOUSE-DRAG2 (send xyController2 after-drag MOUSE-X2
                                      MOUSE-Y2))



(begin-for-test
  (check-equal? (send xyController1 in-this?
                      (+ MOUSE-X 5) MOUSE-Y) true
                                             "it should be inside controller")
  (check-equal? (send xyController1 in-handle?
                      (+ MOUSE-X1 ) MOUSE-Y1) false
                                              "it should not be inside handler") 
  (check-equal? (send xyCon-AFTER-KEY-EVENT in-this?
                      (+ MOUSE-X 5) MOUSE-Y) true
                                             "it should be inside controller")
  
  (check-equal? (send xyController1 in-this?
                      (+ MOUSE-X 2) MOUSE-Y) true
                                             "it should be inside controller") 
  
  (check-equal? (send xyController1 in-handle?
                      (+ MOUSE-X 2) MOUSE-Y) false
                                             "it should not be inside handler")
  
  (check-equal? (send xyController2 in-handle?
                      MOUSE-X2 MOUSE-Y2) false
                                         "it should not be inside handler"))
(begin
  (send xyController2 set-handle true)
  (send xyController2 set-controller-selected false)
  (send xyController2 after-drag MOUSE-X2
        MOUSE-Y2)
  (check-equal?   (send xyController2 in-handle?
                        MOUSE-X2 MOUSE-Y2) false
                                           "it should not be inside handler"))
