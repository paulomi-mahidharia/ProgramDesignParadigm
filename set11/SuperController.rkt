;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; displays as an outline rectangle with text showing the x                   ;;
;; coordinate and velocity of the particle.                                   ;;
;; the rectangle is draggable                                                 ;;
;; right,left increments or decrements the speed of the particle              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "Interfaces.rkt")
(require "extras.rkt")

(provide SuperController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GLOBAL CONSTANTS :

; dimensions of the canvas
(define CANVAS-WIDTH 600)
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define CANVAS-HEIGHT 500)
(define CANVAS-HEIGHT-HALF (/ CANVAS-HEIGHT 2))

; dimensions of the controller
(define CONTROLLER-WIDTH 150)    
(define CONTROLLER-HEIGHT 100)

; dimension of the handle in the controller
(define HANDLE-LEN 10)

; color for selected
(define SELECTED-COLOR "red")
(define UNSELECTED-COLOR "black")

; mode of image
(define OUTLINE "outline")

; overlay offset for handle
(define HANDLE-POS 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The SuperController% class :

;; A SuperController% is a (new SuperController% [model Model<%>])

;; A SuperController% implements the the Controller<%> interface

;; A SuperController% is a Controller<%> that changes the particle's position 
;; and velocity in the x and y direction.

;; It is the super class for the TextController% and ImageController% 

(define SuperController%
  (class* object% (Controller<%>) 
    
    (init-field model)
    
    ;................................................................................
    
    ;; INTERPRETATION:
    ;--------------------------------------------------------------------------------
    ;; model : The model for the controller from where the information of the particle
    ;;         could be fetched
    ;--------------------------------------------------------------------------------
    
    ;; The x and y coordinate position of the controller  
    (field [x CANVAS-WIDTH-HALF] [y CANVAS-HEIGHT-HALF])
    
    ;; The width and height of the controller (in pixels)
    (field [width CONTROLLER-WIDTH][height CONTROLLER-HEIGHT])
    
    ;; Half of the width and height of the controller
    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])
    
    ;; The x and y coordinate position of the particle in the controller
    (field [particle-x 0])
    (field [particle-y 0])
    
    ;; The velocity of the particle in the controller in x and y direction
    (field [particle-vx 0])
    (field [particle-vy 0])
    
    ;; Padding for the display of controller
    (field [SPACE 30])
    
    ;; fields for dragging
    ;; It there has ever been a button-down in this object, then these
    ;; contain the position of last button-down relative to
    ;; center of viewer.  Else any value
    
    ;; set to true only if mouse click is within the handle of controller
    ;; else false
    (field [handle-selected? false])
    
    ;; set to true only if mouse click is within the controller
    ;; else false
    (field [controller-selected? false])
    
    ;; the coordiantes of the mouse click wrt the handle of controller
    (field [saved-mx 0])
    (field [saved-my 0])
    
    ;...............................................................................
    
    (super-new)
    
    ;................................................................................
    
    ;; Registering this object to the model to fetch latest position and velocity values
    ;; of the particle
    (send model register this)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; in-this? -> NonNegInt NonNegInt -> Boolean
    ; GIVEN : the x and y coordinates of the mouse click
    ; RETURNS : true, iff the mouse click is within the controller,
    ;           else false
    ; STRATEGY : Combining simpler function
    
    (define/public (in-this? other-x other-y)
      (and
       (<= (- x  half-width) other-x (+ x half-width))
       (<= (- y half-height) other-y (+ y half-height))))
    
    ;................................................................................
    
    ; in-handle? -> NonNegInt NonNegInt -> Boolean
    ; GIVEN : the x and y coordinates of the mouse click
    ; RETURNS : true, iff the mouse click is within the handler of the
    ;           controller, else false
    ; STRATEGY : Combining simpler function
    
    (define/public (in-handle? other-x other-y)
      (and
       (<= (- x  (/ width 2)) other-x (+ (- x (/ width 2)) HANDLE-LEN)) 
       (<= (- y  (/ height 2)) other-y  (+ (- y  (/ height 2)) HANDLE-LEN))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Signal -> Void
    ;; EFFECT  : decodes signal and updates local data
    ;; DETAILS : updates the x and y coordinate position of the particle by fetching it
    ;;           from the values received from the signal
    ;; STRATEGY: Cases on whether the signal is for reporting position or velocity
    
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (begin
           (set! particle-x (report-position-pos-x sig))
           (set! particle-y (report-position-pos-y sig)))]
        
        [(report-velocity? sig)
         (begin
           (set! particle-vx (report-velocity-v-x sig))
           (set! particle-vy (report-velocity-v-y sig)))]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN   : the location of a button-down event
    ;; EFFECT  : selects either the handle or the controller area except the handle
    ;; DETAILS : If the handle is selected, the rectangle can be dragged.
    ;;         : If the area of rectangle except the handle is selected, then drag is
    ;;         : is diabled
    ;; STRATEGY: Cases on whether the event is within the handle of this object or 
    ;;           outside handle but within the object
    
    (define/public (after-button-down mx my)
      (cond
        [(in-handle? mx my) 
         (begin
           (set! saved-mx (- mx x))
           (set! saved-my (- my y))
           (set! handle-selected? true))]
        [(in-this? mx my)
         (perform-button-down mx my)]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: the (x,y) location of a button-up event
    ;; EFFECT: makes this unselected
    (define/public (after-button-up mx my)
      (begin
        (set! handle-selected? false)
        (set! controller-selected? false)
        (send model execute-command
              (make-set-selected controller-selected?))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: the location of a drag event
    ;; STRATEGY: Cases on whether this is selected.
    ;; If it is selected, move it so that the vector from its position to
    ;; the drag event is equal to saved-mx.  Report the new position to
    ;; the registered balls.
    (define/public (after-drag mx my)
      (if handle-selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          2744))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/public (add-to-scene scene)
      (local ((define viewer (viewer-image)))
        (place-image
         (overlay
          (overlay/xy
           (square HANDLE-LEN OUTLINE (current-color)) HANDLE-POS HANDLE-POS
           (rectangle width height OUTLINE UNSELECTED-COLOR)) 
          viewer) 
         x y scene)))    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; final-controller-width : NonNegInt -> NonNegInt
    ;; GIVEN : the width of the controller image without includng the handler
    ;; RETURN : the width of the controller image includng the handler
    ;;
    (define/public (final-controller-width width-data-iamge)
      (set! width (+ width-data-iamge SPACE)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; final-controller-height : NonNegInt -> NonNegInt
    ;; GIVEN : the width of the controller image without includng the handler
    ;; RETURN : the width of the controller image includng the handler
    
    (define/public (final-controller-height height-data-iamge)
      (set! height (+ height-data-iamge SPACE)))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; set-particle : Int Int -> Void
    ;; GIVEN : the offset between x and y coordiantes of mouse click and
    ;;         saved mouse click 
    ;; EFFECT : send signal to update the new x and y in model
    
    (define/public (set-particle new-x new-y)
      (send model execute-command
            (make-set-position new-x new-y)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-tick : -> Void
    ;; GIVEN : No arguments
    ;; EFFECT : no effect
    
    (define/public (after-tick) 'viewer1-after-tick-trap)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; current-color : -> Color
    ;; GIVEN : no arguments
    ;; RETURNS : a Color based on whether the object is selected or not
    
    (define/public (current-color)
      (if handle-selected? SELECTED-COLOR UNSELECTED-COLOR))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; will be defined in the subclass
    (abstract perform-button-down)
    
    ;................................................................................
    
    ;; will be defined in the subclass
    (abstract update-view-controller)
    
    ;................................................................................
    
    ;; will be defined in the subclass
    ;; assemble the image of the viewer
    (abstract viewer-image)
    
    ;................................................................................
    
    ;; will be defined in the subclass
    (abstract data-image)
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; KeyEvent -> Void
    ;; interpret arrow keys as commands to the model
    ;; arrow keys alter position and velocity of the particle in their respective classes
    (abstract after-key-event)))

