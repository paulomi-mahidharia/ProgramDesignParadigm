#lang racket


(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
 make-playground
 make-block
 Block<%>
 ;get-team
 ;add-teammate
)

(define StatefulWorld<%>
  (interface ()

    ; Integer Integer MouseEvent-> Void
    ; GIVEN: a location
    ; EFFECT: updates this world to the state that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this world to the state that should follow the
    ; given key event
    after-key-event      

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene

   ; Widget -> Void
   ; GIVEN: A widget
   ; EFFECT: adds the given widget to the world
   add-widget

   ; SWidget -> Void
   ; GIVEN: A stateful widget
   ; EFFECT: adds the given widget to the world
   add-stateful-widget))


(define SWidget<%> 
  (interface()          

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene))


(define (run rate) 
      (big-bang  (make-playground)
      (on-draw   (lambda (w) (send w to-scene)))
      (on-key    (lambda (w kev) (begin (send w after-key-event kev)))) 
      (on-mouse  (lambda (w mx my mev)
                   (begin (send w after-mouse-event mx my mev)
                          w))))) 


(define (make-block mx my blocks)
  (new Block-class% [x mx] [y my]
                 [selected? false]
                 [mx 0]
                 [my 0]
                 [blocks blocks]
                 [teammates empty]
                 [parent-rel-x 0]
                 [parent-rel-y 0]))


(define (make-playground) 
  (new PlayGroundState% [w-mx 200][w-my 250])) 


(define PlayGroundState%
  (class* object% (StatefulWorld<%>)

    (field [canvas-width 500])
    (field [canvas-height 600]) 
    (field [EMPTY-SCENE (empty-scene canvas-width canvas-height)])       


    (init-field w-mx w-my) ; lets consider this as the initial mouse coordinates
    (init-field [sobjs empty])  ; ListOfSWidget

    (field [EMPTY-CANVAS (empty-scene canvas-width canvas-height)])

    (super-new)
 
    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Widgets and SWidgets in this World 
    ;; Note: the append is inefficient, but clear.
    (define/public (to-scene)        
     (foldr
      (lambda (obj scene) (send obj add-to-scene scene))
        EMPTY-SCENE
      sobjs)) 

    
    ;; after-key-event : KeyEvent -> WorldState
    ;; STRATEGY: Pass the KeyEvents on to the objects in the world.
    (define/public (after-key-event kev)
      (cond 
        [(key=? kev "b")
         ;(make-block w-mx w-my sobjs)]))
         (begin (set! sobjs (add-stateful-widget)) this)]
        [else this])) 

  
    ;; world-after-mouse-event : Nat Nat MouseEvent -> WorldState
    ;; STRATGY: Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down") (world-after-button-down mx my)]
        [(mouse=? mev "drag")        (world-after-drag mx my)]
        [(mouse=? mev "button-up")   (world-after-button-up mx my)]
        [else this])) 

    
    ;; the next few functions are local functions, not in the interface.
    (define (world-after-button-down mx my)
      (begin
        (set! w-mx mx)
        (set! w-my my)
        (begin (for-each 
                (lambda (obj) (send obj after-button-down w-mx w-my))
                sobjs) sobjs))) 

     
    (define (world-after-button-up mx my)
      (begin
        (set! w-mx mx)
        (set! w-my my)
        (begin (for-each 
                (lambda (obj) (send obj after-button-up))
                sobjs))))

    
    (define (world-after-drag mx my)
      (begin
        (set! w-mx mx)
        (set! w-my my)
           (begin (for-each 
                   (lambda (obj) (send obj after-drag w-mx w-my))
                   sobjs) sobjs)))

    
    (define/public (add-widget) 
      this)
    
    (define/public (get-widgets) 
      sobjs)

    (define (p objs)
      (for-each (lambda (ele) (printf (string-append(number->string(length (send ele get-blocks))) "..."))) objs))

    
    (define/public (add-stateful-widget)
      (local ((define new-objs (cons (make-block w-mx w-my sobjs) sobjs)))
        (begin (for-each (lambda (ele) (send ele update-blocks new-objs))
                         new-objs) new-objs)))
    ))


(define PLAYGROUND (make-playground))
(define PG-WITH-WIDGET (make-playground))

(begin-for-test (for-each (lambda (ele) (printf (string-append (number->string (length (send ele get-blocks))) "..")))
                          (send (send (send PLAYGROUND after-key-event "b") after-key-event "b") get-widgets)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Block<%>
  (interface(SWidget<%>) 

    get-team
    add-teammate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Block-class%
  (class* object% (Block<%>)

    (init-field x y selected? mx my blocks teammates parent-rel-x parent-rel-y)

    (super-new)

    (define/public (after-tick) this)

    (define/public (after-key-event) this)

    (define/public (update-blocks new-objs)
      (set! blocks new-objs))

    
    (define (p tmates)
      (for-each (lambda (ob) (printf (string-append (number->string (send ob get-x)) ".." "\n"))) tmates))

    (define (pr tm)
      (printf (string-append (number->string (length teammates)) "..." "\n" )))
    
    
    (define/public (after-drag mmx mmy)
      (local ((define get-x-pos (if selected? (- mmx (- mx x)) x))
              (define get-y-pos (if selected? (- mmy (- my y)) y)))
        (begin
        (set! x get-x-pos)
        (set! y get-y-pos)
        (set! mx mmx)
        (set! my mmy)
         (if selected? (drag-teammates mmx mmy)
             234)
            
        (for-each (lambda (ele) (if (and (not-member-of-teammates? ele) (check-if-sides-overlap? ele))
                                    (update-teammates (begin (send ele update-teammates this) ele))
                                    111)) blocks))))

    

        (define/public (drag-teammates mmx mmy)
          (begin
            (for-each
             (lambda (teammate) (begin
                                  (send teammate set-x (- mmx (- mx x) parent-rel-x))
                                  (send teammate set-y (- mmy (- my y) parent-rel-y))
                                  ;(send teammate set-mx mmx)
                                  ;(send teammate set-my mmy)
                                  ))
             teammates)))
    
    (define/public (set-x team-x)
      (set! x  team-x))

        
    (define/public (set-y team-y)
      (set! y  team-y))

        
    (define/public (set-my team-my)
      (set! my  team-my))
        
    (define/public (set-mx team-mx)
      (set! mx  team-mx))

    (define/public (update-teammates ele)
      (set-parent-relative-distance (set! teammates (flatten
                                                     (cons (send ele get-team)
                                                           (cons ele teammates))))))

    
    (define (set-parent-relative-distance num)
      (for-each (lambda (ele) (begin (set-relative-distance-x-for-teammates ele)
                                     (set-relative-distance-y-for-teammates ele)))
                teammates))


    
    (define (set-relative-distance-x-for-teammates teammate)
      (send teammate set-parent-rel-x (- (send teammate get-x) x)))

    
    (define (set-relative-distance-y-for-teammates teammate)
      (send teammate set-parent-rel-y (- (send teammate get-y) y)))

    
    (define (not-member-of-teammates? ele)
      (cond
        [(empty? teammates) true]
        [else (andmap
               (lambda (ele1) (not (equal? ele1 ele)))
               teammates)]))

      
    (define/public (check-if-sides-overlap? ele)
      (local ((define x-diff (- (send ele get-x) x))
              (define y-diff (- (send ele get-y) y)))
        (or (and (<= x-diff 5) (>= x-diff -5)  (= y-diff 20))
            (and (= x-diff 20) (= y-diff 0))
            (and (= x-diff 0) (= y-diff (- 20)))
            (and (= x-diff (- 20)) (= y-diff 0)))))

    
    (define/public (after-button-down mx my) (update-mx-my mx my))
    
    (define/public (is-selected?) selected?)
    
    (define/public (after-button-up)
      (set! selected? false)
      (set! mx mx)
      (set! my my))
    
    
    (define/public (add-to-scene scene)
      (local ((define color (if selected? "red" "green")))
      (place-image (square 40 "outline" color) x y scene)))


    
    (define/public (get-blocks)
      blocks)
    
    (define/public (get-x)
      x)

    
    (define/public (get-y)
      y)

(define/public (set-parent-rel-x parent-x)
      (set! parent-rel-x parent-x))

    
    
(define/public (set-parent-rel-y parent-y)
      (set! parent-rel-y parent-y))

    
    
    (define/public (get-team) teammates)
    
    (define/public (add-teammate) this)

    (define/public (add-block-to-teammate block)
                   (set! teammates (cons block teammates)))
    
    (define/public (update-mx-my w-mx w-my)
      (begin (if (in-block? w-mx w-my)
        (begin
        (set! selected? true)
        (set! mx w-mx)
        (set! my w-my))
        42)))

    (define/public (in-block? w-mx w-my)
      (and
       (<= (- x  20) w-mx (+ x 20))
       (<= (- y  20) w-my (+ y 20))))))
  

(define BLOCK (make-block 200 250 '()))
(define BLOCK1 (make-block 220 250 '()))


    (begin-for-test (equal? (send BLOCK check-if-sides-overlap? BLOCK1) #t))
    
    






