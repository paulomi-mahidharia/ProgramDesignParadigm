;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;SET 01 - QUES 01;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

;;DATA DEFINITION : none

;;CONTRACT :
;;distance-to-origin: Real Real -> NonNegReal

;;PURPOSE STATEMENT :
;;GIVEN: Values of x and y that together represent point (x,y) on the cartesian plane
;;RETURNS: Distance between the given coordinate point (x,y) and the origin (0,0)
;;         on the cartesian plane

;;EXAMPLES:
;;(distance-to-origin 3 4)=5
;;(distance-to-origin -12 5)=13
;;(distance-to-origin 0 0)=0

;;DESIGN STRATEGY: Combine simpler functions

(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

;;TESTS:
(begin-for-test
  (check-equal? (distance-to-origin 3 4) 5 "Distance between (0,0) and (3,4) is 5")
  (check-equal? (distance-to-origin -12 5) 13 "Distance between (0,0) and (-12,5) is 13")
  (check-equal? (distance-to-origin 0 0) 0 "Distance between (0,0) and (0,0) is 10"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;