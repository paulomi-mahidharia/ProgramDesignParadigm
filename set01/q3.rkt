;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;SET 01 - QUES 03;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

;;DATA DEFINITION : none

;;image-area: Image -> PosReal

;;GIVEN: An image
;;RETURNS: The number of pixels in a given image (area of image)

;;EXAMPLES:
;;(image-area CAT)=8775
;;(image-area RECTANGLE)=200


;;DESIGN STRATEGY: Combine simpler functions
                                           
(define CAT (bitmap "C:/Users/patelinfo/Desktop/cat.jpg"))    
(define RECTANGLE (rectangle 10 20 "solid" "blue"))

(define (image-area i)
  (* (image-height i) (image-width i)))


;;TESTS:
(begin-for-test
  (check-equal? (image-area CAT) 8775 "image area of cat is 8775")
  (check-equal? (image-area RECTANGLE) 200 "image area of rectangle is 200"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;END OF PROBLEM;;;;;;;;;;;;;;;;;;;;;;;;;;