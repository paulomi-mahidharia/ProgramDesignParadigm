;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;SET 01 - QUES 05;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

;;DATA DEFINITION : none

;;string-delete: String NonNegNumber -> String

;;GIVEN: A string and a number i 
;;RETURNS: A string which the ith position deleted from the given string

;;EXAMPLES
;;(string-delete "paulomi" 5)="pauloi"
;;(string-delete "I like USA" 1)="Ilike USA"
;;(string-delete "" 2)="Invalid! Either empty string or out-of-bound index position!"
;;(string-delete "Miami" 6)="Invalid! Either empty string or out-of-bound index position!"

;;DESIGN STRATEGY: Combine simpler functions

(define (string-delete str i)
  (if (or (= (string-length str) 0) (>= i (string-length str)))
      "Invalid! Either empty string or out-of-bound index position!"  
      (string-append (substring str 0 i) (substring str (+ i 1))))
)

;;TESTS:
(begin-for-test
  (check-equal? (string-delete "paulomi" 5) "pauloi"
                "After deletion paulomi becomes pauloi")
  (check-equal? (string-delete "I like USA" 1) "Ilike USA"
                "After deletion I like USA becomes Ilike USA")
  (check-equal? (string-delete "" 2)
                "Invalid! Either empty string or out-of-bound index position!"
                "Empty string error")
  (check-equal? (string-delete "Miami" 6)
                "Invalid! Either empty string or out-of-bound index position!"
                "The index position is greater than total number of characters"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;END OF PROBLEM;;;;;;;;;;;;;;;;;;;;;;;;;;