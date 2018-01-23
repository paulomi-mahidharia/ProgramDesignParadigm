;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;SET 01 - QUES 04;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

;;DATA DEFINITION : none

;;string-insert: String NonNegNumber -> String

;;GIVEN: A string and a number i 
;;RETURNS: A string with "_" inserted at the ith position of the given string

;;EXAMPLES:
;;(string-insert "paulomi" 4)="paul_omi"
;;(string-insert "I like USA" 1)="I_ like USA"
;;(string-insert "" 2 )="Invalid! Either empty string or out-of-bound index position!"
;;(string-insert "Miami" 6)="Invalid! Either empty string or out-of-bound index position!"

;;DESIGN STRATEGY: Combine simpler functions

(define (string-insert str i)
  (if (or (= (string-length str) 0) (>= i (string-length str)))
      "Invalid! Either empty string or out-of-bound index position!"  
      (string-append (substring str 0 i) "_" (substring str i)))
)

;;TESTS:
(begin-for-test
  (check-equal? (string-insert "paulomi" 4) "paul_omi"
                "After insertion paulomi becomes paul_omi")
  (check-equal? (string-insert "I like USA" 1) "I_ like USA"
                "After insertion I like USA becomes I_ like USA")
  (check-equal? (string-insert "" 2)
                "Invalid! Either empty string or out-of-bound index position!"
                "Empty string error")
  (check-equal? (string-insert "Miami" 6)
                "Invalid! Either empty string or out-of-bound index position!"
                "The index position is greater than total number of characters"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;END OF PROBLEM;;;;;;;;;;;;;;;;;;;;;;;;;;