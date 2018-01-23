;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;SET 01 - QUES 02;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

;;DATA DEFINITION : none

;;string-first: String -> String

;;GIVEN: A non-empty string
;;RETURNS: the first 1String (string of length 1) from the given non-empty string

;;EXAMPLES:
;;(string-first "paulomi")="p"
;;(string-first "I like USA")="I"
;;(string-first " PDP q2")=" "

;;DESIGN STRATEGY: Combine simpler functions

(define (string-first s)
  (string-ith s 0))

;;TESTS:
(begin-for-test
  (check-equal? (string-first "paulomi") "p" "first 1String of paulomi is p")
  (check-equal? (string-first "I like USA") "I" "first 1String of I like USA is I")
  (check-equal? (string-first " PDP q2") " " "first 1String of <space>PDP q2 is <space>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;END OF PROBLEM;;;;;;;;;;;;;;;;;;;;;;;;;;
