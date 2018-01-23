;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;PDP Problem Set 07 : Question 03;;;;;;;;;;;;;;;;;;;;;;;

(require "extras.rkt")
(require rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide
 rainfall)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A ListOfNumber (LON) is one of
;; -- empty
;; -- (cons Number LON)

;; TEMPLATE:
;; list-fn : LON -> ??
#; (define (list-fn lon)
     (cond
       [(empty? lon)...]
       [else (... (first lon)
                  (list-fn (rest lon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSATNTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ZERO 0)
(define ONE 1)
(define TERMINATE -999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rainfall: LON -> NonNegNumber
;; GIVEN: a list of numbers where each number represents amount of rainfall on a 
;; particular day
;; RETURNS: the average of all positive numbers from the given list
;; EXAMPLES:
;; (rainfall (list 20 10 50 20))=25
;; (rainfall (list 20 -10 50))=35
;; (rainfall (list -999 20 10 50))=0
;; (rainfall (list 0 0))=0
;; (rainfall (list -10 -20 -30))=0
;; (rainfall (list 10 20 -999))=15
;; STRATEGY: Combine simpler functions

(define (rainfall lst)
  (if (equal? (cal-length lst ZERO) ZERO)
      ZERO
      (cal-average lst)))

;;TESTS:
(begin-for-test
  (check-equal? (rainfall (list 20 10 50 20)) 25
                "must return average of all positive numbers")
  (check-equal? (rainfall (list 20 -10 50)) 35
                "must return average of all positive numbers, ignoring negative
                 numbers")
  (check-equal? (rainfall (list -999 20 10 50)) 0
                "must return average for all numbers in the list before -999")
  (check-equal? (rainfall (list 0 0)) 0
                "must return average as 0")
  (check-equal? (rainfall (list -10 -20 -30)) 0
                "must return average as 0")
  (check-equal? (rainfall (list 10 20 -999)) 15
                "must return average as 15"))

;........................................................................................
;; cal-average : LON -> NonNegNumber
;; GIVEN: a list of numbers where each number represents amount of rainfall on a 
;; particular day
;; RETURNS: average of all psitive numbers when list of numbers is not empty
;; Examples/Tests: See function rainfall
;; Design Strategy: Combine simpler functions
(define (cal-average lst)
  (/ (cal-sum lst) (cal-length lst ZERO)))

;........................................................................................
;; cal-sum : LON -> NonNegNumber
;; GIVEN: a list of numbers where numbers represent amount of rainfall on a 
;; particular day 
;; RETURNS: Sum of all positive numbers from the list
;; Examples/Tests: See function rainfall
;; Design/Strategy: Use template for LON on lst
(define (cal-sum lst)
  (cond
    [(empty? lst) ZERO]
    [else (if (check-terminate? (first lst))
              ZERO
              (call-helper-sum lst ))]))
;........................................................................................
;; call-helper-sum : LON -> NonNegNumber
;; GIVEN: a list of numbers where numbers represent amount of rainfall on a 
;; particular day 
;; RETURNS: Sum of all positive numbers from the list
;; Examples/Tests: See function rainfall
;; Design/Strategy: Combine simpler functions
(define (call-helper-sum lst)
  (if  (check-positive? (first lst))
       (+ (first lst) (cal-sum (rest lst)))
       (cal-sum (rest lst))))

;........................................................................................
;; cal-length : LOR Number -> NonNegNumber
;; GIVEN: a list of numbers where numbers represent amount of rainfall on a 
;; particular day, a numeric value n
;; WHERE: n represents number of postive numbers contained within the sublist traversed
;;        in the original list of numbers
;; RETURNS: total number of positive numbers from the list
;; Examples/Tests: See function rainfall
;; Design Strategy: Use template for LON on lst

(define (cal-length lst n)
  (cond
    [(empty? lst) n]
    [else (if (check-terminate? (first lst))
              n
              (call-helper lst n))]))
;.......................................................................................
;; call-helper : LOR Number -> NonNegNumber
;; GIVEN: a list of numbers where numbers represent amount of rainfall on a 
;; particular day, a numeric value n
;; WHERE: n represents number of postive numbers  contained within the sublist traversed
;;        in the original list of numbers
;; RETURNS: total number of positive numbers from the list
;; Examples/Tests: See function rainfall
;; Design Strategy: Combine simpler functions
(define (call-helper lst n)
  (if (check-positive? (first lst))
      (cal-length (rest lst) (+ n ONE))
      (cal-length (rest lst) n)))
;........................................................................................
;; check-terminate? : Number -> Boolean
;; GIVEN: a number from the list of numbers where numbers represent amount of
;; rainfall on a particular day
;; RETURNS: true iff the number is -999, the terminating value
;; Examples/Tests: See function rainfall
;; Design Strategy: Combine simpler functions
(define (check-terminate? num)
  (equal? num TERMINATE))


;; check-positive : Number -> Boolean
;; GIVEN: a number from the list of numbers where numbers represent amountof
;; rainfall on a particular day
;; RETRNS: true iff the number from the list is positive
;; Examples/Tests: See function rainfall
;; Design Strategy: Combine simpler functions
(define (check-positive? num)
  (>= num ZERO))
