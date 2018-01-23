;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;CLASS-LISTS.RKT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Two professors - Felleisen and Shivers maintain slips of students
;Duplicate slips from the same professors's class are to be removed
;Mixed slips are to be sorted into slips that belong to
;      a particular professor's class

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 felleisen-roster
 shivers-roster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define YELLOW "yellow")
(define BLUE "blue")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA DEFINITIONS;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct slip (color name1 name2))
;A Slip is a (make-slip Color String String)
;;INTERPRETATION:
;; color is the color of the slip
;; name1 is the firstname or lastname of the student
;; name2 is the firstname or lastname of the student
;;   such that "name1 name2" represents either "firstname lastname"
;;   or "lastname firstname"

;; Template:
;; slip-fn : Slip -> ??
;(define (slip-fn s)
;  (... (slip-color s)
;       (slip-name1 s)
;       (slip-name2 s)))
;...........................................................................
;; ListofSlip

;; A ListOfSlip (LOS) is either
;; -- empty
;; -- (cons Slip LOS)

;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (slip-fn (first los))
;;             (los-fn (rest los)))]))
;............................................................................
;A Color is one of
;-- "yellow"
;-- "blue"

;;TEMPLATE:
#|(define (color-fn c)
  (cond
    [(string=? YELLOW c)...]
    [(string=? BLUE c)...]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CONTANTS FOR TESTING:

(define s1 (make-slip YELLOW "Paulomi" "Mahidharia"))
(define s2 (make-slip BLUE "Jay" "Nathani"))
(define s3 (make-slip YELLOW "Dishant" "Mehta"))
(define s4 (make-slip YELLOW "Mahidharia" "Paulomi"))
(define s5 (make-slip BLUE "Jahnvi" "Gandhi"))
(define s6 (make-slip BLUE "Jay" "Nathani"))
(define s7 (make-slip YELLOW "Jahnvi" "Gandhi"))

(define LOS-WITH-DUP-SWAPED-NAME (list s1 s2 s3 s4))
(define LOS-WITH-DUP-NAME (list s1 s2 s5 s6))
(define LOS-EMPTY empty)
(define LOS-WITHOUT-DUPLICATE (list s1 s2 s3))
(define LOS-ONLY-FELLEISEN (list s1 s3 s4 s7))
(define LOS-ONLY-SHIVERS (list s2 s5 s6))
(define LOS-SAME-SLIP-IN-BOTH (list s2 s4 s5 s7))
(define LOS-ALL (list s1 s2 s3 s4 s5 s6 s7))
(define LOS-ALL-WITHOUT-s1 (list s2 s3 s4 s5 s6 s7))
(define LOS-ALL-WITHOUT-s3 (list s1 s2 s4 s5 s6 s7))
(define SLIP-WITHOUT-COLOR (make-slip "" "Vinni" "Raj"))
(define RED-SLIP (make-slip "red" "Riya" "Singh"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;felleisen-roster : ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;         Felleisen's class, without duplication.
;;STRATEGY: Use template for ListOfSlip on los
;;EXMAPLES:
;;(felleisen-roster LOS-EMPTY)='()
;;(felleisen-roster LOS-WITH-DUP-SWAPED-NAME)=(cons s3 (cons s4 '()))
;;(felleisen-roster LOS-ONLY-FELLEISEN)=(cons s3 (cons s4 (cons s7 '())))
(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [else (if (check-for-felleisen (first los))
              (check-for-duplication (cons (first los) (felleisen-roster (rest los))))
              (felleisen-roster (rest los)))]))
;TESTS:
(begin-for-test
  (check-equal? (felleisen-roster LOS-EMPTY)
                '()
                "Empty list is returned since the class list is empty")
  (check-equal? (felleisen-roster LOS-WITH-DUP-SWAPED-NAME)
                (cons s3 (cons s4 '()))
                "s1 is removed as it is same slip as s4 with first and last names swaped
                 and s2 is removed because it does to not belong to prof Felleison")
  (check-equal? (felleisen-roster LOS-ONLY-FELLEISEN)
                (cons s3 (cons s4 (cons s7 '())))
                "Only slips belonging to Felleisen are retained"))
;.....................................................................................
;;check-for-felleisen : Slip -> Boolean
;;GIVEN: a slip
;;RETURNS: true if it belongs to Prof. Felleisen i.e a yellow slip otherwise false
;;STRATEGY: Use template for Slip on s
;;EXAMPLES:
;;(check-for-felleisen s2)=#false
;;(check-for-felleisen s1)=#true
;;(check-for-felleisen SLIP-WITHOUT-COLOR)=#false
(define (check-for-felleisen s)
  (string=? (slip-color s) YELLOW))

;TESTS:
(begin-for-test
  (check-equal? (check-for-felleisen s2)
                #false
                "Slip s2 does not belong to Felleisen")
  (check-equal? (check-for-felleisen s1)
                #true
                "Slip s1 does not belong to Felleisen")
  (check-equal? (check-for-felleisen SLIP-WITHOUT-COLOR)
                #false
                "Slip with no color does not belong to Felleisen"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;shivers-roster: ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;         Shivers' class, without duplication.
;;STRATEGY: Use template for ListOfSlip on los
;;EXAMPLES:
;;(shivers-roster LOS-EMPTY)='()
;;(shivers-roster LOS-ALL)=(cons s5 (cons s6 '()))
;;(shivers-roster LOS-ONLY-FELLEISEN)='()
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [else (if (check-for-shivers (first los))
              (check-for-duplication (cons (first los) (shivers-roster (rest los))))
              (shivers-roster (rest los)))]))
;TESTS:
(begin-for-test
  (check-equal? (shivers-roster LOS-EMPTY)
                '()
                "Empty list is returned since the class list is empty")
  (check-equal? (shivers-roster LOS-ALL)
                (cons s5 (cons s6 '()))
                "List of slips that belong to Shivers without any duplicate slips")
  (check-equal? (shivers-roster LOS-ONLY-FELLEISEN)
                '()
                "No slips belong to Shivers as they all belong to Felleisen"))
;......................................................................................
;;check-for-shivers : Slip -> Boolean
;;GIVEN: a slip
;;RETURNS: true if it belongs to Prof. Shivers i.e a blue slip otherwise false
;;STRATEGY: Use template for Slip on s
;;EXAMPLES:
;;(check-for-shivers s2)=#true
;;(check-for-shivers s1)=#false
;;(check-for-shivers SLIP-WITHOUT-COLOR)=#false
;;(check-for-shivers RED-SLIP)=#false
(define (check-for-shivers s)
  (string=? (slip-color s) BLUE))

;TESTS:
(begin-for-test
  (check-equal? (check-for-shivers s2)
                #true
                "Slip s2 belongs to Shivers")
  (check-equal? (check-for-shivers s1)
                #false
                "Slip s1 does not belong to Shivers")
  (check-equal? (check-for-shivers SLIP-WITHOUT-COLOR)
                #false
                "Slip with no color does not belong to Shivers")
  (check-equal? (check-for-shivers RED-SLIP)
                #false
                "Slip with red color does not belong to Shivers"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-for-duplication : ListOfSlip -> ListOfSlip
;;GIVEN: a list with or without duplicate slips
;;RETURNS: a list without any duplicate slips
;;STRATEGY: Use template for ListOfSlip on los
;;EXAMPLES:
;;(check-for-duplication LOS-ALL)=(cons s3 (cons s4 (cons s6 (cons s7 '()))))
;;(check-for-duplication LOS-WITHOUT-DUPLICATE)=(cons s1 (cons s2 (cons s3 '())))
;;(check-for-duplication LOS-EMPTY)='()
(define (check-for-duplication los)
  (cond
    [(empty? los) empty]
    [else (if (slip-match-found? (first los) (rest los))
              (check-for-duplication (rest los)) ;discard duplicated slip and check rest
              (cons (first los) (check-for-duplication (rest los))))])) ;add slip to list
;TESTS:
(begin-for-test
  (check-equal? (check-for-duplication LOS-ALL)
                (cons s3 (cons s4 (cons s6 (cons s7 '()))))
                "All duplicate slips are removed")
  (check-equal? (check-for-duplication LOS-WITHOUT-DUPLICATE)
                (cons s1 (cons s2 (cons s3 '())))
                "The list without duplicates returns the same list")
  (check-equal? (check-for-duplication LOS-EMPTY)
                '()
                "Empty list is returned as the class-list is empty"))
;.......................................................................................
;;slip-match-found? : Slip ListOfSlip -> Boolean
;;GIVEN: a slip and a list to which it belongs
;;RETURNS: true if a match/duplicate slip is found
;;STRATEGY: Use template for ListOfSlip on los
;;EXAMPLES:
;;(slip-match-found? s1 LOS-ALL-WITHOUT-s1)=#true
;;(slip-match-found? s3 LOS-ALL-WITHOUT-s3)=#false
;;(slip-match-found? s2 LOS-EMPTY)=#false
(define (slip-match-found? s1 lst)
  (cond
    [(empty? lst) #f]
    [else (if (name-match-found? s1 (first lst))
              #t
              (slip-match-found? s1 (rest lst)))]))
;TESTS:
(begin-for-test
  (check-equal? (slip-match-found? s1 LOS-ALL-WITHOUT-s1)
                #true
                "Duplicate of s1 is present in the rest of the list")
  (check-equal? (slip-match-found? s3 LOS-ALL-WITHOUT-s3)
                #false
                "Duplicate of s3 is not present in the rest of the list")
  (check-equal? (slip-match-found? s2 LOS-EMPTY)
                #false
                "The list passed is empty and hence no match can be found"))
;.......................................................................................
;;name-match-found?: Slip Slip -> Boolean
;;GIVEN: two slips for comparison
;;RETURNS: true if both the slips are of the same student
;;STRATEGY: Use template for Slip on slip1 and slip2
;;EXAMPLES:
;;(name-match-found? s1 s2)=#false
;;(name-match-found? s1 s4)=#true
;;(name-match-found? s2 s6)=#true
(define (name-match-found? slip1 slip2)
  (or (and (string=? (slip-name1 slip1)  (slip-name1 slip2))
           (string=? (slip-name2 slip1)  (slip-name2 slip2)))
      (and (string=? (slip-name1 slip1)  (slip-name2 slip2))
           (string=? (slip-name2 slip1)  (slip-name1 slip2)))))
;TESTS:
(begin-for-test
  (check-equal? (name-match-found? s1 s2)
                #false
                "Both the slips are different")
  (check-equal? (name-match-found? s1 s4)
                #true
                "Both the slips are of the same student")
  (check-equal? (name-match-found? s2 s6)
                #true
                "Both the slips are of the same student"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
