;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET05 : QUESTION 01;;;;;;;;;;;;;;;;;;;;
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
 shivers-roster
 make-slip
 slip-color
 slip-name1
 slip-name2)

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
;-- YELLOW
;-- BLUE

;;TEMPLATE:
#|(define (color-fn c)
  (cond
    [(string=? c YELLOW)...]
    [(string=? c BLUE)...]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CONTANTS FOR TESTING:

(define s1 (make-slip YELLOW "Paulomi" "Mahidharia"))
(define s2 (make-slip BLUE "Jay" "Nathani"))
(define s3 (make-slip YELLOW "Dishant" "Kapadiya"))
(define s4 (make-slip YELLOW "Mahidharia" "Paulomi"))
(define s5 (make-slip BLUE "Jahnvi" "Gandhi"))
(define s6 (make-slip BLUE "Jay" "Nathani"))
(define s7 (make-slip YELLOW "Jahnvi" "Gandhi"))
(define s8 (make-slip YELLOW "paulomi" "mahidharia"))

(define LOS-WITH-DUP-SWAPED-NAME (list s1 s2 s3 s4))
(define LOS-OF-s1-s2 (list s1 s2))
(define LOS-AFTER-ADDING-s3 (list s3 s1 s2))
(define LOS-WITH-DUP-SWAPED-NAME-ONLY-YELLOW (list s1 s3 s4))
(define LOS-WITH-DUP-NAME (list s1 s2 s5 s6))
(define LOS-EMPTY empty)
(define LOS-WITHOUT-DUPLICATE (list s1 s2 s3))
(define LOS-ONLY-FELLEISEN (list s1 s3 s4 s7))
(define LOS-ONLY-SHIVERS (list s2 s5 s6))
(define LOS-SAME-SLIP-IN-BOTH (list s2 s4 s5 s7))
(define LOS-SAME-SLIP-IN-BOTH-ONLY-YELLOW (list s4 s7))
(define LOS-ALL (list s1 s2 s3 s4 s5 s6 s7))
(define LOS-ALL-WITHOUT-s1 (list s2 s3 s4 s5 s6 s7))
(define LOS-ALL-WITHOUT-s3 (list s1 s2 s4 s5 s6 s7))
(define SLIP-WITHOUT-COLOR (make-slip "" "Vinni" "Raj"))
(define RED-SLIP (make-slip "red" "Riya" "Singh"))
(define LOWER-CASE-F (list s1 s8))
(define NEWLOS (list s1 s2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;felleisen-roster : ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;         Felleisen's class, without duplication.
;;STRATEGY: Use HOF foldr on los
;;EXMAPLES:
;;(felleisen-roster LOS-EMPTY)='()
;;(felleisen-roster LOS-WITH-DUP-SWAPED-NAME)=(cons s3 (cons s4 '()))
;;(felleisen-roster LOS-ONLY-FELLEISEN)=(cons s3 (cons s4 (cons s7 '())))
(define (felleisen-roster los)
  (foldr
   add-if-not-duplicate  ;;removes duplicated slips from the list of yellow slips
   empty
   (check-slip-color los YELLOW))) ;;returns list of yellow slips 
;;TESTS: 
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
;;check-slip-color : ListOfSlip Color -> ListOfSlip
;;GIVEN: a list of slips and a color of the slip
;;RETURNS: a list the slips of the given color
;;STRATEGY: Use HOF filter on los
;;EXAMPLES:
;;(check-for-felleisen LOS-WITH-DUP-SWAPED-NAME YELLOW)=LOS-WITH-DUP-SWAPED-NAME-ONLY-YELLOW
;;(check-for-felleisen LOS-ONLY-SHIVERS YELLOW)='()
;;(check-for-felleisen LOS-SAME-SLIP-IN-BOTH YELLOW)=LOS-SAME-SLIP-IN-BOTH-ONLY-YELLOW

(define (check-slip-color los c)
  (filter
   ;;Slip -> Boolean
   ;;GIVEN: a slip
   ;;RETURNS: true is the color of the given slip is same as specified by c
   (lambda (s) (string=? (slip-color s) c)) los))
;TESTS:
(begin-for-test
  (check-equal? (check-slip-color LOS-WITH-DUP-SWAPED-NAME YELLOW)
                LOS-WITH-DUP-SWAPED-NAME-ONLY-YELLOW
                "Slip s2 is not yellow")
  (check-equal? (check-slip-color LOS-ONLY-SHIVERS YELLOW)
                '()
                "None of the yellow slips belong to shivers")
  (check-equal? (check-slip-color LOS-SAME-SLIP-IN-BOTH YELLOW)
                LOS-SAME-SLIP-IN-BOTH-ONLY-YELLOW
                "Slip with no color does not belong to Felleisen"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shivers-roster: ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;         Shivers' class, without duplication.
;;STRATEGY: Use HOF foldr on los
;;EXAMPLES:
;;(shivers-roster LOS-EMPTY)='()
;;(shivers-roster LOS-ALL)=(cons s5 (cons s6 '()))
;;(shivers-roster LOS-ONLY-FELLEISEN)='()

(define (shivers-roster los)
  (foldr
   add-if-not-duplicate   ;;removes duplicated slips from the list of blue slips
   empty
   (check-slip-color los BLUE))) ;;returns list of blue slips

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add-if-not-duplicate : ListOfSlip ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips with or without duplicates and an empty list of slips
;;RETURNS: a list without any duplicate slips
;;STRATEGY: Use HOF ormap on los
;;EXAMPLES:
;;(add-if-not-duplicate s1 LOS-OF-s1-s2)=LOS-OF-s1-s2
;;(add-if-not-duplicate s3 LOS-OF-s1-s2)=LOS-AFTER-ADDING-s3
;;(add-if-not-duplicate s5 LOS-EMPTY)=(list s5)

(define (add-if-not-duplicate s los)
  (if (ormap
       ;;Slip -> Boolean
       ;;GIVEN: a slip
       ;;RETURNS: true if the given slip is of the same student as the slip s1
       (lambda (s1) (name-match-found? s s1)) los)
      los
      (cons s los)))
;TESTS:
(begin-for-test
  (check-equal? (add-if-not-duplicate s1 LOS-OF-s1-s2)
                LOS-OF-s1-s2
                "s1 is already a part of LOS-OF-TWO, hence it is not added")
  (check-equal? (add-if-not-duplicate s3 LOS-OF-s1-s2)
                LOS-AFTER-ADDING-s3
                "The list without duplicates returns the same list")
  (check-equal? (add-if-not-duplicate s5 LOS-EMPTY)
                (list s5)
                "Empty list is returned as the class-list is empty"))

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