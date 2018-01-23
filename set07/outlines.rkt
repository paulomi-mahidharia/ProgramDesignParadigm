;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;PDP Problem set 07 : Question 01;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct line (lop secdata))
;; A Line is a (make-line ListOfPosInt String)
;; INTERP: (make-line lop secdata) is a section where
;; secdata is the header text of the section
;; lop is the list of positive integers that represents the section number

;;DESTRUCTOR TEMPLATE:

;;line-fn : Line -> ??
;;(define (line-fn l)
;;  (...(line-lop l)
;;      (line-secdata l)))

;;A ListOfLine (LOL) is either
;;-- empty
;;-- (cons Line ListOfLine)

;;DESTRUCTOR TEMPLATE:
;;lol-fn : ListOfLine -> ??
;;(define (lol-fn lol)
;; (...
;;   (cond  
;;     [(empty? lol) ...]  
;;     [else (...  
;;             (line-fn (first lol))  
;;             (lol-fn (rest lol)))]))

;; A FlatRep is a ListOfLine

;;A ListOfPosInt (LOP) is either
;;-- empty                                                                         
;;-- (cons PosInt ListOfPosInt)

;;DESTRUCTOR TEMPLATE:
;;lop-fn : ListOfPosInt -> ??
;;(define (lop-fn lop)
;  (...
;;   (cond  
;;     [(number? lop) ...]  
;;     [else (...  
;;             (...(first lop))
;;             (lop-fn (rest lop)))]))


(define-struct section (name los))
;; A section is a (make-section String ListOfSection)
;; INTERP:
;; name is the header text of the section
;; los is the list of subsections of the section

;; TEMPLATE:
;; sec-fn : Section -> ??
;; (define (sec-fn s)
;;     (...
;;      (...(section-name s))
;;      (list-fn (section-los s))))

;; A ListOfSection (LOS) is one of
;; -- empty
;; -- (cons Section LOS)

;;TEMPLATE
;; list-fn : LOS -> ??
;; (define (list-fn los)
;;     (cond
;;       [(empty? los)...]
;;       [else
;;        (sec-fn (first los)
;;                (list-fn (rest los)))]))

;; An Outline is a ListOfSection

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define VALID-LIST (list
                    (make-line (list 1) "The first section")
                    (make-line (list 1 1) "A subsection with no subsections")
                    (make-line (list 1 2) "Another subsection")
                    (make-line (list 1 2 1) "This is a subsection of 1.2")
                    (make-line (list 1 2 2) "This is another subsection of 1.2")
                    (make-line (list 1 3) "The last subsection of 1")
                    (make-line (list 2) "Another section")
                    (make-line (list 2 1) "More stuff")
                    (make-line (list 2 2) "Still more stuff")))

(define INVALID-LIST (list
                      (make-line (list 1) "The first section")
                      (make-line (list 1 1) "A subsection with no subsections")
                      (make-line (list 1 2) "Another subsection")
                      (make-line (list 1 2 1) "This is a subsection of 1.2")
                      (make-line (list 1 2 2) "This is another subsection of 1.2")
                      (make-line (list 1 4) "The last subsection of 1")))

(define INIT-LOP (list 0))

(define ONE 1)
(define ZERO 0)
(define LIST '())

(define TEST
  (list 
   (make-section "The first section"
                 (list
                  (make-section "A subsection with no subsections" empty)
                  (make-section "Another subsection"
                                (list
                                 (make-section "This is a subsection of 1.2" empty)
                                 (make-section "This is another subsection of 1.2"
                                               empty)))
                  (make-section "The last subsection of 1" empty)))
   (make-section "Another section"
                 (list
                  (make-section "More stuff" empty)
                  (make-section "Still more stuff" empty)))))

(define RESULT
  (list
   (make-line (list 1) "The first section")
   (make-line (list 1 1) "A subsection with no subsections")
   (make-line (list 1 2) "Another subsection")
   (make-line (list 1 2 1) "This is a subsection of 1.2")
   (make-line (list 1 2 2) "This is another subsection of 1.2")
   (make-line (list 1 3) "The last subsection of 1")
   (make-line (list 2) "Another section")
   (make-line (list 2 1) "More stuff")
   (make-line (list 2 2) "Still more stuff")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;legal-flat-rep? : ListOfLine -> Boolean
;;GIVEN: a list of lines, like the one above
;;RETURNS: true iff it is a legal flat representation of an outline.
;;EXAMPLE:
;;(legal-flat-rep? INVALID-LIST)=#false
;;(legal-flat-rep? VALID-LIST)=#true
;;(legal-flat-rep? '())=#true
;;STRATEGY: Use template for ListOfLine on lol

(define (legal-flat-rep? lol)
  (cond
    [(empty? lol) #t]
    [else (lines-in-sequence? (first lol) (rest lol) INIT-LOP)]))

;TEST:
(begin-for-test
  (check-equal? (legal-flat-rep? INVALID-LIST)
                #false
                "The given list is an invalid flat representation")
  (check-equal? (legal-flat-rep? VALID-LIST)
                #true
                "The given list is an valid flat representation")
  (check-equal? (legal-flat-rep? '())
                #true
                "The given list is empty and hence is a valid flat representation"))
;......................................................................................
;;lines-in-sequence? : Line ListOfLine ListOfPosInt -> Boolean
;;GIVEN: a line belonging to some list, a list of lines following the given line in the
;;       original list, a list of positive integers
;;WHERE: the list of positive integers keep track of the section number of the line above 
;;       the given line in the original list
;;RETURNS: true iff the given line and the lines following it represents a valid sequence
;;         of section numbers.
;;EXAMPLES:
;;(lines-in-sequence? (make-line (list 1) "The first section")
;;                    (list (make-line (list 1 1) "A subsection with no subsections")
;;                          (make-line (list 1 2) "Another subsection")) N)
;; => #true
;;(lines-in-sequence? (make-line (list 2) "The first section")
;;                    (list (make-line (list 2 1) "A subsection with no subsections")
;;                          (make-line (list 2 2) "Another subsection")) N)
;; => #false
;;STRATEGY: Use template for ListOfLine on lol

(define (lines-in-sequence? l1 lol c-lop)
  (if (and (is-section-valid? l1 c-lop) (not (empty? lol)))
      (lines-in-sequence? (first lol) (rest lol) (line-lop l1))
      (is-section-valid? l1 c-lop)))
     
;......................................................................................
;;is-section-valid? : Line ListOfPosInt -> Boolean
;;GIVEN: a line belonging to some list and a list of positive integers
;;WHERE: the list of positive integers keep track of the section number of the line 
;;       above the given line in the original list
;;RETURNS: true if the given line has a valid section number after the line above it
;;         in the original list
;;EXAMPLES:
;;(is-section-valid? (make-line (list 1 3) "The last subsection of 1") (list 1 2 2))
;; => #true
;;(is-section-valid? (make-line (list 1 3) "The last subsection of 1") (list 1 3 1))
;; => #false
;;(is-section-valid? (make-line (list 1 3 1) "The last subsection of 1") (list 1))
;; => #false
;;STRATEGY: Divide cases based on difference of length between list of positive integers of l1
;;          and c-lop

(define (is-section-valid? l1 c-lop)
  (cond
    [(= (length (line-lop l1)) (length c-lop))
     (new-section-started? (line-lop l1) c-lop)]
    [(= (length (line-lop l1)) (+ (length c-lop) ONE))
     (is-subsection-valid? (line-lop l1) c-lop)]
    [(< (length (line-lop l1)) (length c-lop))
     (previous-section-ended? (line-lop l1) c-lop)]
    [else #f]))

;TESTS:
(begin-for-test
  (check-equal? (is-section-valid? (make-line (list 1 3) "The last subsection of 1")
                                   (list 1 2 2))
                #true
                "(list 1 3) is a valid section number after (list 1 3)")
  (check-equal? (is-section-valid? (make-line (list 1 3) "The last subsection of 1")
                                   (list 1 3 1))
                #false
                "(list 1 3) is not a valid section number after (list 1 3 1)")
  (check-equal? (is-section-valid? (make-line (list 1 3 1) "The last subsection of 1")
                                   (list 1))
                #false
                "(list 1 3 1) is not a valid section number after (list 1)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-section-started? : ListOfPosInt ListOfPosInt -> Boolean
;;GIVEN: a list of positive integers representing the section number of a line in the 
;;       original list and a list of positive integers
;;WHERE: (a) the second list of positive integers is the section number of the previous
;;           line in the original list whose section numbers are represented 
;;           by the first list of positive integers and
;;       (b) the length of both the list of positive integers is same
;;RETURNS: true if the section number represented by the first list of positive integers
;;         is a valid new section or subsection after the one represented by another
;;         list of positive integers
;;EXAMPLES:
;;(new-section-started? (list 1 2) (list 1 1))=#true
;;(new-section-started? (list 1 3) (list 1 1))=#false
;;STRATEGY: Use template for ListOfPosInt on lop

(define (new-section-started? lop c-lop)
  (if (= (first (reverse lop)) (+ (first (reverse c-lop)) ONE))
      (are-other-elements-equal? lop c-lop) 
      #f))
;TESTS:
(begin-for-test
  (check-equal? (new-section-started? (list 1 2) (list 1 1))
                #true
                "(list 1 2) is a valid new section of (list 1 1)")
  (check-equal? (new-section-started? (list 1 3) (list 1 1))
                #false
                "(list 1 3) is not a valid new section of (list 1 1)"))
;.........................................................................................
;;are-other-elements-equal? : ListOfPosInt ListOfPosInt -> Boolean
;;GIVEN: a list of positive integers representing the section number of a line in the 
;;       original list and a list of positive integers 
;;WHERE: the second a list of positive integers is the section number of the previous line
;;       in the original list
;;RETURNS: true if all the elements except the last one in both the lists of numbers
;;         are same
;;EXAMPLES:
;;(are-other-elements-equal? (list 1 1 2) (list 1 1 1))=#true
;;(are-other-elements-equal? (list 1 2 2) (list 1 1 1))=#false
;;STRATEGY: Use template for ListOfPosInt on c-lop

(define (are-other-elements-equal? lop c-lop)
  (cond
    [(empty? (rest c-lop)) #t]
    [else (and (= (first lop) (first c-lop))
               (are-other-elements-equal? (rest lop) (rest c-lop)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;is-subsection-valid? : LineOfPosInt ListOfPosInt -> Boolean
;;GIVEN: a list of positive integers representing the section number of a line in the 
;;       original list and a list of positive integers
;;WHERE: (a)the second list of positive integers keep track of the section number of the
;;          line above the line in the original list and
;;       (b)the first list of positive integers is of length more than the second list
;;          of positive integers
;;RETURNS: true if the first list of positive integers represents a valid sub section 
;;         number after the one represented by the second list of positive integers
;;EXAMPLES:
;;(is-subsection-valid? (list 1 1 1) (list 1 1))=#true
;;(is-subsection-valid? (list 1 1 2) (list 1 1))=#false
;;STRATEGY: Use template for ListOfPosInt on lop

(define (is-subsection-valid? lop c-lop)
  (cond
    [(empty? c-lop) (= (first (reverse lop)) ONE)]
    [else (and (= (first lop) (first c-lop))
               (is-subsection-valid? (rest lop) (rest c-lop)))]))

;;TESTS:
(begin-for-test
  (check-equal? (is-subsection-valid? (list 1 1 1) (list 1 1))
                #true
                "(list 1 1 1) is a valid subsection of (list 1 1)")
  (check-equal? (is-subsection-valid? (list 1 1 2) (list 1 1))
                #false
                "(list 1 1 2) is a valid subsection of (list 1 1)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;previous-section-ended? : ListOfPosInt ListOfPosInt -> Boolean
;;GIVEN: a list of positive integers representing the section number of a line in the 
;;       original list and a list of positive integers
;;WHERE: (a) the second list of positive integers keep track of the section number of the 
;;           line above the line in the original list and
;;       (b) the first list of positive integers is of length lesser than the second
;;           list of positive integers
;;RETURNS: true if the first list of positive integers represents a valid new section or
;;         sub section number after the one represented by the second list of positive
;;         integers in the original list
;;STRATEGY:
;;EXAMPLES:
;;(previous-section-ended? (list 2) (list 1 3 1))=#true
;;(previous-section-ended? (list 1 4) (list 1 3 1))=#true
;;(previous-section-ended? (list 3 4) (list 1 3 1))=#false

(define (previous-section-ended? lop c-lop)
  (cond
    [(empty? (rest lop)) (= (+ (first c-lop) ONE) (first lop))]
    [else (if (= (first lop) (first c-lop))
              (previous-section-ended? (rest lop) (rest c-lop))
              #false)]))
;;TESTS:
(begin-for-test
  (check-equal? (previous-section-ended? (list 2) (list 1 3 1))
                #true
                "(list 2) is a valid section number after (list 1 3 1)")
  (check-equal? (previous-section-ended? (list 1 4) (list 1 3 1))
                #true
                "(list 1 4) is a valid section number after (list 1 3 1)")
  (check-equal? (previous-section-ended? (list 3 4) (list 1 3 1))
                #false
                "(list 3 4) is not a valid sequence number after (list 1 3 1)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-rep-to-flat-rep : Outline -> FlatRep
;; GIVEN: the representation of an outline as a list of Sections
;; RETURNS: the flat representation of the outline
;; STRATEGY: Combine simpler functions
;; EXAMPLE: (tree-rep-to-flat-rep TEST)=RESULT

(define (tree-rep-to-flat-rep los)
  (append-sub-section los ONE LIST))

;;TESTS:
(begin-for-test
  (check-equal? (tree-rep-to-flat-rep TEST) RESULT
                "Must create a flat representation as reult"))
;........................................................................................
;; append-sub-section : ListOfSection n ListOfPosInt -> ListOfLine
;; GIVEN: a list of section, a numeric value n, list of positive integers
;; WHERE: n represents the new value of the sub section to be added to the list
;; of numbers and lop contains the list of positive integers for current subsection
;; RETURNS: A list of all sublists appended
;; EXAMPLES/TESTS: See function tree-rep-to-flat-rep
;; STRATEGY: Use template for ListOfSection on los

(define (append-sub-section los n lop)
  (cond
    [(empty? los) empty]
    [else(append
          (create-sub-section (first los) n lop)
          (append-sub-section (rest los) (+ n ONE) lop))]))

;........................................................................................
;; create-sub-section : Section n LOP -> LOL
;; GIVEN: a section, a numeric value n and a list of positive integers
;; WHERE: n represents the new value of the sub section to be added to the list
;; of numbers and lop contains the list of positive integers for current subsection
;; RETURNS: list of sublists
;; EXAMPLES/TESTS: See function tree-rep-to-flat-rep
;; STRATEGY:: Use template for line

(define (create-sub-section s n lop)
  (cons
   (make-line (reverse(cons n lop)) (section-name s))
   (append-sub-section (section-los s) ONE (cons n lop))))

