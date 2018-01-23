;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET05 : QUESTION 03;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Given are a list of (student, class) pairs called an enrollment.
;;The program produces the class roster for each class that has at least one student
;;enrolled.
;;The program can check whether two rosters are equal i.e have same members.
;;The program also checks whther two sets of rosters are equal.
;;For a given set of enrollments, the program produces a set of class rosters that has a
;;particular class and the corresponding set of students in each class.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 roster=?
 rosterset=?
 enrollments-to-rosters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;A SetOfX is a list of X's without duplication. Two SetOfX's are
;;considered equal if they have the same members.

;;Example: (list (list 1 2) (list 2 1)) is NOT a SetOfSetOfNumber,
;;because (list 1 2) and (list 2 1) represent the same set of numbers.

;; SetofX

;; A SetofX (SOX) is either
;; -- empty
;; -- (cons X SOX)

;; setx-fn : SetofX -> ??
;; (define (setx-fn setx)
;;   (cond
;;     [(empty? setx) ...]
;;     [else (...
;;             (... (first setx))
;;             (setx-fn (rest setx)))]))

(define-struct enrollment (student class))
;;An Enrollment is a (make-enrollment Student Class)
;;(make-enrollment s c) represents the assertion that student s is
;;enrolled in class c.

;;Student is unspecified, but you may assume that students may be
;;compared for equality with equal?

;;Class is unspecified, but you may assume that classes may be
;;compared for equality with equal?

;; template:
;; enrollment-fn : Enrollment -> ??
;;(define (enrollment-fn e)
;;  (... (enrollment-student e)
;;       (enrollment-class e)))

;; SetofEnrollment
;; where the SetofEnrollment has no duplicate members

;; A SetofEnrollment (SOE) is either  
;; -- empty  
;; -- (cons Enrollment SOE)

;;soe-fn : SetofEnrollment -> ??  
;; (define (soe-fn soe)  
;;  (cond  
;;     [(empty? soe) ...]  
;;     [else (...  
;;             (enrollment-fn (first soe))  
;;             (soe-fn (rest soe)))]))  



(define-struct roster (classname students))
;;A ClassRoster is a (make-roster Class SetOfStudent)
;;(make-roster c ss) represents that the students in class c are exactly
;;the students in set ss.

;;Two ClassRosters are equal if they have the same class and equal
;;sets of students.

;;In the output, the classes may be in any order, and the students in
;;each class may be in any order.

;; template:
;; roster-fn : ClassRoster -> ??
;(define (roster-fn r)
;  (... (roster-classname r)
;       (roster-students r)))

;;SetofClassRoster
;;where the SetofClassRoster has no duplicate members

;;A SetofClassRoster (SOR) is either  
;; -- empty  
;; -- (cons Roster SOR)  
  
;; sor-fn : SetofClassRoster -> ??  
;; (define (sor-fn sor)  
;;   (cond  
;;     [(empty? sor) ...]  
;;     [else (...  
;;             (roster-fn (first sor))  
;;             (sor-fn (rest sor)))]))  


;;A Student is a String
;;where the String specifies the name of the Student

;;SetofStudent
;;where the SetofStudent has no duplicate members

;;A SetofStudent (SOS) is either  
;; -- empty  
;; -- (cons Student SOS)

;; sos-fn : SetofStudent -> ??  
;; (define (sos-fn sos)  
;;   (cond  
;;     [(empty? sos) ...]  
;;     [else (...  
;;             (... (first sos))  
;;             (sos-fn (rest sos)))]))

;;A Class is a String
;;where the String specifies the name of the Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Class constants
(define CLASS1 "PDP")
(define CLASS2 "DBMS")
(define CLASS4 "English")
(define CLASS5 "Math")
(define CLASS6 "Music")

;Student constants
(define STUD1 "Amit")
(define STUD2 "Ben")
(define STUD3 "Chen")
(define STUD4 "Rozy")

;Set of Students
(define SOS1 (list STUD1 STUD2 STUD3))
(define SOS2 (list STUD1 STUD3))
(define SOS3 (list STUD1 STUD3 STUD2))

;Enrollments
(define ENR1 (make-enrollment STUD1 CLASS4))
(define ENR2 (make-enrollment STUD1 CLASS6))
(define ENR3 (make-enrollment STUD2 CLASS5))
(define ENR4 (make-enrollment STUD3 CLASS4))
(define ENR5 (make-enrollment STUD4 CLASS1))

;Set of Enrollments
(define SETENR1 (list ENR1 ENR2 ENR3 ENR4))
(define SETENR2 (list (make-enrollment "John" "PDP")
                      (make-enrollment "Kathryn" "Networks")
                      (make-enrollment "Feng" "PDP")
                      (make-enrollment "Amy" "PDP")
                      (make-enrollment "Amy" "Networks")))

;;Class rosters
(define CLASSROSTER1 (make-roster CLASS1 SOS1))
(define CLASSROSTER2 (make-roster CLASS1 SOS2))
(define CLASSROSTER3 (make-roster CLASS2 SOS1))
(define CLASSROSTER4 (make-roster CLASS1 SOS3))
(define CLASSROSTER5 (make-roster CLASS6 (list STUD1)))
(define CLASSROSTER6 (make-roster CLASS5 (list STUD2)))
(define CLASSROSTER7 (make-roster CLASS4 (list STUD1 STUD3)))
(define CLASSROSTER8 (make-roster CLASS5 (list STUD3 STUD1)))

;;Set of Class Rosters
(define SETROS1 (list CLASSROSTER1 CLASSROSTER2))
(define SETROS3 (list CLASSROSTER2 CLASSROSTER1))
(define SETROS2 (list CLASSROSTER2 CLASSROSTER3))
(define SETROS4 (list CLASSROSTER1 CLASSROSTER2 CLASSROSTER3))
(define SETROS5 (list CLASSROSTER1 CLASSROSTER7))
(define SETROS6 (list CLASSROSTER3 CLASSROSTER2 CLASSROSTER1))
(define SETENR1-TO-SETROS (list CLASSROSTER5 CLASSROSTER6 CLASSROSTER7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;roster=? : ClassRoster ClassRoster -> Boolean
;;GIVEN: two class rosters
;;RETURNS: true iff the two arguments represent the same roster
;;STRATEGY: Use template for ClassRoster on r1 and r2/Call a more genral function
;;EXAMPLES:
;;(roster=? CLASSROSTER3 CLASSROSTER2)=#false
;;(roster=? CLASSROSTER1 CLASSROSTER2)=#false
;;(roster=? CLASSROSTER1 CLASSROSTER4)=#true

(define (roster=? r1 r2)
  (if (equal? (roster-classname r1) (roster-classname r2))
      (set-equal? (roster-students r1) (roster-students r2) is-student-present?)
      #f))

;TESTS:
(begin-for-test
  (check equal? (roster=? CLASSROSTER3 CLASSROSTER2)
         #false
         "The two rosters neither have same class or same set of students")
  (check equal? (roster=? CLASSROSTER1 CLASSROSTER2)
         #false
         "The two rosters have same class but don't have same set of students")
  (check equal? (roster=? CLASSROSTER1 CLASSROSTER4)
         #true
         "Both the rosters have same class and same set of students"))
;......................................................................
;;set-equal? : SetOfX SetOfX (X SetOfX -> Boolean) -> Boolean
;;GIVEN: two set of Xs and a function that decides whether X is a part of set of X
;;WHERE X can be a Student or ClassRoster
;;RETURNS: true iff both the set of Xs are equal i.e have same members
;;STRATEGY: Combine simpler functions
;;EXAMPLES:
;;(set-equal? SOS1 SOS2 is-student-present?)=#false
;;(set-equal? SOS3 SOS1 is-student-present?)=#true
;;(set-equal? SETROS4 SETROS6 is-roster-present?)=#true
;;(set-equal? SETROS4 SETROS5 is-roster-present?)=#false

(define (set-equal? set1 set2 f)
  (and
   (subset? set1 set2 f)
   (subset? set2 set1 f)))

;TESTS:
(begin-for-test
  (check equal? (set-equal? SOS1 SOS2 is-student-present?)
         #false
         "Both the set of students are not equal")
  (check equal? (set-equal? SOS3 SOS1 is-student-present?)
         #true
         "Both the set of students are equal i.e have same members")
  (check equal? (set-equal? SETROS4 SETROS6 is-roster-present?)
         #true
         "Both the set of class rosters are equal i.e have same members")
  (check equal? (set-equal? SETROS4 SETROS5 is-roster-present?)
         #false
         "Both the set of class rosters are not equal"))
;.......................................................................
;;subset? : SetOfX SetOfX (X SetOfX -> Boolean) -> Boolean
;;GIVEN: two set of Xs and a function that decides whether X is a part of set of X
;;WHERE  X can be a Student or ClassRoster
;;RETURNS: true if every member of the first set is present in the second set
;;STRATEGY: Use HOF andmap on set1
;;EXAMPLES:
;;(subset? SETROS1 SETROS4 is-roster-present?)=#true
;;(subset? SETROS4 SETROS1 is-roster-present?)=#false

(define (subset? set1 set2 f)  
  (andmap
   ;;X -> Boolean
   ;;GIVEN: a member X of some SetOfX
   ;;WHERE X can be a student or a class roster
   ;;RETURNS: true if X is present in set2
   (lambda (s1) (f s1 set2)) set1))

;;TESTS:
(begin-for-test
  (check equal? (subset? SETROS1 SETROS4 is-roster-present?)
         #true
         "SETROS1 is a subset of SETROS4")
  (check equal? (subset? SETROS4 SETROS1 is-roster-present?)
         #false
         "SETROS4 is not a subset of SETROS1"))
;........................................................................
;;is-student-present? : Student SetOfStudent -> Boolean
;;GIVEN: a student and a set of students
;;WHERE there are no duplicate members within a set of students
;;RETURNS: true if the given student is present in the given set of students
;;STRATEGY: Use HOF ormap on set2
;;EXAMPLES:
;;(is-student-present? STUD1 SOS1)=#true
;;(is-student-present? STUD4 SOS1)=#false

(define (is-student-present? s1 set2)
  (ormap
   ;;Student -> Boolean
   ;;GIVEN: a student
   ;;RETURNS: true if the student is present in set2
   (lambda (s2) (equal? s1 s2)) set2))

;TESTS:
(begin-for-test
  (check equal? (is-student-present? STUD1 SOS1)
         #true
         "The student STUD1 is present in the given set of students SOS1")
  (check equal? (is-student-present? STUD4 SOS1)
         #false
         "The student STUD4 is not present in the given set of students SOS1"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;;GIVEN: two set of class rosters
;;WHERE there are no duplicate members within a set of class roster
;;RETURNS: true iff the two arguments represent the same set of class rosters
;;STRATEGY: Call a more general function
;;EXAMPLES:
;(rosterset=? SETROS1 SETROS2)
;(rosterset=? SETROS3 SETROS2)
;(rosterset=? SETROS1 SETROS3)

(define (rosterset=? setr1 setr2)
  (set-equal? setr1 setr2 is-roster-present?))

;TESTS:
(begin-for-test
  (check equal? (rosterset=? SETROS1 SETROS2)
         #false
         "SETROS1 and SETROS2 does not represent the same set of class rosters")
  (check equal? (rosterset=? SETROS3 SETROS2)
         #false
         "SETROS3 and SETROS2 does not represent the same set of class rosters")
  (check equal? (rosterset=? SETROS1 SETROS3)
         #true
         "SETROS1 and SETROS3 represents the same set of class rosters"))

;.....................................................................
;;is-roster-present? : ClassRoster SetOfClassRoster -> Boolean
;;GIVEN: a class roster and set of class rosters
;;WHERE there are no duplicate members within a set of class rosters
;;RETURNS: true iff the given class roster is present in the set of class rosters
;;STRATEGY: Use HOF ormap on setr2
;;EXAMPLES:
;;(is-roster-present? CLASSROSTER1 SETROS3)=#true
;;(is-roster-present? CLASSROSTER4 SETROS2)=#false

(define (is-roster-present? r1 setr2)
  (ormap
   ;;ClassRoster -> Boolean
   ;;GIVEN: a class roster
   ;;RETURNS: true if the given class roster equals r1
   (lambda (r2) (roster=? r1 r2)) setr2))

;TESTS:
(begin-for-test
  (check equal? (is-roster-present? CLASSROSTER1 SETROS3)
                #true
                "CLASSROSTER1 is present in SETROS3")
  (check equal? (is-roster-present? CLASSROSTER4 SETROS2)
                #false
                "CLASSROSTER4 is not present in SETROS2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;enrollments-to-rosters: SetOfEnrollment -> SetOfClassRoster
;;GIVEN: a set of enrollments
;;RETURNS: the set of class rosters for the given enrollments
;;STRATEGY: Use HOF foldr on soe
;;EXAMPLE:
;;(enrollments-to-rosters
;;  (list (make-enrollment "John" "PDP")
;;        (make-enrollment "Kathryn" "Networks")
;;        (make-enrollment "Feng" "PDP")
;;        (make-enrollment "Amy" "PDP")
;;        (make-enrollment "Amy" "Networks")))
;;=>
;; (list
;;   (make-roster "PDP" (list "John" "Feng" "Amy"))
;;   (make-roster "Networks" (list "Kathryn" "Amy")))
;;
;;(enrollments-to-rosters SETENR) = SETENR-TO-SETROS

(define (enrollments-to-rosters soe)
  (foldr
   update-roster
   empty
   soe))

;TESTS:
(begin-for-test
  (check rosterset=? (enrollments-to-rosters SETENR2)
                     (list (make-roster "PDP" (list "John" "Feng" "Amy"))
                           (make-roster "Networks" (list "Kathryn" "Amy")))
                     "Returns the set of roster for the given enrollments")
  (check rosterset=? (enrollments-to-rosters SETENR1)
                     SETENR1-TO-SETROS
                     "Returns the set of roster for the given enrollments"))
;...........................................................................
;;update-roster : Enrollment SetOfClassRoster -> SetOfClassRoster
;;GIVEN: an enrollment and a set of class rosters
;;RETURNS: a set of class rosters with the given enrollment added to it
;;STRATEGY: Use HOF ormap on setr
;;EXAMPLE:
;;(update-roster ENR5 SETROS5)=(list (make-roster CLASS1 (list STUD4 STUD1 STUD2 STUD3))
;;                                   (make-roster CLASS4 (list STUD1 STUD3)))
;;(update-roster ENR3 SETROS5)=(list (make-roster CLASS5 (list STUD2))
;;                                   (make-roster CLASS1 (list STUD1 STUD2 STUD3))
;;                                   (make-roster CLASS4 (list STUD1 STUD3)))

(define (update-roster e setr)
  (if (ormap
       ;;ClassRoster -> Boolean
       ;;GIVEN: a class roster
       ;;RETURNS: true if the given class roster is of the same class as that
       ;;         specified in enrollment e
       (lambda (r) (class-match-found? r e)) setr)
      (add-student-to-roster e setr)
      (cons (make-new-roster e) setr)))

;TEST:
(begin-for-test
  (check rosterset=? (update-roster ENR5 SETROS5)
                     (list (make-roster CLASS1 (list STUD4 STUD1 STUD2 STUD3))
                           (make-roster CLASS4 (list STUD1 STUD3)))
                     "The given enrollment has a matching class in rosterset to which
                     it is added")
  (check rosterset=? (update-roster ENR3 SETROS5)
                     (list (make-roster CLASS5 (list STUD2))
                           (make-roster CLASS1 (list STUD1 STUD2 STUD3))
                           (make-roster CLASS4 (list STUD1 STUD3)))
                     "A new roster is added to the set of rosters as no matching class
                     is found for the given enrollment"))
;...........................................................................
;;add-student-to-roster : Enrollment SetOfClassRoster -> SetOfClassRoster
;;GIVEN: an enrollment and a set of class rosters to which the given enrollment
;;       is to be added
;;RETURNS: the set of class rosters for the given enrollments 
;;STRATEGY: Use HOF map on setr
;;EXAMPLES:
;;(add-student-to-roster ENR5 SETROS5)=(list (make-roster CLASS1 (list STUD4 STUD1 STUD2 STUD3))
;;                                           (make-roster CLASS4 (list STUD1 STUD3)))
;;(add-student-to-roster ENR4 SETROS1)=(list (make-roster CLASS1 (list STUD1 STUD2 STUD3))
;;                                           (make-roster CLASS1 (list STUD1 STUD3)))
(define (add-student-to-roster e setr)
  (map
   ;;ClassRoster -> ClassRoster
   ;;GIVEN: a class roster
   ;;RETURNS: a class roster with either the enrollment added to it or
   ;;         the the class roster as it is given
   (lambda (r) (check-student-class-to-add r e))
       setr))

;TESTS:
(begin-for-test
  (check rosterset=? (add-student-to-roster ENR5 SETROS5)
         (list (make-roster CLASS1 (list STUD4 STUD1 STUD2 STUD3))
               (make-roster CLASS4 (list STUD1 STUD3)))
         "Since a matching class is found in the roster, the enrollment is
                      added to it")
  (check rosterset=? (add-student-to-roster ENR4 SETROS1)
         (list (make-roster CLASS1 (list STUD1 STUD2 STUD3))
               (make-roster CLASS1 (list STUD1 STUD3)))
         "No matching class is found, so enrollment is not added"))

;..........................................................................
;;check-student-class-to-add : ClassRoster Enrollment -> ClassRoster
;;GIVEN: a class roster and an enrollment that is to be added to the roster
;;RETURNS: a class roster with either the given enrollment added in it or
;;         the class roster as it is
;;STRATEGY: Combine simpler functions
;;EXAMPLES:
;;(check-student-class-to-add CLASSROSTER1 ENR5)
;;     =(make-roster CLASS1 (list STUD4 STUD1 STUD2 STUD3))
;;(check-student-class-to-add CLASSROSTER1 ENR2)
;;     =(make-roster CLASS1 (list STUD1 STUD2 STUD3))

(define (check-student-class-to-add r e)
  (if (class-match-found? r e)
      (add-student-to-this-class r e)
      r))

;TESTS:
(begin-for-test
  (check roster=? (check-student-class-to-add CLASSROSTER1 ENR5)
                  (make-roster CLASS1 (list STUD4 STUD1 STUD2 STUD3))
                  "The given enrollment is added to the roster as both represent the same
                   class")
  (check roster=? (check-student-class-to-add CLASSROSTER1 ENR2)
                  (make-roster CLASS1 (list STUD1 STUD2 STUD3))
                  "The given enrollment is not added to the roster as the enrollment 
                  belongs to a different class than the roster"))
;...........................................................................
;;add-student-to-this-class : ClassRoster Enrollment -> ClassRoster
;;GIVEN: a class roster and an enrollment that is to be added to the given roster
;;RETURNS: the class roster with the record of student in the given enrollment added to it
;;STRATEGY: Use template for Enrollment and ClassRoster on enr and cr respectively
;;EXAMPLE:
;;(add-student-to-this-class CLASSROSTER1 ENR5)
;;=(make-roster CLASS1 (list STUD1 STUD2 STUD3 STUD4))

(define (add-student-to-this-class cr enr)
  (make-roster
   (roster-classname cr)
   (cons (enrollment-student enr) (roster-students cr))))

;;TEST:
(begin-for-test
  (check roster=? (add-student-to-this-class CLASSROSTER1 ENR5)
         (make-roster CLASS1 (list STUD1 STUD2 STUD3 STUD4))
         "Adds the enrollment to the roster"))
;...........................................................................
;;make-new-roster :Enrollment -> ClassRoster
;;GIVEN: an enrollment 
;;RETURNS: a class roster representing the same information as is present
;;         in the given enrollment
;;STRATEGY: Use template for Enrollment on enr
;;EXAMPLE:
;;(make-new-roster ENR1)=(make-roster CLASS4 (list STUD1))

(define (make-new-roster enr)
  (make-roster
   (enrollment-class enr)
   (cons (enrollment-student enr) empty)))

;TEST:
(begin-for-test
  (check roster=? (make-new-roster ENR1)
         (make-roster CLASS4 (list STUD1))
         "A new roster is made for the given enrollment"))
;...........................................................................
;;class-match-found? :ClassRoster Enrollment -> Boolean
;;GIVEN: a class roster and an enrollment
;;RETURNS:true if the class name of the roster and the enrollment are same
;;STRATEGY: Use template for Enrollment and ClassRoster on enr and cr respectively
;;EXAMPLES:
;;(class-match-found?  CLASSROSTER1 ENR5)=#true
;;(class-match-found?  CLASSROSTER1 ENR2)=#false

(define (class-match-found? cr enr)
  (equal? (roster-classname cr) (enrollment-class enr)))

;TESTS:
(begin-for-test
  (check equal? (class-match-found?  CLASSROSTER1 ENR5)
         #true
         "CLASSROSTER1 and ENR1 both have PDP class")
  (check equal? (class-match-found?  CLASSROSTER1 ENR2)
         #false
         "CLASSROSTER1 has PDP class but ENR2 has Music class"))
