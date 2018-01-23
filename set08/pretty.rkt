;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;PDP Problem set 08 : Question 01;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)
(require "extras.rkt")

(provide
 expr-to-strings
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-diff-exp NELOExpr)
;; Interpretation: a sum-exp represents a sum and a diff-exp
;; represents a difference calculation.

;;;;DESTRUCTOR TEMPLATE:

;;expr-fn : Expr -> ??
;;(define (expr-fn e)
;;  (cond
;;   [(integer? e) ...]
;;   [(sum-exp? e) (sum-exp-fn e)]
;;   [(diff-exp? e) (diff-exp-fn e)]))
;.....................................................................................
(define-struct sum-exp (exprs))

;;A Sum-exp is a (make-sum-exp NELOExpr)
;;INTERPRETATION:
;;exprs represents a non-empty list of expressions

;;DESTRUCTOR TEMPLATE:

;;sum-exp-fn : Sum-exp -> ??
;;(define (sum-exp-fn e)
;;  (...(sum-exp-exprs e)))
;......................................................................................

(define-struct diff-exp (exprs))

;;A Diff-exp is a (make-diff-exp NELOExpr)
;;INTERPRETATION:
;;exprs represents a non-empty list of expressions

;;DESTRUCTOR TEMPLATE:

;;diff-exp-fn : Diff-exp -> ??
;;(define (diff-exp-fn e)
;;  (...(diff-exp-exprs e)))
;......................................................................................

;; A LOExpr is one of
;; -- empty
;; -- (cons Expr LOExpr)

;; DESTRUCTOR TEMPLATE:
;; loe-fn : LOExpr -> ??
;; (define (loe-fn loe)
;;  (...
;;    (cond  
;;     [(empty? loe) ...]  
;;     [else (...  
;;             (expr-fn (first loe))  
;;             (loe-fn (rest loe)))]))


;; A NELOExpr is a non-empty LOExpr

;;A NELOExpr is a
;;-- (cons Expr empty)
;;-- (cons Expr NELOExpr)

;; DESTRUCTOR TEMPLATE:
;; neloe-fn : NELOExpr -> ??
;; (define (neloe-fn neloe)
;;  (...
;;    (cond  
;;     [(empty? (rest neloe) (expr-fn (first neleo))]  
;;     [else (...  
;;             (expr-fn (first neloe))  
;;             (neloe-fn (rest neloe)))]))
;......................................................................................

;;A ListOfString (LOS) is either
;;-- empty                                     
;;-- (cons String ListOfString)

;; DESTRUCTOR TEMPLATE:
;; los-fn : LOS -> ??
;; (define (los-fn los)
;;  (...
;;    (cond  
;;     [(empty? los) ...]  
;;     [else (...  
;;             (... (first los))  
;;             (los-fn (rest los)))]))


;; A NELOS is a non-empty ListOfString

;;A NELOS is a
;;-- (cons String empty)
;;-- (cons String NELOS)

;; DESTRUCTOR TEMPLATE:
;; nelos-fn : NELOS -> ??
;; (define (nelos-fn nelos)
;;  (...
;;    (cond  
;;     [(empty? (rest nelos) (... (first nelos))]  
;;     [else (...  
;;             (... (first nelos))  
;;             (nelos-fn (rest nelos)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define EXAMPLE-1
  (make-sum-exp (list 22 333 44)))

(define EXAMPLE-2
  (make-sum-exp
   (list
    (make-diff-exp (list 22 3333 44))
    (make-diff-exp
     (list
      (make-sum-exp (list 66 67 68))
      (make-diff-exp (list 42 43))))
    (make-diff-exp (list 77 88)))))

(define EXAMPLE-2-AT-WIDTH-13
  (list
   "(+ (- 22"
   "      3333"
   "      44)"
   "   (- (+ 66"
   "         67"
   "         68)"
   "      (- 42"
   "         43))"
   "   (- 77 88))"))

(define EXAMPLE-3
  (make-sum-exp
   (list
    (make-sum-exp (list 22 3333 44))
    (make-sum-exp
     (list
      (make-sum-exp (list 66 67 68))
      (make-sum-exp (list 42 43))))
    (make-sum-exp (list 77 88)))))

(define EXAMPLE-ONLY-INT 22)

(define EMP-STR "")
(define EMP-LST '())
(define SUM-OP "(+ ")
(define DIFF-OP "(- ")
(define LEVEL-OFFSET 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;expr-to-strings : Expr NonNegInt -> ListOfString
;;GIVEN: An expression and a width
;;RETURNS: A representation of the expression as a sequence of lines, with
;;each line represented as a string of length not greater than the width.
;;EXAMPLES:
;;(expr-to-strings EXAMPLE-1 4)=Not enough room
;;(expr-to-strings EXAMPLE-1 15)=(list "(+ 22 333 44)")
;;(expr-to-strings EXAMPLE-1 10)=(list "(+ 22" "   333" "   44)")
;;(expr-to-strings EXAMPLE-2 13)= EXAMPLE-2-AT-WIDTH-13
;;STRATEGY: Call a more general function

(define (expr-to-strings exp w)
  (check-expression-type exp w 1))

(begin-for-test
  (check-error (expr-to-strings EXAMPLE-1 4))
  
  (check-equal? (expr-to-strings EXAMPLE-1 15)
                (list "(+ 22 333 44)")
                "The expression fits into the width of 15 and hence is returned as list
                of a single string")
  (check-equal? (expr-to-strings EXAMPLE-1 10)
                (list "(+ 22" "   333" "   44)")
                "The expression does not fit into the width of 10 and hence is broken into 
                many strings where each string represents a new line in the stack")
  (check-equal? (expr-to-strings EXAMPLE-2 13)
                EXAMPLE-2-AT-WIDTH-13
                "The expression does not fit into the width of 13 and hence is broken into 
                many strings where each string represents a new line in the stack"))
;......................................................................................
;;check-expression-type : Expr NonNegInt PosInt -> ListOfString
;;GIVEN: an expression, a non negative integer as width and a positive integer as
;;       level
;;WHERE: the level represents the current indentation level from left to right of some
;;       sub-expression in original expression expr0 in the stack representation 
;;RETURNS: a list of strings with every string representing a part of expression in a
;;         line of the stack representation along with proper indentation
;;EXAMPLES: (check-expression-type EXAMPLE-1 10 1)=(list "(+ 22" "   333" "   44)")
;;        : (check-expression-type EXAMPLE-ONLY-INT 5 1)=(list "22")
;;STRATEGY: Use template for type of expression

(define (check-expression-type exp w level)
  (cond
    [(integer? exp) (check-integer-expr exp w level)]    
    [(sum-exp? exp) (check-sum-and-diff-expr exp w level SUM-OP sum-exp-exprs)] 
    [(diff-exp? exp) (check-sum-and-diff-expr exp w level DIFF-OP diff-exp-exprs)]))

;;TESTS:
(begin-for-test
  (check-equal? (check-expression-type EXAMPLE-1 10 1)
                (list "(+ 22" "   333" "   44)")
                "")
  (check-equal? (check-expression-type EXAMPLE-ONLY-INT 5 1)
                (list "22")
                ""))
;......................................................................................
;;check-integer-expr : Expr NonNegInt PosInt -> NELOExpr
;;GIVEN: an expression, a non negative integer as width, a positive integer as indentation
;;       level
;;WHERE: the level represents the current indentation level from left to right of some
;;       sub-expression in original expression expr0 in the stack representation
;;RETURNS: a list of strings with every string representing a part of expression in a
;;         line of the stack representation along with proper indentation
;;EXAMPLES: (check-integer-expr 22 5 1)=(list "22")
;;        : (check-integer-expr 22 5 6)=Not enough room
;;STRATEGY: Divide cases based on whether the string of expression fits into the
;;          given width
(define (check-integer-expr exp w level)
  (local
       ((define expr-str (number->string exp))
        (define expr-str-len (string-length expr-str)))
       (if (fits-into-width? expr-str-len w level)
           (list expr-str)
           (error "Not enough room"))))
;......................................................................................
;;check-sum-and-diff-expr : Expr NonNegInt PosInt String (Expr -> NELOExpr) ->
;;                        : NELOExpr
;;GIVEN: an expression, a non negative integer as width, a positive integer as indentation
;;       level, a string as an operator and a function that takes the given expression
;;       and returns its non-empty list of expressions
;;WHERE: (a) the given expression is either a sum expression or a difference expression
;;       (b) the level represents the current indentation level from left to right of some
;;           sub-expression in original expression expr0 in the stack representation 
;;RETURNS: a list of strings with every string representing a part of expression in a
;;         line of the stack representation along with proper indentation
;;EXAMPLES: (check-sum-and-diff-expr EXAMPLE-1 10 1 "(+ " sum-exp-exprs)
;;          => (list "(+ 22" "   333" "   44)")
;;STRATEGY: Divide cases based on whether the string of expression fits into the
;;          given width

(define (check-sum-and-diff-expr expr w level op f)
  (local
    ((define expr-str (string-append op (get-next-expr (f expr) w level EMP-STR)))
     (define expr-str-len (string-length expr-str)))
    (if (fits-into-width? expr-str-len w level)
        (list expr-str)
        (add-closing-paran w (break-expression w (+ level LEVEL-OFFSET) op (f expr))))))
;......................................................................................
;;get-next-expr : NELOExpr NonNegInt PosInt String -> String
;;GIVEN: a non-empty list of expressions, a non negative integer as width, a positive 
;;       integer as indentation level, and a string
;;WHERE: (a) the level represents the current indentation level from left to right of some
;;           sub-expression in original expression expr0 in the stack representation 
;;       (b) the string represents the string built so far from the elements of non-empty 
;;           list of expressions 
;;RETURNS: a string containing elements from the given non-empty list of expressions that 
;;         could fit in one line of the given width
;;EXAMPLES: (get-next-expr (list 4 5) 10 1 EMP-STR)="4 5)"
;;        : (get-next-expr (list (make-sum-exp (list 4 5))) 10 1 EMP-STR)="(+ 4 5))"             
;;STRATEGY: Recur on check-expression-type
;;HALTING MEASURE: Length of non-empty list of expression neloe
;;TERMINATION CONDITION: If the (rest neleo) is empty i.e we are processing
;;                       the last element of the list then recur on (first neleo) and put
;;                       a closing paranthesis, else call (rest neleo) and recur on its
;;                      (first neleo).Since everytime the non-empty list of expressions is
;;                       reduced until the last expressions is checked, the halting 
;;                       measure is reduced.

(define (get-next-expr neloe w level str)
  (cond
    [(empty? (rest neloe))
     (string-append str (first (check-expression-type (first neloe) w level)) ")")]
    [else (get-next-expr  (rest neloe) w level 
                          (string-append
                           str
                           (first (check-expression-type (first neloe) w level)) " "))]))

;.....................................................................................
;;break-expression : NonNegInt PosInt String NELOExp -> NELOS
;;GIVEN: a non negative integer as width, a positive integer as indentation level, a
;;       string defining the operator and a non-empty list of expressions
;;WHERE:  the level represents the current indentation level from left to right of some
;;        sub-expression in original expression expr0 in the stack representation 
;;RETURNS: a non empty list of strings in which each string represnts a line to be
;;         printed in stack fashion as output
;;EXAMPLES: (break-expression (make-sum-exp (list 22 3333 44)) 5 1 "(+ " sum-exp-exprs)
;;          => (list "(+ 22" "   3333" "   44")
;;STRATEGY: Recur on check-expression-type
;;HALTING MEASURE: Length of non-empty list of expression neloe
;;TERMINATION CONDITION:
;;The output is a list obtained by appending three lists. First list consists the first
;;element obtained by recuring on (first neloe). Second is obtained by calling rest of the
;;element obtained by recuring on (first neloe) and last is obtained by recurring on
;;(rest neloe). At every call the list obtained will be decreased by length of one and the 
;;list is updated. Since everytime the non-empty list of expressions is reduced until the
;;last expressions is checked , the halting measure is reduced.

(define (break-expression w level op neloe)
  (append
   (list (string-append op (first (check-expression-type (first neloe) w level))))
   (list-after-levels (rest (check-expression-type (first neloe) w level)))
   (break-rest-expression (rest neloe) w level EMP-LST)))

;.....................................................................................
;;break-rest-expression : NELOExpr NonNegInt PosInt LOExpr -> NELOS
;;GIVEN: a non empty list of expressions, a non negative integer as width, a positive
;;       integer as indentation level, a list of expressions
;;WHERE: (a) the level represents the current indentation level from left to right of some
;;           sub-expression in original expression expr0 in the stack representation 
;;       (b) the list represents the list of expressions built so far after appending
;;           spaces to the string of expression in the original output list of strings
;;RETURNS: a non empty list of strings in which each string represnts a line to be
;;         printed in stack as output along with proper indentation
;;EXAMPLES: (break-rest-expression (list (make-sum-exp (list 22 3333 44))) 10 1 '())
;;          => (list "   (+ 22" "      3333" "      44)")
;;STRATEGY: Recur on check-expression-type
;;HALTING MEASURE: Length of non-empty list of expression neloe
;;TERMINATION CONDITION: If the (rest neloe) is empty i.e we are processing
;;                       the last element of the list then recur on (first neleo)
;;                       else call (rest neleo) and recur on its (first neleo).
;;                       Since everytime the non-empty list of expressions is
;;                       reduced until the last expressions is checked, the halting 
;;                       measure is reduced.

(define (break-rest-expression neloe w level lst)
  (cond
    [(empty? (rest neloe))
     (append lst (list-after-levels (check-expression-type (first neloe) w level)))]
    [else
     (append (list-after-levels (check-expression-type (first neloe) w level))
             (break-rest-expression (rest neloe) w level lst))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;fits-into-width? : PosInt NonNegInt NonNegInt -> Boolean
;;GIVEN: a positive interger as length of expression string, a non negative integer as
;;       width and a non negative integer as level
;;WHERE: the level represents the current indentation level from left to right of some
;;       sub-expression in original expression expr0 in the stack representation 
;;RETURNS: ture if the expression string along with its appended spaces as per the given  
;;         level fits into the given width
;;EXAMPLES: (fits-into-width? 5 10 6)=#false
;;        : (fits-into-width? 4 10 5)=#true
;;STRATEGY: Call simpler functions

(define (fits-into-width? expr-str-len w level)
  (<= (+ level expr-str-len) w))
;.....................................................................................
;;list-after-levels : ListOfString -> ListOfString
;;GIVEN: a list of strings of expression
;;WHERE: each string represents one line to be printed in the spack representation
;;RETURNS: the given list of strings but with appropriate spaces appeneded to them for
;;       : proper indentation in the stack
;;EXAMPLES: (list-after-levels (list "(+ 22" "   5)"))=(list "   (+ 22" "      5)")
;;STRATEGY: Use HOF map on lst

(define (list-after-levels lst)
  (map
   ;;String -> String
   ;;GIVEN: a string containing sub-expression from original expression expr0
   ;;RETURNS: the given string with appropriate spaces appended before it    
   (lambda (l) (string-append (make-string LEVEL-OFFSET #\space) l)) lst))
;.....................................................................................
;;add-closing-paran : NonNegInt NELOS -> NELOS
;;GIVEN: a non negative integer as width and a list of strings of expressions
;;RETURNS: The given list of expressions with a closing paranthesis added to the last
;;       : string in the list
;;EXAMPLES: (add-closing-paran 18 (list "(+ (- 22 3333 44)"
;;                                      "   (- (+ 66 67 68)"
;;                                      "      (- 42 43))"
;;                                      "   (- 77 88)"))
;;          => (list "(+ (- 22 3333 44)"
;;                   "   (- (+ 66 67 68)"
;;                   "      (- 42 43))"
;;                   "   (- 77 88))")
;;STRATEGY: Use template for NELOS on expr-str

(define (add-closing-paran w expr-str)
  (cond
    [(empty? (rest expr-str)) (final-check w (list (string-append (first expr-str) ")")))]
    [else (final-check w (append (list (first expr-str))
                  (add-closing-paran w (rest expr-str))))]))
;.....................................................................................
;;final-check : NonNegInt ListOfString -> Boolean
;;GIVEN: a non negative integer as width and a list of strings as a list of
;;       expression string
;;WHERE: the list of expression string contains a completed expression string to be 
;;       printed in one line of the stack representation
;;RETURNS: true if the expression string in the list fits into the given width
;;EXAMPLES: (final-check 5 (list "    22"))=Not enough room
;;        : (final-check 6 (list "    22"))=(list "    22")
;;STRATEGY: Divide into cases based on whether each string in given list fits into
;;          the width
(define (final-check w lst)
  (if (andmap
       ;;ListOfString -> ListOfString
       ;;GIVEN: a list of strings
       ;;RETURNS: the given list of strings if every string fits into the given width 
       (lambda (l) (<= (string-length l) w)) lst)
      lst
      (error "Not enough room")))

;;TESTS:
(begin-for-test
  (check-error (final-check 5 (list "    22"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
