;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 02 - QUESTION 01;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;editor.rkt;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-location "02" "editor.rkt")

(require rackunit)
(require "extras.rkt")

(provide
  make-editor
  editor-pre
  editor-post
  editor?
  edit
  )

;;CONSTANTS:

(define NULL "")
(define L "left")
(define R "right")

;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct editor [pre post])

; An Editor is (make-editor String String) 
; INTERPRETATION:
; (make-editor s t) means the text in the editor is 
; (string-append s t) with the cursor is displayed between s and t

;; editor-fn : Editor -> ??
#|                   
(define (editor-fn ed)
  (...
    (editor-pre ed)
    (editor-post ed)
   )
)
|#

;; EXAMPLE:
;; (edit (make-editor "Welcome" "Eddy") L) = (make-editor "Welcom" "eEddy")
;; (edit (make-editor "" "Eddy") L) = (make-editor "" "Eddy")
;; (edit (make-editor "Welcome" "Eddy") R) = (make-editor "WelcomeE" "ddy")
;; (edit (make-editor "Welcome" "") R) = (make-editor "Welcome" "")
;; (edit (make-editor "Welcome" "Eddy") "\b") = (make-editor "Welcom" "Eddy")
;; (edit (make-editor "" "Eddy") "\b") = (make-editor "" "Eddy")
;; (edit (make-editor "Welcome" "Eddy") "\r") = (make-editor "Welcome" "Eddy")
;; (edit (make-editor "Welcome" "Eddy") "\t") = (make-editor "Welcome" "Eddy")
;; (edit (make-editor "Welcome" "Eddy") "\u007F") = (make-editor "Welcome" "Eddy")
;; (edit (make-editor "Welcome" "Eddy") "a") = (make-editor "Welcomea" "Eddy")
;; (edit (make-editor "" "Eddy") "%") = (make-editor "%" "Eddy")
;; (edit (make-editor "Welcome" "Eddy") " ") = (make-editor "Welcome " "Eddy")
;; (edit (make-editor "Welcome" "Eddy") "") = (make-editor "Welcome " "Eddy")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; edit : Editor String -> Editor
;; GIVEN: an Editor and the key-event (in String)
;; RETURNS: an updated Editor based on the "key" passed.
;; STRATEGY: Use template for Editor on ed

(define (edit ed ke)
  (cond 
    [(= (string-length ke) 1) (single-key ed ke)] 
    [(string=? L ke) (move-cursor-left ed)] 
    [(string=? R ke) (move-cursor-right ed)] 
    [else ed] 
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; single-key : Editor String -> Editor
;; GIVEN: an Editor and a single letter key-event (as String)
;; RETURNS: An updated editor based on the key-event passed
;; STRATEGY: Cases on KeyEvent
;; EXAMPLE:
;;(single-key (make-editor "Hello" "Jenny") "\b")= (make-editor "Hell" "Jenny")
;;(single-key (make-editor "Hello" "Jenny") "\t") = (make-editor "Hello" "Jenny")
;;(single-key (make-editor "Hello" "Jenny") "\r") = (make-editor "Hello" "Jenny")
;;(single-key (make-editor "Hello" "Jenny") "\u007F") = (make-editor "Hello" "Jenny")
;;(single-key (make-editor "Hello" "Jenny") "y") = (make-editor "Helloy" "Jenny")

(define (single-key ed ke)
  (cond
    [(key=? "\b" ke) (remove-left ed)]
    [(key=? "\t" ke) ed]
    [(key=? "\r" ke) ed]
    [(key=? "\u007F" ke) ed]
    [else (insert ed ke)]  
  )
)

;; remove-left : Editor -> Editor
;; GIVEN:  an editor 
;; RETURNS: returns an editor where one character immediately to the left of the cursor
;;          has been deleted if present or returns the given editor
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;;(remove-left (make-editor "Hello" "Jenny"))=(make-editor "Hell" "Jenny")
;;(remove-left (make-editor "" "Jenny"))=(make-editor "" "Jenny")

(define (remove-left ed)
  (cond
    [(string=? (editor-pre ed) NULL) ed]
    [else (execute-backspace ed)])
)
 
;; execute-backspace : Editor -> Editor
;; GIVEN: an editor
;; RETURNS: an editor where one character immediately to the left of the cursor
;;          is deleted
;; STRATEGY: Use template for editor on ed
;; EXAMPLE:
;; (execute-backspace (make-editor "Hello" "Jenny"))=(make-editor "Hell" "Jenny")

(define (execute-backspace ed)
  (make-editor
   (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1))
   (editor-post ed))
)

;; insert : Editor 1String -> Editor
;; GIVEN: an editor and a 1String
;; RETURNS: an editor with the given 1String is appended to the
;;          right end of pre
;; STRATEGY:  Use template for editor on ed
;; EXAMPLES:
;; (insert (make-editor "Hello" "Jenny") "^")=(make-editor "Hello^" "Jenny")
;; (insert (make-editor "" "Jenny") "O")=(make-editor "O" "Jenny")

(define (insert ed ke)
  (make-editor
   (string-append
    (editor-pre ed) ke)
    (editor-post ed)
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-cursor-left : Editor -> Editor
;; GIVEN: an editor
;; RETURNS: returns an editor with the cursor is moved left by one character
;;          if present or returns the given editor
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (move-cursor-left (make-editor "" "Jenny"))=(make-editor "" "Jenny")
;; (move-cursor-left (make-editor "Hello" "Jenny"))=(make-editor "Hell" "oJenny")

(define (move-cursor-left ed)
  (cond
    [(string=? (editor-pre ed) NULL) ed]
    [else (execute-move-left ed)])
)

;; execute-move-left : Editor -> Editor
;; GIVEN: an editor
;; RETURNS: an editor with cursor shifted one character left
;; STRATEGY: Template for editor on ed
;; EXAMPLE:
;; (execute-move-left (make-editor "Hello" "Jenny"))=(make-editor "Hell" "oJenny")

(define (execute-move-left ed)
  (make-editor
    (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1))
    (string-append
      (substring (editor-pre ed) (- (string-length (editor-pre ed)) 1))
      (editor-post ed)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-cursor-right : Editor -> Editor
;; GIVEN: an editor
;; RETURNS: returns an editor with the cursor moved right by one character
;;          if present or returns the given editor
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;; (move-cursor-right (make-editor "Hello" ""))=(make-editor "Hello" "")
;; (move-cursor-right (make-editor "Hello" "Jenny"))=(make-editor "HelloJ" "enny")


(define (move-cursor-right ed)
  (cond
    [(string=? (editor-post ed) NULL) ed]
    [else (execute-move-right ed)])
)

;; execute-move-right : Editor -> Editor
;; GIVEN: an editor
;; RETURNS: an editor with cursor shifted one character right
;; STRATEGY: Template for editor on ed
;; EXAMPLE:
;; (move-cursor-right (make-editor "Hello" "Jenny"))=(make-editor "HelloJ" "enny")

(define (execute-move-right ed)
   (make-editor
    (string-append
      (editor-pre ed)
      (substring (editor-post ed) 0 1)
    )
    (substring (editor-post ed) 1)
  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TESTS:
(begin-for-test
  (check-equal? (edit (make-editor "Welcome" "Eddy") L) (make-editor "Welcom" "eEddy")
                "Left keyevent shifts cursor one character left")
  (check-equal? (edit (make-editor "" "Eddy") L) (make-editor "" "Eddy")
                "Pre is empty so no left move is executed")
  (check-equal? (edit (make-editor "Welcome" "Eddy") R) (make-editor "WelcomeE" "ddy")
                "Right keyevent shifts cursor one character right")
  (check-equal? (edit (make-editor "Welcome" "") R) (make-editor "Welcome" "")
                "Post is empty so no right move is executed")
  (check-equal? (edit (make-editor "Welcome" "Eddy") "\b") (make-editor "Welcom" "Eddy")
                "\b (backspace) keyevent deletes one character on left")
  (check-equal? (edit (make-editor "" "Eddy") "\b") (make-editor "" "Eddy")
                "\b (backspace) keyevent deletes one character on left")
  (check-equal? (edit (make-editor "Welcome" "Eddy") "\r") (make-editor "Welcome" "Eddy")
                "\r (return) keyevent has no effect on editor")
  (check-equal? (edit (make-editor "Welcome" "Eddy") "\t") (make-editor "Welcome" "Eddy")
                "\t (tab) keyevent has no effect on editor")
  (check-equal? (edit (make-editor "Welcome" "Eddy") "\u007F") (make-editor "Welcome" "Eddy")
                "\u007F (delete) keyevent has no effect on editor")
  (check-equal? (edit (make-editor "Welcome" "Eddy") "a") (make-editor "Welcomea" "Eddy")
                "a keyevent will insert an a at rhe end of pre")
  (check-equal? (edit (make-editor "" "Eddy") "%") (make-editor "%" "Eddy")
                "% keyevent will insert an a at rhe end of pre")
  (check-equal? (edit (make-editor "Welcome" "Eddy") " ") (make-editor "Welcome " "Eddy")
                " (space) keyevent will insert a blank space at rhe end of pre")
  (check-equal? (edit (make-editor "Welcome" "Eddy") "") (make-editor "Welcome" "Eddy")
                "No keyevent will have no effect on the editor")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;