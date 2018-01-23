;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;PROBLEM SET 02 - QUESTION 03;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;coffee-machine.rkt;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-location "02" "coffee-machine.rkt")

(require rackunit)
(require "extras.rkt")

(provide
  initial-machine
  machine-next-state
  machine-output
  machine-remaining-coffee
  machine-remaining-chocolate
  machine-bank 
  )

;;CONSTANTS

(define COFFEE "coffee")
(define HOT-CHOCOLATE "hot chocolate")
(define CHANGE "change")

(define OUT-OF-ITEM "Out of Item")
(define NOTHING "Nothing")

(define COFFEE-COST 150)
(define HOTCHOC-COST 60)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct machineState (num-coffee num-hotchocolate bank init-deposit))

;; A MachineState is (make-machineState NonNegInt NonNegInt NonNegInt NonNegInt)
;; INTERPRETATION:
;;   num-coffee = number of cups of coffee currently available in the machine
;;   num-hotchocolate = number of cups of hot chocolate currently available
;;                     in the machine
;;   bank = the money it has kept from customers' purchases in cents i.e USD*100
;;          (e.g. $5.45 => 545)
;;   init-deposit = money entered by user in the machine before selecting any input
;;                    choices in cents i.e USD*100


;; machineState-fn : MachineState -> ??
#|                   
(define (machine-state-fn state)
  (...
    (machineState-num-coffee state)
    (machineState-num-hotchocolate state)
    (machineState-bank state)
    (machineState-init-deposit state)))
|#

;.................................................................................
#|
A CustomerInput is one of
-- a PosInt        interp: insert the specified amount of money, in cents
-- COFFEE          interp: request a coffee
-- HOT-CHOCOLATE   interp: request a hot chocolate
-- CHANGE          interp: return all the unspent money that the
                             customer has inserted

;;TEMPLATE:
(define (customer-input-fn ci)
(cond
    [(number? ci)...]
    [(string=? COFFEE ci)...]
    [(string=? HOT-CHOCOLATE ci)...]
    [(string=? CHANGE ci)...]))
|#

#|
A MachineOutput is one of
-- COFFEE         interp: machine dispenses a cup of coffee
-- HOT-CHOCOLATE  interp: machine dispenses a cup of hot chocolate
-- OUT-OF-ITEM    interp: machine displays "Out of Item"
-- a PosInt         interp: machine releases the specified amount of
                            money, in cents
-- NOTHING        interp: the machine does nothing

;;TEMPLATE:
(define (machine-output-fn mo)
(cond
    [(string=? COFFEE mo)...]
    [(string=? HOT-CHOCOLATE mo)...]
    [(string=? OUT-OF-ITEM mo)...]
    [(number? mo)...]
    [(string=? NOTHING mo)...]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;initial-machine : NonNegInt NonNegInt -> MachineState
;;GIVEN: number of cups of coffee and of hot chocolate
;;RETURNS: the state of a machine loaded with the given number of cups
;;         of coffee and of hot chocolate, with an empty bank.
;;STRATEGY: Combine simpler functions
;;EXAMPLE:
;;(initial-machine 20 30)=(make-machineState 20 30 0 0)

(define (initial-machine num-coffee num-hotchocolate)
  (make-machineState
    num-coffee
    num-hotchocolate
    0
    0
  )
)

;TEST:
(begin-for-test
  (check-equal? (initial-machine 20 30)
                (make-machineState 20 30 0 0)
                "machine is loaded with 20 cups of coffee and 30 cups of hot chocolate"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

;;machine-next-state : MachineState CustomerInput -> MachineState
;;GIVEN: a machine state and a customer input
;;RETURNS: the state of the machine that should follow the customer's
;;input
;;STRATEGY: 
;;STRATEGY: Use template for CustomerInput on ci


;;EXAMPLE:

;;State after loading deposit :
;;(machine-next-state(initial-machine 20 30) 200)=(make-machineState 20 30 0 200)

;;State followed by requesting coffee without loading money :
;;(machine-next-state(initial-machine 20 30) COFFEE)=(make-machineState 20 30 0 0)

;;State followed by requesting coffee after loading money :
;;(machine-next-state(make-machineState 20 30 0 200) COFFEE)
;;                   =(make-machineState 19 30 150 50)

;;State after requesting change after ordering coffee:
;;(machine-next-state (make-machineState 19 30 150 50) CHANGE) = (make-machineState 19 30 150 0)

;;Request change after immediately after depositing and before ordering:
;;(machine-next-state(machine-next-state(initial-machine 20 30) 200) CHANGE)
;;                   =(make-machineState 20 30 0 0)

;;Unchanged state returned for requesting item which is not in stock :
;;(machine-next-state(make-machineState 0 30 0 200) COFFEE)
;;                   =(make-machineState 0 30 0 200)

(define (machine-next-state state ci)
  (cond
    [(number? ci) (take-deposit state ci)]
    [else (take-order state ci)])
)

;;TESTS:

(begin-for-test
  (check-equal? (initial-machine 20 30) (make-machineState 20 30 0 0)
                "machine is loaded with 20 cups of coffee and 30 cups of hot chocolate")
  (check-equal? (machine-next-state(initial-machine 20 30) 200)
                (make-machineState 20 30 0 200)
                "200  cents is loaded in the machine")
  (check-equal? (machine-next-state(initial-machine 20 30) COFFEE)
                (make-machineState 20 30 0 0)
                "no changes made after requesting coffee because deposit is not loaded")
  (check-equal? (machine-next-state(make-machineState 20 30 0 200) COFFEE)
                (make-machineState 19 30 150 50)
                "coffee request is accepted after paying deposit")
  (check-equal? (machine-next-state(make-machineState 19 30 150 50) CHANGE)
                (make-machineState 19 30 150 0)
                "change rendered after completing coffee request")
  (check-equal? (machine-next-state(machine-next-state(initial-machine 20 30) 200)
                CHANGE)
                (make-machineState 20 30 0 0)
                "deposit returned as change as no order is made")
  (check-equal? (machine-next-state(make-machineState 0 30 0 200) COFFEE)
                (make-machineState 0 30 0 200)
                "same state is returned as coffee is out of stock")
  
  
  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;take-deposit: MachineState CustomerInput -> MachineState
;;GIVEN: a machine state and the amount entered by customer
;;RETURNS: a state with deposit amount loaded
;;STRATEGY: Use template for MachineState on state
;;EXAMPLES:
;;(take-deposit (initial-machine 20 30) 250)=(make-machineState 20 30 0 250)

(define (take-deposit state ci)
  (make-machineState
        (machineState-num-coffee state)
        (machineState-num-hotchocolate state)
        (machineState-bank state)
        (+ (machineState-init-deposit state) ci)
  )
        
)

;TESTS:
(begin-for-test
  (check-equal? (take-deposit (initial-machine 20 30) 250) (make-machineState 20 30 0 250)))

;...............................................................................................

;; take-order : MachineState CustomerInput -> MachineState
;; GIVEN:  a machine state and input selection entered by customer
;; RETURNS: a state with selected order processed 
;; STRATEGY: Use template for CustomerInput on ci
;; EXAMPLES:
;; (take-order (make-machineState 20 30 0 250) COFFEE)
;;             =(make-machineState 19 30 150 100)
;; (take-order (initial-machine 20 30) COFFEE)=(make-machineState 20 30 0 0)

(define (take-order state ci)
  (cond
           [(string=? COFFEE ci) (select-coffee state)]
           [(string=? HOT-CHOCOLATE ci) (select-hotchocolate state)]
           [(string=? CHANGE ci) (remove-change state)]
           [else state] )
)

;TESTS:
(begin-for-test
  (check-equal? (take-order (make-machineState 20 30 0 250) COFFEE)
                (make-machineState 19 30 150 100)
                "takes order for coffee as the deposit has been paid")
  (check-equal? (take-order (initial-machine 20 30) COFFEE)
                (make-machineState 20 30 0 0)
                "Doesn't take order as no deposit is made"))

;...................................................................................
 
;; select-coffee : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: a state with coffee order processed if there is coffee in machine and
;;          amount is deposited else returns same machine state
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;;(select-coffee (initial-machine 20 30))=(make-machineState 20 30 0 0)
;;(select-coffee (make-machineState 19 30 170 200))=(make-machineState 18 30 320 50)

(define (select-coffee state)
  (if  ;;text-expr
       (and (> (machine-remaining-coffee state) 0)
            (>= (machineState-init-deposit state) COFFEE-COST))
       ;;then-expr
       (accept-coffee-order state)
       ;;else-expr
       state
  )
)

;TESTS:
(begin-for-test
  (check-equal? (select-coffee (initial-machine 20 30))
                (make-machineState 20 30 0 0)
                "coffee order is not processed due to no deposit")
  (check-equal? (select-coffee (make-machineState 19 30 170 200))
                (make-machineState 18 30 320 50)
                "coffee order processed "))
;......................................................................................
;; accept-coffee-order : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: a machine state after order for coffee is processed
;; STRATEGY: Use template for MachineState on state
;; EXAMPLES:
;;(accept-coffee-order (make-machineState 20 10 0 210))=(make-machineState 19 10 150 60)

(define (accept-coffee-order state)
  (make-machineState
             (- (machineState-num-coffee state) 1)
             (machineState-num-hotchocolate state)
             (+ (machineState-bank state) COFFEE-COST)
             (- (machineState-init-deposit state) COFFEE-COST))
)

;TEST:
(begin-for-test
  (check-equal? (accept-coffee-order (make-machineState 20 10 0 210)) 
                (make-machineState 19 10 150 60)))

;................................................................................

;; select-hotchocolate : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: a state with hot chocolate order processed if there is hot chocolate in
;;           machine and amount is deposited else returns same machine state
;; STRATEGY: Combine simpler functions
;; EXAMPLES:
;;(select-hotchocolate (make-machineState 20 30 0 100))
;;                 =(make-machineState 20 29 60 40)
;;(select-hotchocolate (make-machineState 20 30 5 10))
;;                 =(make-machineState 20 30 5 10)

(define (select-hotchocolate state)
  (if (and (> (machine-remaining-chocolate state) 0)
           (>= (machineState-init-deposit state) HOTCHOC-COST))
      ;;then-expr
      (accept-hotchocolate-order state)
      ;;else-expr
       state
  )
)

;TESTS:
(begin-for-test
  (check-equal? (select-hotchocolate (make-machineState 20 30 0 100))
                (make-machineState 20 29 60 40)
                "order processed")
  (check-equal? (select-hotchocolate (make-machineState 20 30 5 10))
                (make-machineState 20 30 5 10)
                "order not processed"))
;............................................................................

;; accept-hotchocolate-order :  MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS:  a machine state after order for hot chocolate is processed
;; STRATEGY: Use template for MachineState on state

;;EXAMPLE:
;;(select-hotchocolate (make-machineState 20 30 0 100))
;;                 =(make-machineState 20 29 60 40)


(define (accept-hotchocolate-order state)
  (make-machineState
                        (machineState-num-coffee state)
                        (- (machineState-num-hotchocolate state) 1)
                        (+ (machineState-bank state) HOTCHOC-COST)
                        (- (machineState-init-deposit state) HOTCHOC-COST))
)
;TESTS:
(begin-for-test
  (check-equal? (select-hotchocolate (make-machineState 20 30 0 100))
                (make-machineState 20 29 60 40)
                "order processed"))
;.................................................................................

;; remove-change :  MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS:  a state after the change has been rendered to the customer
;; STRATEGY: Use template for MachineState on state
;; EXAMPLE:
;;(remove-change (make-machineState 20 30 60 40))=(make-machineState 20 30 60 0)
                      
(define (remove-change state)
  (make-machineState
                        (machineState-num-coffee state)
                        (machineState-num-hotchocolate state)
                        (machineState-bank state)
                         0
))

(begin-for-test
  (check-equal? (remove-change (make-machineState 20 29 60 40))
                (make-machineState 20 29 60 0)
                "40 cents of change left in deposit is returned back, hence deposit is now 0 cents"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;machine-output : MachineState CustomerInput -> MachineOutput
;;GIVEN: a machine state and a customer input
;;RETURNS: a MachineOutput that describes the machine's response to the
;;customer input
;;STRATEGY: Use template for CustomerInput on ci
;;EXMAPLES:

;;machine output if deposit is loaded :
;;(machine-output(initial-machine 20 30) 200)="Nothing"

;;machine output for coffee :
;;(machine-output(initial-machine 20 30) COFFEE)="coffee"

;;machine output if coffee is out of stock :
;;(machine-output(initial-machine 0 30) COFFEE)="Out of Item"

;;machine output for hot chocolate :
;;(machine-output(initial-machine 20 30) HOT-CHOCOLATE)="Hot chcolate"

;;machine output if hot chocolate is out of stock :
;;(machine-output(initial-machine 10 0) HOT-CHOCOLATE)="Out of Item"

;;change returned is 0 cents since no deposit is paid and no order is made :
;;(machine-output(initial-machine 20 30) CHANGE)=0

;;Change is returned after valid order is placed and deposit is paid :
;;(machine-output(make-machineState 19 30 150 50) CHANGE)=50


(define (machine-output state ci)
(if (number? ci)
         (output-for-deposit state)
         (cond
           [(string=? COFFEE ci) (output-for-coffee state)]
           [(string=? HOT-CHOCOLATE ci) (output-for-hotChocolate state)]
           [(string=? CHANGE ci) (output-for-change state)]
           [else state]
         )
     )
  )

;TESTS:
(begin-for-test
  (check-equal? (machine-output(initial-machine 20 30) 200) "Nothing")
  (check-equal? (machine-output(make-machineState 20 20 0 260) COFFEE) "coffee")
  (check-equal? (machine-output(initial-machine 0 30) COFFEE) "Out of Item")
  (check-equal? (machine-output(make-machineState 20 20 0 260) HOT-CHOCOLATE) "hot chocolate")
  (check-equal? (machine-output(make-machineState 20 20 0 26) HOT-CHOCOLATE) "Out of Item")
  (check-equal? (machine-output(initial-machine 20 30) CHANGE)
                0
                "initially no transaction is made and so no change is there to return")
  (check-equal? (machine-output(make-machineState 19 30 150 50) CHANGE)
                50
                "returns change in cents after processing order"))
;...................................................................................

;; output-for-deposit : MachineState -> MachineOutput
;; GIVEN:  a machine state with amount deposited
;; RETURNS: a string "Nothing"
;; STRATEGY: Combine simpler functions
;; EXAMPLE:
;;(output-for-deposit(machine-next-state (initial-machine 20 30) 100))
;;                   = "Nothing"

(define (output-for-deposit state) NOTHING)

;TEST:
(begin-for-test
  (check-equal? (output-for-deposit(machine-next-state (initial-machine 20 30) 100))
                "Nothing"))

;...................................................................................

;; output-for-coffee : MachineState -> MachineOutput 
;; GIVEN:  a machine state with coffee requested
;; RETURNS: "coffee" if it is in stock or "Out of item"
;; STRATEGY: Use template for MachineState on state
;; EXAMPLES:
;; (output-for-coffee(make-machineState 20 20 0 260))
;;                         ="coffee"
;; (output-for-coffee(machine-next-state (initial-machine 0 30) COFFEE))
;;                        ="Out of Item"

(define (output-for-coffee state)
  (if (and (> (machine-remaining-coffee state) 0)
           (> (machineState-init-deposit state) COFFEE-COST))
      COFFEE      
      OUT-OF-ITEM
  )
)

;TESTS:
(begin-for-test
  (check-equal? (output-for-coffee(make-machineState 20 20 0 260))
                "coffee")
  (check-equal? (output-for-coffee(machine-next-state (initial-machine 0 30) COFFEE))
                "Out of Item"))

;..................................................................................

;; output-for-hotChocolate : MachineState -> MachineOutput 
;; GIVEN:  a machine state with hot chocolate requested
;; RETURNS: "hot chocolate" if it is in stock or "Out of item"
;; STRATEGY: Use template for MachineState on state
;; EXAMPLES:
;; (output-for-hotChocolate(make-machineState 20 20 0 260))
;;                         ="hot chocolate"
;; (output-for-hotChocolate(machine-next-state (initial-machine 20 0) HOT-CHOCOLATE))
;;                        ="Out of Item"


(define (output-for-hotChocolate state)
  (if (and (> (machine-remaining-chocolate state) 0)
           (> (machineState-init-deposit state) HOTCHOC-COST))
      HOT-CHOCOLATE
      OUT-OF-ITEM
  )
)

;TESTS:
(begin-for-test
  (check-equal? (output-for-hotChocolate(make-machineState 20 20 0 260))
                "hot chocolate")
  (check-equal? (output-for-hotChocolate(machine-next-state (initial-machine 20 0)
                HOT-CHOCOLATE))
                "Out of Item"))

;...................................................................................

;; output-for-change : MachineState -> MachineOutput 
;; GIVEN:  a machine state with change requested
;; RETURNS: change in cents
;; STRATEGY: Use template for MachineState on state
;; EXAMPLES:
;; (output-for-change (machine-next-state (machine-next-state(initial-machine 20 30) 100)
;;                    HOT-CHOCOLATE)) = 40
;;(output-for-change  (machine-next-state(initial-machine 20 30) 10))
;;                    10

(define (output-for-change state)
  (machineState-init-deposit state)
)

(begin-for-test
  (check-equal? (output-for-change (machine-next-state (machine-next-state(initial-machine
                20 30) 100) HOT-CHOCOLATE))
                40
                "40 cents are returned from 100 cents after pushing 60 cents for
                hot chocolate in bank")
  (check-equal? (output-for-change (machine-next-state(initial-machine 20 30) 10))
                10
                "No order is made hence deposit is given back on change request"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;machine-remaining-coffee : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of cups of coffee left in the machine
;;STRATEGY: Use template for MachineState on state
;;EXAMPLE:
;;(machine-remaining-coffee (initial-machine 20 0))=20

(define (machine-remaining-coffee state)
  (machineState-num-coffee state)
)

;TEST:
(begin-for-test
  (check-equal? (machine-remaining-coffee (initial-machine 20 0))
                20
                "returns number of cups of coffee presently loaded in machine"))

;...........................................................................

;;machine-remaining-chocolate : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of cups of hot chocolate left in the machine
;;STRATEGY: Use template for MachineState on state
;;EXAMPLE:
;;(machine-remaining-chocolate (initial-machine 20 0))=0

(define (machine-remaining-chocolate state)
  (machineState-num-hotchocolate state)
)

;TEST:
(begin-for-test
  (check-equal? (machine-remaining-chocolate (initial-machine 20 0))
                0
                 "returns number of cups of hot chocolate presently loaded in machine"))
;.....................................................................................

;;machine-bank : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the amount of money in the machine's bank, in cents
;;STRATEGY: Use template for MachineState on state
;;EXAMPLE:
;;(machine-bank (initial-machine 20 0))=0
;;(machine-bank (machine-next-state (machine-next-state(initial-machine 20 30) 100)
;;              HOT-CHOCOLATE))=60

(define (machine-bank state)
  (machineState-bank state)
)

;TESTS:

;Contants fo test:
(define CHANGE-AFTER-HOT-CHOCOLATE
       (machine-next-state (machine-next-state(initial-machine 20 30) 100) HOT-CHOCOLATE))

(begin-for-test
  (check-equal? (machine-bank (initial-machine 20 0))
                0
                "initially bank is empty")
  (check-equal? (machine-bank CHANGE-AFTER-HOT-CHOCOLATE)
                60
                "After processing order, 60 cents are loaded in bank")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;