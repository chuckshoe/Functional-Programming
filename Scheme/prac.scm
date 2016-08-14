; expressions are in paranthesis () and they are evaluated
; unless you ' ()
(quote x)  ; same as 'x or ' x
; just after the paran we have a function or keyword like (function OR keyword  ...)
"the list is : " '(1 2 3 4 5)

; DEFINING VARIABLES / FUNCTIONS
(define x 3)  ; define variables
x
(define (f x y) (+ x y)) ; define functions
(f 3 4)
; NOTE if u do, define x -> variable OR if u do define ( -> defining a function.
; the ( after a define tells it is defining a function

; CONDITIONAL STATEMENTS : if and cond
; (if (= 3 4) ("condition match") ("condition not match"))  NOTE: this gives error
; coz paran means expression and expression start from function or keyword 
(if (= 3 4) "condition match" "condition not match")
(if (< 3 4) 3 4)
; COND can be used instead of nesting if
(cond ((< 4 3) 1)
      ((= 4 3) 2)
      ((>= 4 3) '3)) ; NOTE : ' also works. we just made it a symbol.

; LISTS : are heterogenous , can have elements of different type
; 1. can create list simply by listing the elements & must use '
; (1 2 3 "chs") gives error as first element inside () has to be fn name or keyword
; instead o :
"list using '"'(1 2 3 "chs")
; 2. by using (LIST ...)
"list using list :"(list '(1 2 3 4 5) 2 )
; 3. can use (cons x L) -> gives a list with x as first element. does not modify L 
"list using cons"(cons 1 2) ; this does not create a list instead creates a pair
(cons '(1 2) '(2)) ; creates a nested list
; 4. use append L1 L2 : takes 2 list , returns after appending L2 with L1
"list using append"(append '(1) '(2 3 4 5))
;(append 1 '(2 3 4 5))   ; gives error coz append takes 2 lists

;; LIST FUNCTIONS : CAR , CDR, REVERSE
; (car L)
; (cdr L)
; (reverse L)
"list functions :car, cdr, reverse"
(car '(1 2 3))
(cdr '(1 2 3))
(reverse '(1 2 3 4))
; note : reverse reverses only at one level
(reverse '(1 2 (3,4)))
 
; CAR CDR shorthands
(car (cdr '(1 2 3 4))) ; gives 2nd element
; this is same as this one :
(cadr '(1 2 3 4 ))

; MAP procedure :
(map + '(1 2 3) '(4 5 6)) 
(map cadr '((a (b,c)) (d e) (g h)))

; nested declarations ; defining new scopes
; let, let*, letrec
 (let ((x 10)
        (y (+ x 2)))  ;; the x here refers to the outer x, which is the global x =3
    y)
 (let* ((x 10)
        (y (+ x 2)))  ;; the x here refers to the outer x, which is the local x=10 whose
    y)
;;; LETREC allows you to define recursive functions within a nested scop
(define (f x)
  (letrec ((fac (lambda (x) (if (= x 0) 1 (* x (fac (- x 1)))))))
     (fac x))
  (f 6))
  (define (g x)
    (letrec ((fac1 (lambda (x) (if (= x 0) 1 (* x (fac2 (- x 1))))))
             (fac2 (lambda (x) (fac1 x))))
      (fac1 x)))
  (g 6)

; boolean
(and #t (equal? '(1 2 3 4) '(1 2 3 4)))