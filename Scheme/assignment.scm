;;;;;       SCHEME ASSIGNMENT    ;;;;;   

;; Q 1
;; (fromTo  k  n):     returns  the  list  of  integers  from  k  to  n.  The  size
;;                     of  the  problem  can  be  seen  as  the  difference
;;                     between  k  and  n.
;;  Base  Case:        if  k  =  n  (i.e.  if  the  size  of  the  problem  is  0),  then
;;                     the  result  is  the  list  containing  only  n.
;;  Hypothesis:        Assume  (fromTo  (+  k  1)  n)  returns  the  list  of  integers
;;                     from  k+1  to  n.
;;  Recursive  step:  (fromTo  k  n)  =  (cons  k  (FromTo  (+  k  1)  n)

(define (fromTo k n) (if (= k n) (list n) (cons k (fromTo (+ 1 k) n))))


;; Q 2
;; (removeMults m L):  returns the list containing elements of L that are not multiples of
;;                     m. We check that first element of L is multiple of m or not.
;;                     Accordingly, either add it to the list or don't add. Then recurse 
;;                     on the remaining elements.
;;  Base  Case:        If there are no more elements , Then it means we have checked all previous elements  and removed all
;;                     multiples of m. If empty list is given,output is also an empty list.
;;  Hypothesis:        Assume removeMults gives a list from 2nd to nth element with all 
;;                     multiples of m removed.
;;  Recursive  step:   case 1: first element is multiple of m(removeMults m  L) = (append (list (car L)) (removeMults m (cdr L)))
;;                     case 2: first element is not multiple of m(removeMults m  L)  = (append '() (removeMults m (cdr L)))

(define (removeMults m L) (cond ((null? L) '())
                                  (else (if (= 0 (modulo (car L) m)) (append '() (removeMults m (cdr L)))
                                            (append (list (car L)) (removeMults m (cdr L))))) ))


;; Q 3
;; (removeAllMults L):    returns the list containing elements of L that are not multiples of
;;                        each other. We remove all multiples of first element in L
;;                        and then recurse on the reamining elements. We do this by using removeMults define in Q2
;;                        taking first element of L as m and (cdr L) as L.While recursing on remaining elements
;;                        we know that it has no elements that are multiples of first element. So for this list(cdr L)
;;                        we again use removeMults taking m as its first element.We proceed recursively till we reach the base case. 
;;  Base  Case:           If there is one elment in the list,then it is the multiple of itself only and we
;;                        return that list as it is.
;;  Hypothesis:           Assume removeAllMults gives a list from 2nd to nth element(cdr L) that 
;;                        has no elements that are multiples of each other. This list can possibly have only
;;                        multiples of first element. The first element is the smallest and can'tbe multiple of
;;                        any element, so it has to be in the list.
;;  Recursive  step:      (removeAllMults  L) = (cons (car L)(removeMults(car L) (removeAllMults (cdr L))))



(define (removeAllMults L) (cond ((= 1 (length L)) L)
                                   (else (cons (car L)(removeMults(car L) (removeAllMults (cdr L)))))   ))



;; Q 4
;; (primes n):         returns the list of primes less than or equal to n. We first create a list of
;;                     all elements from 2 to n (using (fromTo) as defined in Q1) and then pass this list
;;                     to removeAllMults(as defined in Q3). The result we get is the list containing all
;;                     primes less than n.ze
;;                     of  the  problem  can  be  seen  as  the  difference
;;                     between  k  and  n.
;;  Base  Case:        if  n is less than 2 we retun empty list.


(define (primes n) (cond((< n 2) '())(else (removeAllMults (fromTo 2 n)))))


;; Q 5
;; (maxdepth  L):      returns the maximum nesting depth of any element within L, such that
;;                     top elements are at level 0.
;;                     
;;  Base  Case:        For list that is empty or has single element return 0  
;;                     
;;  Hypothesis:        Assume  (maxdepth L) works on list of size smaller than L and we call it
;;                     recursively on (car L) and (cdr L)
;;                     
;;  Recursive  step:   (maxdepth L)  =  max (maxdepth (cdr L)) (+ 1 (maxdepth (car L))))

(define (maxdepth L) (cond ((and (list? L) (= 0 (length L))) (+ 0 0))
                             ((and (and (list? L)(not (list? (car L)))) (= 1 (length L))) (+ 0 0))
                             ;((and (list? L) (= 1 (length L))) (+ 0 0))
                             ((not (list? L)) (- 0 1))
                             (else (max (maxdepth (cdr L)) (+ 1 (maxdepth (car L)))))))


;; Q 6
;; (prefix expr):      returns the prefix form of  the  list  of  integers  from  k  to  n.  
;;                     
;;                     
;;  Base  Case:        if  expr is an atom(symbol) we return it as it is.if expr is an empty list, return '().
;;                     if expr is a list of size 1(i.e.it contains one element)we return that element (but not in a list).
;;  Hypothesis:        Assume  (prefix expr) works on expression smaller than expr. We know that
;;                     for expr1 op expr2 prefix is op prefix(exp1) prefix(exp2). op is (cadr expr),exp1 is (car expr) and exp2 is (cddr expr
;;                     from  k+1  to  n.
;;  Recursive  step:  (prefix expr)  = (list (cadr expr) (prefix (car expr)) (prefix (cddr expr)))

(define (prefix expr) (cond ((symbol? expr) expr)
                             ((number? expr) expr)
                             ( (and (list? expr) (= 0 (length expr))) '() )
                             ((and (list? expr) (= 1 (length expr))) (car expr))
                             (else (list (cadr expr) (prefix (car expr)) (prefix (cddr expr)))) ) )


;; Q 7
;; (composition fns):  returns a function that is the composition of the functions in fns
;;                     between  k  and  n.
;;  Base  Case:        if  fns has only one function, say f1 then composition is a function tht is nothing but f1(x) 
;;                     
;;  Hypothesis:        Assume  (composition fns) works for list that is size less than n then we
;;                     can compute recursively composition of (cdr fns) and pass it to (car fns) on parameter x.
;;                     
;;  Recursive  step:  (composition fns )  =  (lambda (x) ((car fns) ((composition (cdr fns) ) x)) )
(define (composition fns) (cond ((null? (cdr fns)) (lambda (x) ((car fns) x) ))
                                  (else (lambda (x) ((car fns) ((composition (cdr fns) ) x)) ))  )  )