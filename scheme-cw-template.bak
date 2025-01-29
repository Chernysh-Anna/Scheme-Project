;;Partner A: Anna Chernysh 
;;Student ID: 001376055
;;Partner B: 
;;Student ID: 


;;Data format: Name, Mother, Father, Date of birth, Date of death.
;;An empty list means Unknown.

;;Maternal branch
(define Mb
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
((Mary Jones) (() ())((12 5 1967) (19 5 2024)))
((Tom Blake) (() ()) ((17 1 1964) ()))
((Ada West) (() ()) ((22 8 1973) ()))
((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
((Ned Bloom) (() ()) ((23 04 2001)()))
((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

;,Paternal branch
(define Pb
'(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021))) 
((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
((John Doe) (() ()) ((18 2 1899)(7 7 1970)))
((Lisa Brown) (() ())((31 6 1904) (6 3 1980)))
((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))

;;define lst-mb
;;define lst-pb
;;define lst-all

;; C1
;Members in the Maternal Branch
(define (lst-mb mb)
  mb)
 
;; C2
;Members in the Paternal Branch
(define (lst-pb pb)
  pb)
  
;; C3
;All Members
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))
			
(define (lst-all mb pb)
  (append-lst mb pb))
;(lst-all Mb Pb)

;; A1 --> List All Parents in the Branch
;SHOULD WE DELETE EMPTY LST AND DUPLICATES?
(define (parents lst)
  (if (null? lst)                    
      '()                            
      (cons (car(cdr (car lst)))       
            (parents (cdr lst))))) 
;(parents (lst-all Mb Pb))
  
;; A2-->List All Living Members

(define (is-alive? person)
  (if (null? (car (cdr (car (cdr (cdr  person)))))) #t #f))
;TEST: forgot to add two car for line 74 --> wrong result
(define (living-members lst)
  (if (null? lst) '()                                            
      (if (is-alive? (car lst)) 
          (cons (car (car lst))               
                (living-members (cdr lst)))   
          (living-members (cdr lst))))) 
;(living-members Pb)



;; A3 -->Current Age of All Living Members
;shoud it be separete by branch?

;(let ((date (seconds->date (current-seconds))))
  ;(display (date-month date)))
;---convert update datem


(define (current-age lst)
  ())
 

  
 ; (current-age Mb)




;; A4
(define (same-birthday-month lst month)
  ())
  
;; A5
(define (sort-by-last lst)
  ())
  
;; A6
(define (change-name-to-Juan lst old-name new-name)
  ())
 
;;
;; B1
(define (children lst)
  ())
  
;; B2
(define (oldest-living-member lst)
  ())
  
;; B3
(define (average-age-on-death lst)
  ())
  
;; B4
(define (birthday-month-same lst month)
  ())
  
;; B5
(define (sort-by-first lst)
  ())
  
;; B6
(define (change-name-to-Maria lst old-name new-name)
  ())
  
;;
;;You should include code to execute each of your functions below.

