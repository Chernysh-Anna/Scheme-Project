 ;;Your full name: Krishna Tamang
;;Student ID: 001428127
;;Date of birth (day/month/year): 03/04/2004

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



;; B1
(define (children lst)
  (if (null? lst)  ; Base case: If the list is empty, return an empty list
      '()
      (let* ((member (car lst))  
             (children-of-member (car member))) 
        (append children-of-member(children (cdr lst))))))  ; Recurse on the rest of the family tree

;(children Pb)
      
  
;; B2
(define (oldest-living-member lst)
  (define current-year 2025)  ; Current year, can be changed

  (define (age person)
    (- current-year (caddr (car (caddr person)))))  ; Calculate age from birth year

  (define (is-living? person)
    (null? (cadr (caddr person))))  ; Check if there's no death date

  ; Helper function to find the oldest living member 
  (define (find-oldest living-members current-oldest)
    (if (null? living-members)
        current-oldest  ; Base case
        (let ((person (car living-members)))
          (if (> (age person) (age current-oldest))
              (find-oldest (cdr living-members) person)  ; New oldest
              (find-oldest (cdr living-members) current-oldest)))))  ; Keep current oldest

  (define living-members (filter is-living? lst))  ; Filter out only living members

  (find-oldest living-members (car living-members)))  ; Start with the first living member

(oldest-living-member Pb)
  
;; B3
(define (average-age-at-death lst)
  (let ((ages (filter number? (map age-at-death lst))))  ;; Get valid ages
    (if (null? ages)    ;Base case
        0 
        (round (/ (apply + ages) (length ages))))))  ;; Calculate rounded average age
(define (age-at-death person)
  (let ((birth (caddr (car (caddr person))))    ;; Extract birth year
        (death (if (null? (cadr (caddr person))) #f (caddr (cadr (caddr person)))))) ;; Extract death year if exists
    (if death (- death birth) #f)))            ;; Calculate age if death year exists, else return #f

(average-age-at-death Pb)
  
;; B4
(define (birthday-month-same lst month)
  (map car   ;returns the first element from the filtered list
       (filter (λ (member) ;creates a new list which fulfils the condition 
                 (= month (cadr (car (caddr member)))))  ;Checks if the input matches the birthday month of the member
               lst)))

(birthday-month-same Pb '5)
  
;; B5
(define(sort-by-first lst)
  (sort lst
        (λ (a b) ;Define two arguemnts 
          (string<? ;Compare them by their elements 
          (symbol->string (car(car a))) ;Converts the first name into a string (for eg "John Smith")
          (symbol->string (car(car b))))))) ;Converts the secind name into a string (for eg "Ana Smith")
(sort-by-first Pb)
  
;; B6
(define (change-name-to-Maria lst old-name new-name)
  (if (null? lst)    ; Base case: If list is empty, return an empty list
      '()
      (let ((member (car lst)))  ; Get the first member
        (if (equal? (car (car member)) old-name)  ; Check first name in name list
            (cons (cons (cons new-name (cdr (car member))) (cdr member))  ; Keep full name
                  (change-name-to-Maria (cdr lst) old-name new-name))  ; Recursively process the rest
            (cons member (change-name-to-Maria (cdr lst) old-name new-name))))))  ; Continue recursion

;(change-name-to-Maria Pb 'Mary 'Maria)



;;define lst-mb
;;define lst-pb
;;define lst-all

;; C1
(define (lst-mb mb)
  Mb)
 
;; C2 
(define (lst-pb pb)
  Pb)

  
;; C3
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))
			
(define (lst-all mb pb)
  (append-lst mb pb))
(lst-all Mb Pb)


