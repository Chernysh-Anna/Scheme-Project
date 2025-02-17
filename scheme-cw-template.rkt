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
(define (lst-mb mb)
  mb)
 
;; C2 
(define (lst-pb pb)
  pb)
  
;; C3
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))
			
(define (lst-all mb pb)
  (append-lst mb pb))

;; A1
;;--> List All Parents in the Branch

(define (tr-parents lst) ;;list of all parents
  (define (parent lst acc) 
    (if (null? lst) acc 
        (parent (cdr lst) (cons (car(cdr (car lst))) acc)))) 
  (parent lst '()))

(define (remove-duplicates lst) ;;remove duplicates
  (if (null? lst) '()
      (let ((rest (remove-duplicates (cdr lst))))
        (if (member (car lst) rest)
            rest
            (cons (car lst) rest)))))

(define (tr-parents-unique lst) ;;format answer
  (reverse (remove-duplicates (tr-parents lst))))


;; A2
;;-->List All Living Members

(define (is-alive? person)
  (if (null? (car (cdr (car (cdr (cdr  person)))))) #t #f)) ;;check if persen has death date

(define (tr-living-members lst) ;;all living members
  (define (living-members lst acc) 
    (if (null? lst) (reverse acc)  
        (if (is-alive? (car lst)) 
            (living-members (cdr lst) (cons (car (car lst)) acc)) 
            (living-members (cdr lst) acc)))) 

  (living-members lst '())) 


;; A3
;;-->Current Age of All Living Members

(define (tr-current-age lst) 
  (let* ((date (seconds->date (current-seconds)))  ; Get current date 
         (current-year (date-year date)) 
         (current-month (date-month date)) 
         (current-day (date-day date))) 

    ; Calculate age 

    (define (calculate-age birth-date) 
      (let* ((birth-day (car birth-date))    
             (birth-month (car (cdr birth-date)))   
             (birth-year (car (cdr (cdr birth-date))))   
             (year-diff (- current-year birth-year)))   
        (if (or (< birth-month current-month)  
                (and (= birth-month current-month) (<= birth-day current-day))) 
            year-diff 
            (- year-diff 1))))  ;; Subtract 1 if birthday hasn't occurred yet 
    ; Main logic 

    (define (current-age lst acc) 
      (if (null? lst)acc 
          (if (is-alive? (car lst)) 
              (current-age (cdr lst)  
                           (cons (list (car (car lst)) ; Name 
                                      (calculate-age (car (car (cdr (cdr (car lst))))))  ; Age 
                                 ) acc)) 
              (current-age (cdr lst) acc))))   
    (current-age lst '())))  


;; A4
;;--> return a list of all members who have a birthday in the same month as input month

(define (same-birthday-month lst month)
  (map car
       (filter (λ (person)
                 (= month (car (cdr (car (car (cdr (cdr person)))))))) 
               lst)))
  
;; A5
;;-->sorts members of the branch by surname
(define (sort-by-lastname lst)
  (sort lst 
        (λ (a b) 
          (string<? 
           (symbol->string (car(cdr (car a))))
           (symbol->string (car(cdr (car b))))))))
  
;; A6
;;-->searche for a specific given name and replaces it with a new name 
(define (change-name-to-Juan lst  old-name new-name)
  (map 
   (λ (person) (let ((name (car person)))
       (if (equal? (car name) old-name)
           (cons (cons  new-name (cdr name)) (cdr person))
           person)))
   lst))
 
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
  

(display "------  Code to execute each of functions ------\n")



  (display "C1. List all members from Maternal Branch\n")
   (lst-mb Mb)

  (display "C2. List all members from Paternal Branch\n")
  (lst-pb Pb)  

  (display "C3. List all members from both branches\n")
  (lst-all Mb Pb)

  (display "A1. List all parents\n")
  (display "--> Mb \n")
  (tr-parents-unique Mb)
  (display "--> Pb \n")
  (tr-parents-unique Pb)
  (display "--> All \n")
  (tr-parents-unique (lst-all Mb Pb))

  (display "A2. List all living members\n")
  (display "--> ")
  (tr-living-members (lst-all Mb Pb))

  (display "A3. Show current ages of all living members\n")
  (display "--> ")
  (tr-current-age (lst-all Mb Pb))

  (display "A4. Find members born in a specific month\n")
  (display "--> Example: Members born in month 5\n")
  (same-birthday-month (lst-all Mb Pb) 5)

  (display "A5. Sort members by last name\n")
  (display "--> ")
  (sort-by-lastname (lst-all Mb Pb))

  (display "A6. Change name 'John' to 'Juan'\n")
  (display "--> ")
  (change-name-to-Juan (lst-all Mb Pb) 'John 'Juan)

 



