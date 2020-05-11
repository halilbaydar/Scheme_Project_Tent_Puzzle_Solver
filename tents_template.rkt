#lang scheme
; 2017400297


;You can replace #f's with your function definitions and define more helper functions as you need to use this template.

; Solver function
(define tent_list '((7 3) (6 1) (5 4) (5 8) (5 6) (2 4) (3 7) (2 2) (3 8) (0 4) (1 7) (1 4)))

(define (TENTS-SOLUTION liste)  (eliminate_neighbor_with_tree_and_tent (car liste) (cadr liste) tent_list '(5 5))) 

(define (REPLACE-NTH list n item) (if (= n 1)  (cons item (cdr list)) (cons (car list) (REPLACE-NTH (cdr list) (- n 1) item))))

; Helper functions
(define RETURN-FIRST-NOT-FALSE (lambda (liste)( if (null? liste) #f(if (> (car liste) 5)(* (car liste) (car liste) )( RETURN-FIRST-NOT-FALSE (cdr liste))))))
(define ADJACENT(lambda (liste1 liste2) (define x1 (car liste1)) (define y1(cadr liste1)) (define x2(car liste2)) (define y2(cadr liste2)) 
                   (if (and (eq? x1 x2)  (eq? (+ y1 1) y2)) #t (if (and (eq? x1 x2)  (eq? (- y1  1) y2)) #t(if (and (eq? y1 y2)  (eq? (+ x1 1) x2)) #t(if (and (eq? y1 y2)  (eq? (- x1  1) x2)) #t
                   (if (and (eq? (- x1 1) x2)  (eq? (+ y1  1) y2)) #t
                   (if (and (eq? (- x1 1) x2)  (eq? (- y1 1) y2)) #t(if (and (eq? (+ x1 1) x2)  (eq? (- y1 1) y2)) #t(if (and (eq? (+ x1 1) x2)  (eq? (+ y1 1) y2)) #t(if (and (eq? x1 x2) (eq? y1 y2)) #t #f)))))))))))

(define ADJACENT-WITH-LIST (lambda (kordinat list1) (if (or (null? list1) (null? kordinat)) #f ( if(ADJACENT kordinat (car list1))  #t (ADJACENT-WITH-LIST kordinat (cdr list1)))))) ;tent

(define NEIGHBOR-LIST (lambda (list1)(define x1 (car list1)) (define y1 (cadr list1))(define liste2 '())(cons(list (- x1 1) y1)  (cons (list (+ x1 1) y1)  (cons (list x1 (- y1 1)) (cons (list x1 (+ y1 1)) liste2)))))) ;tree

(define (append list1 list2) (if (null? list1) list2(cons (car list1) (append (cdr list1) list2))))

(define n-th (lambda (liste n) (if (eq? 1 n) (car liste) (n-th (cdr liste) (- n 1)))))

(define (insert-n list item n) (if (= n 0) (cons item list) (cons (car list) (insert-n (cdr list) item (- n 1)))))

(define (ele liste1 table_size) (cond
                                         ((null? liste1) '())
                                         ((or(> (car(car liste1)) (car table_size))(> (cadr(car liste1)) (cadr table_size)) )  (ele (cdr liste1) table_size))
                                         (else (cons (car liste1) (ele (cdr liste1) table_size)))))

(define eliminate_neighbor_with_tree_and_tent (lambda (kordinat tree_list tent_list table_size) (eliminate_duplicate(eliminate_duplicate(ele (NEIGHBOR-LIST kordinat) table_size) tree_list ) tent_list)))

(define (eliminate_duplicate komsu_liste_in_table tree_list) (cond
                                                 ((null? komsu_liste_in_table) '())
                                                 ((if (member (car komsu_liste_in_table) tree_list) (eliminate_duplicate (cdr komsu_liste_in_table) tree_list) (cons (car komsu_liste_in_table)
                                                                                                                                                                     (eliminate_duplicate (cdr komsu_liste_in_table) tree_list)) ))))
(define same(lambda (item tree_list) (if (null? tree_list) #f (if(eq? item (car tree_list)) #t (same item (cdr tree_list))))))
  
(define (deleteitem list1 item) 
( cond
    ((null? list1) '())
    ((equal? (car list1) item) (deleteitem (cdr list1) item)) 
    (else (cons (car list1) (deleteitem (cdr list1) item)))
))
(define (length lst)
    (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))])) 
