#lang scheme

(define RETURN-FIRST-NOT-FALSE(lambda (func list)
                                (if(null? list) #F
                                   (if(func (car list))
                                      (func (car list))
                                      (RETURN-FIRST-NOT-FALSE func (cdr list))   
                                      ))            ))
(define ADJACENT(lambda (pos1 pos2)
                  (if(null? pos1) #T
                     (if(or (= (car pos1) (car pos2)) (= (car pos1) (- (car pos2) 1) )   (= (car pos1) (+ (car pos2) 1) )    )
                        (ADJACENT (cdr pos1) (cdr pos2))
                        #F
                        ))                ))
(define NEIGHBOR-LIST(lambda (pos) (list (list  (car pos) (+ (cadr pos) 1))  (list (+ (car pos) 1) (cadr pos)) (list (car pos) (- (cadr pos) 1)) (list (- (car pos) 1) (cadr pos)))      ))
(define ADJACENT-WITH-LIST(lambda (pos list)
                            (if(null? list) #F
                               (if(ADJACENT pos (car list))
                                  #T
                                  (ADJACENT-WITH-LIST pos (cdr list))
                                  ))        ))
(define REPLACE-NTH(lambda (list pos value)
                     (if(eq? pos 1)
                        (cons value (cdr list))
                        (cons (car list)  (REPLACE-NTH (cdr list) (- pos 1) value))
                        )       ))

(define LENGTH(lambda (list) (if(null? list) 0 (+ (LENGTH (cdr list)) 1))     ))
(define TREE(lambda (allTrees pos) (if(null? allTrees) #F (if(equal? (car allTrees) pos) #T (TREE (cdr allTrees) pos)))      ))
(define AT(lambda (list index) (if(eq? index 1) (car list) (AT (cdr list) (- index 1)))  ))
(define ORGANIZE(lambda (list allTrees tentsRow tentsColumn tents)
                  (if(null? list) '()
                     (if(or (= (caar list) 0) (= (caar list) (+ (LENGTH tentsRow) 1)) (= (cadar list) 0) (= (cadar list) (+ (LENGTH tentsColumn) 1))
                            (TREE allTrees (car list))
                            (ADJACENT-WITH-LIST (car list) tents)
                            (= (AT tentsRow (caar list)) 0) (= (AT tentsColumn (cadar list)) 0))
                        (ORGANIZE (cdr list) allTrees tentsRow tentsColumn tents)
                        (cons (car list) (ORGANIZE (cdr list) allTrees tentsRow tentsColumn tents))
                        ))      ))

(define HELPER-TENTS(lambda (trees allTrees tentsRow tentsColumn list tents)
                      (if(or (= (LENGTH list) 1) (TENTS (cdr trees) allTrees (REPLACE-NTH tentsRow (caar list) (- (AT tentsRow (caar list)) 1)) (REPLACE-NTH tentsColumn (cadar list) (- (AT tentsColumn (cadar list)) 1)) (cons (car list) tents)))
                         (TENTS (cdr trees) allTrees (REPLACE-NTH tentsRow (caar list) (- (AT tentsRow (caar list)) 1)) (REPLACE-NTH tentsColumn (cadar list) (- (AT tentsColumn (cadar list)) 1)) (cons (car list) tents))
                         (HELPER-TENTS trees allTrees tentsRow tentsColumn (cdr list) tents)
                         )        ))
(define TENTS(lambda (trees allTrees tentsRow tentsColumn tents)
               (if(null? trees) tents
                  (if(null? (ORGANIZE (NEIGHBOR-LIST (car trees)) allTrees tentsRow tentsColumn tents))
                     #F
                     (HELPER-TENTS trees allTrees tentsRow tentsColumn (ORGANIZE (NEIGHBOR-LIST (car trees)) allTrees tentsRow tentsColumn tents)  tents)
                     ))     ))

(define SUM(lambda (list) (if(null? list) 0 (+ (SUM (cdr list)) (car list)))   ))
(define CHECK(lambda (list number) (if(null? list) #T (if(> (car list) number) #F (CHECK (cdr list) number)))   ))

(define TENTS-SOLUTION(lambda (list)
                        (if(and (and (= (SUM (car list)) (SUM (cadr list))) (= (SUM (car list)) (LENGTH (caddr list))))
                                (CHECK (car list) (quotient (+ (LENGTH (cadr list)) 1) 2))
                                (CHECK (cadr list) (quotient (+ (LENGTH (car list)) 1) 2)))
                           (TENTS (caddr list) (caddr list) (car list) (cadr list) '())
                           #F
                           )   ))











