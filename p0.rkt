#lang racket

;;Meagan Gonzalez

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md
(define x (cons 42 (cons -1 (cons 3 (cons 1(cons 7 '()))))))

(define t (cons 'X (cons 'O (cons 'E '()))))

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 


;;checks whether elems are X O or E
(define (valid-letter? l)
  (if (null? l)
    '()
    (cond 
      [(equal? (car l) 'X) (cons (car l) (valid-letter? (cdr l)))]
      [(equal? (car l) 'O) (cons (car l) (valid-letter? (cdr l)))]
      [(equal? (car l) 'E) (cons (car l) (valid-letter? (cdr l)))]
      [else (valid-letter? (cdr l))])))

  ;;    (lambda (lst) (andmap (lambda (e) (equal? e 'X)) lst))lsts))


 (define (square-length lst) ;;check whether a list length is a square of an int
    (integer? (sqrt (length lst))))
  
  
 

;;square root of length of list and check if its int
; Check whether a list is a valid board
;; and statement or helper function to check if each element is 'x' 'o' 'e'
(define (board? lst)
  (cond
    [(empty? lst) #f]
    [(equal?(square-length lst) #f)#f]
    [(not(equal? (length(valid-letter? lst))(length lst)))#f]
;;check number of x's and o's differ by only one
    [(not(<=(abs (- (count (lambda (x) (equal? 'X x)) lst)(count (lambda (x) (equal? 'O x)) lst )) )1)) #f]
    [else #t] ))
  

    

;;should check car and cdr
;;; From the board, calculate who is making a move this turn
(define (x-counter l)
  (count (lambda (x) (equal? 'X x)) l))

(define (o-counter l)
  (count (lambda (x) (equal? 'O x)) l))
        
(define (next-player board)
  (cond
    [(equal? (x-counter board) 0) 'X]
    [(equal?(- (x-counter board) (o-counter board)) 1) 'O]
    [(equal? (x-counter board) (o-counter board)) 'X]))

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)

;; grab first 3 elems and grab result of board

(define (get-dimensions board)
  (sqrt (length board)))

(define (e-per-row board)
  (quotient (length board)(exact-round(get-dimensions board))))

;;(define (num-rows (quotient (length board) elements-per-row)))
(define (get-rows board)
  (let* ((dimensions (exact-round (get-dimensions board))))
    (let ((row-start-indices (range 0 (length board) dimensions)))
      (map (lambda (start-index)
             (take (drop board start-index) dimensions))
           row-start-indices))))
   
;;(define (get-rows board)
;;  (if (empty? board)
;;    '()
;;    (cons (take board (e-per-row board))(get-rows (drop board (e-per-row board))))))

;;(define (desired-row board row)
;;  (list-ref (get-rows board) row))

;;(define (get-space board row col)
;;  (list-ref (desired-row board row) col))
  
;; (define (get-space board row col)
;;  (list-ref board (+ (* row (get-dimensions board)) col)))

(define (get-space board row col)
    (define dimensions (get-dimensions board))
    (define index (+ (* row dimensions) col))
    (list-ref board index))
;;account for n x n board
;; account for negative row and col inputs
;; account for 
(define (valid-move? board row col player)
  (cond
    [(not (list? board)) #f]
    [(not (number? row)) #f]
    [(not (number? col)) #f]
    [(not (< row (get-dimensions board))) #f]
    [(not (< col (get-dimensions board))) #f]
    [(not (or (equal? 'X player)(equal? 'O player)))#f]
    [(not (equal? (next-player board) player)) #f]
    [(< row 0) #f]
    [(< col 0) #f]
    [(equal? (get-space board row col)'E) #t]
    [else #t]))


;;helper function to replace an item at an index
(define (replace-at-index board i x)
  (cond
    [(empty? board)'()] ;;base case
    [(= i 0) (cons x (cdr board))] 
    [else (cons (car board) (replace-at-index (cdr board) (- i 1) x))]))



(define (get-position board row col)
  (+ (* row (get-dimensions board)) col))
;;; To make a move, replace the position at row col to player ('X or 'O)
;;board at:  '(E E E E X E E E E)
;;(make-move '(E E E E X E E E E) 0 0 'O)  
;; returns   '(O E E E X E E E E
(define (make-move board row col player)
  (list-set board (get-position board row col) player))
  ;;(cond
  ;;  [(not(board? board)) #f] ;;must be a board
  ;;  [(not(and (>= 0 row)(>= 0 col))) #f] ;;must be a positive row or column number
  ;;  [(not(or (equal? 'X player)(equal? 'O player))) #f];; player is 'X or 'O
   ;; [else (list-set board row col)]))
    
(define (left-diagonal board)
  (map (λ (i) (list-ref board (+ i (* i (sqrt (length board)))))) (range (sqrt (length board)))))

(define (transpose board)
  (apply append (map (λ (row) (reverse row)) (get-rows board))))

(define (diagonals board)
 (list (left-diagonal board) (left-diagonal (transpose board))))

(define (col b i)
  (define s (sqrt (length b)))
  (map (λ (r) (list-ref b (+ i (* s r)))) (range s)))

(define (is-any-list-all-X lsts)
  ;; does *any* list (called lst in each iteration) satisfy the property...
  (ormap
   ;; "are all elements equal? to 'X?"
   (λ (lst) (andmap (λ (e) (equal? e 'X)) lst))
   lsts))

(define (is-any-list-all-O lsts)
  ;; does *any* list (called lst in each iteration) satisfy the property...
  (ormap
   ;; "are all elements equal? to 'O?"
   (λ (lst) (andmap (λ (e) (equal? e 'O)) lst))
   lsts))


(define (get-cols board)
  (define d (get-dimensions board))
  (define col-indices (range d))
  (map (lambda (col-index) (col board col-index)) col-indices))
 ;; To determine whether there is a winner?
(define (winner? board)
  (cond
    [(not (board? board)) #f]
    [(equal?(is-any-list-all-X (diagonals board)) #t) 'X] ;;check diagonals
    [(equal?(is-any-list-all-O (diagonals board)) #t) 'O]
    [(equal?(is-any-list-all-X (get-rows board))  #t) 'X];;check rows
    [(equal?(is-any-list-all-O (get-rows board))  #t) 'O]
    [(equal?(is-any-list-all-X (get-cols board))  #t) 'X];;check cols
    [(equal?(is-any-list-all-O (get-cols board))  #t) 'O]
    [else #f]))




