(defun .start () (load "project3"))
; 1. List Functions
; 1.1 .LENGTH - Counts the number of elements in a list
(defun .LENGTH (L)
    (if (null L) 0
    (+ 1 (.LENGTH (cdr L)))))

; 1.2. .REMOVE-ALL - Removes all occurrences of an element X from a list L
(defun .REMOVE-ALL (X L)
    (cond ((null L) nil)
        ((equalp (car L) X) (.REMOVE-ALL X (cdr L)))
        (t (cons (car L) (.REMOVE-ALL X (cdr L))))))

; 1.3. .MAP - Apply a function to each element of a list
(defun .MAP (F L)
    (if (null L) nil
      (cons (funcall F (car L))
      (.MAP F (cdr L)))))
; Defining .ADD3
(defun .ADD3 (X)
 	(+ X 3))

; 1.4. .MERGE - Merge two sorted lists into one sorted list
(defun .MERGE (L1 L2)
    (cond ((null L1) L2)
        ((null L2) L1) 
        ((<= (car L1) (car L2)) 
            (cons (car L1) (.MERGE (cdr L1) L2))) 
        (t (cons (car L2) (.MERGE L1 (cdr L2)))))) 

; 2. Set Functions
; 2.1 .ELEMENT-OF - Chekc if an element is in a set
(defun .ELEMENT-OF (X S)
    (cond ((null S) nil) 
        ((equalp X (car S)) t) 
        (t (.ELEMENT-OF X (cdr S)))))


; 2.2 .INSERT - Insert an element into a set
(defun .INSERT (S X)
    (if (.ELEMENT-OF X S) S (cons X S)))

; 2.3 .DIFFERENCE - Compute the difference between two sets
(defun .DIFFERENCE (S1 S2)
    (cond ((null S1) nil) 
        ((.ELEMENT-OF (car S1) S2) (.DIFFERENCE (cdr S1) S2))
        (t (cons (car S1) (.DIFFERENCE (cdr S1) S2)))))

; 2.4 .SUPRSETEQ - Check if S1 is a superset of or equal to S2
(defun .SUPERSETEQ (S1 S2)
    (if (null S2) 
      t  
      (and (.ELEMENT-OF (car S2) S1) (.SUPERSETEQ S1 (cdr S2)))))

; 3. Math Functions
; 3.1 .FACTORIAL - Compute the factorial of a positive integer N
(defun .FACTORIAL (N)
  (if (< N 0)
      (error "Factorial is undefined for negative numbers.")
      (if (= N 0) 1 (* N (.FACTORIAL (- N 1))))))

; 3.2 .RIGHT-TRI - Check if three positive integers form a right triangle
(defun .RIGHT-TRI (A B C)
  (and 
    (and (> (+ A B) C) (> (+ B C) A) (> (+ A C) B))
    (= (+ (* A A) (* B B)) (* C C))
  )
)  

; 3.3 .NTH-FIBO - Return the N-th Fibonacci number
(defun .NTH-FIBO (N)
    (cond 
        ((= N 0) 0)         
        ((= N 1) 1)
        (t (+ (.NTH-FIBO (- N 1)) (.NTH-FIBO (- N 2))))))

; 3.4 .POW - Computed X raised to the power Y(X^Y)
(defun .POW (X Y)
  (if (= Y 0) ; Base case
    1
    (if(> Y 0)
    (* X (.POW X (- Y 1)))
    (/ 1 (.POW X (- Y))) 
    )   
  )
)

; Test List Functions
;(print (.LENGTH '(1 2 3 4)))                    ; Expected: 4
;(print (.LENGTH ()))		                ; Expected: 0

;(print (.REMOVE-ALL 'A '(B A C A A D A)))       ; Expected: (B C D)
;(print (.REMOVE-ALL 'A '(X Y Z)))	        ; Expected: (X Y Z)
;(print (.REMOVE-ALL 'A '()))		        ; Expected: NIL

;(print (.MAP '.ADD3 '(1 2 3 4))) 		; Expected: (4 5 6 7)
;(print (.MAP '(LAMBDA (X) (* 2 x)) '(1 2 3 4))) ; Expected: (2 4 6 8)

;(print (.MERGE '(1 3 4 7) '(2 3 6)))            ; Expected: (1 2 3 3 4 6 7)

; Test Set Functions
;(print (.ELEMENT-OF 3 '(1 3 X A)))              ; Expected: T
;(print (.ELEMENT-OF 2 '(1 3 X A)))              ; Expected: NIL
;(print (.ELEMENT-OF 2 '()))                     ; Expected: NIL

;(print (.INSERT '(B C D) 'A))                   ; Expected: (A B C D)
;(print (.INSERT '(A B C D) 'A))                 ; Expected: (A B C D)

;(print (.DIFFERENCE '(A B C) '(A C D)))         ; Expected: (B)
;(print (.DIFFERENCE '(A C D) '(A B C)))         ; Expected: (D)

;(print (.SUPERSETEQ '(A B C D) '(A B)))         ; Expected: T
;(print (.SUPERSETEQ '(A B) '(A B)))             ; Expected: T
;(print (.SUPERSETEQ '() '(A B)))                ; Expected: NIL

; Test Math Functions
;(print (.FACTORIAL 5))                          ; Expected: 120

;(print (.RIGHT-TRI 3 4 5))                      ; Expected: T
;(print (.RIGHT-TRI 1 2 3))                      ; Expected: NIL
;(print (.RIGHT-TRI 5 4 3))                      ; Expected: NIL

;(print (.NTH-FIBO 6))                           ; Expected: 8
;(print (.NTH-FIBO 10))                          ; Expected: 55

;(print (.POW 2 8))                              ; Expected: 256
;(print (.POW 2 0))                              ; Expected: 1
;(print (.POW 2 -2))                             ; Expected: 1/4
