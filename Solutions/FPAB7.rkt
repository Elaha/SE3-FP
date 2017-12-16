#lang racket

; Aufgabe 1



(define xs
  '(1 2 1 3 4 2))
  
 ; die Funktionen arbeitet mit einem zusätzlichem Parameter "zaehler" 
 ;das Vorkommen eines Elements in der Liste wird gezählt und der zaehler wird dann jeweils um einen hochgezählt
 ; zu Beginn hat der zaehler den Inhalt 0 
(define (zaehlen n xs zaehler)
(cond ((null? xs) zaehler)
      ((equal? n (car xs))
       (zaehlen n (cdr xs) add1 zaehler))
      (else (zaehlen n xs (cdr xs)(zaehler)))))
  
  (zaehlen 2 '(1 2 1 3 4 2) 0)
  
; Ansatz für rekursive Funktion
; TODO: länge muss von der Endliste berechnet werden (länge der Endliste = Anzahl des gegebenen Elements, in der Liste) 
(define (zählenrek y xs)
(cond ((null? xs) '())
      ((not(equal? y (car xs))) (zählenrek y (cdr xs)))
      (else (cons (car xs) (zählenrek y (cdr xs))))))
         

; Dargestellt als Funktion höherer Ordnung
(define (filterer y xs)
  (length(filter (lambda (x) (= x y)) xs)))

; Aufgabe 2

(require racket/trace)
; Creates a list with lists for each row
(define (create-field-v1 cells [field '()]) 
  (if (>= cells 0)
      (create-field-v1 (- cells 1) (cons #f field) )
      field))
; Create a list of booleans
(define (create-field-v2 rows columns [field '()])
  (letrec ([create-row (lambda (row length)
                        (if (> length 0)
                            (create-row (cons #f row) (- length 1))
                            row))])
    (if (> rows 0)
        (create-field-v2 (- rows 1) columns (cons (create-row '() columns) field))
        field)))

(define default-field-v1 (create-field-v1 300))
(define default-field-v2 (create-field-v2 3 4))

; zero based x,y coordinates
(define (get-neighbours-v1 x y field)
  (let ([posmin (if (> y 0)
                    (+ x (* (- y 1) 30))
                    (+ x (* y 30)))] ; Rand des Spielfelds erreicht
        [posmax (if (< y 29)
                    (+ x (* (+ y 1) 30))
                    (+ x (* y 30)))] ; Rand des Spielfelds erreicht
        [posignore (+ x (* y 30))])
  (list posmin posmax posignore)))
(get-neighbours-v1 0 5 default-field-v1)
