#lang racket

; Aufgabe 1



(define xs
  '(1 2 1 3 4 2))

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
