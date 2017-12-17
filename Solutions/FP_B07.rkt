#lang racket

;Aufgabenblatt 7
;Übungsgruppenleiter: Benjamin Seppke
;Gruppe: Mi 10-12 Uhr
; Jan Ouborny, Ozan Boga, Elaha Khaleqi


;Aufgabe 1 

; rekursive Funktion, die das Vorkommen eines Elements n in einer Liste xs zählt 
(define (zaehlen n xs)
  (if (null? xs) 0
      (if (equal? n (car xs))
          (+ 1 (zaehlen n (cdr xs)))
          (zaehlen n (cdr xs)))))


; endrekursive Funktion, die das Vorkommen eines Elements n in einer Liste xs zählt
; arbeitet mit einem Zähler zaehler, der zu Beginn auf 0 gesetzt ist und je nach vorkommen des Elements hochgezählt wird
(define (zaehlen1 n xs zaehler)
(cond ((null? xs) zaehler)
      ((equal? n (car xs))
       (zaehlen1 n (cdr xs) add1 zaehler))
      (else (zaehlen1 n xs (cdr xs)(zaehler)))))


; Funktion höherer Ordnung, die das Vorkommen eines Elements n in einer Liste xs zählt
(define (filter y xs)
  (length(filter (lambda (x) (= x y)) xs)))


