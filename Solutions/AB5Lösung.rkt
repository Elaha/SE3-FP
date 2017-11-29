#lang racket


;Aufgabe 1

; Musterung
; Strene > Punkte > Streifen
 

; Flügelfarbe
; Blau > grün > gelb > rot


; Fühlerform
; gekrümmt > Geschweift > gerade


; Flügelform
; elliptisch > rhombisch > hexagonal



(require se3-bib/butterfly-module)


; 1.1 Analyse & Grobentwurf

; 1) Datenstruktur Merkmale

; Im Rahmen der Aufgabe, eignet sich eine Liste als Datenstruktur am besten.
; Hierbei kann für jede Eigentschaft (Musterung, Flügelfarbe, Fühlerform, Flügelform) eine
; eigene Unterliste in der Liste erstellt werden.
; Hierbei könnte man die Unterlisten in einer bestimmten Rheinfolge anordnen.
; Es bietet sich an sie von dominant -> reszesiv zu ordnen.
; Denn jetzt könnte man durch verschiedene Operationen wie Beispielsweise car oder cdr
; durch die Liste navigieren und auf bestimmte Elemente zugreifen.
(define fluegelform 3)
(define fuellerform 2)
(define farbe 1)
(define musterung 0)

(define sterne 0)
(define punkte 0)
(define streifen 0)
(define gruen 1)
(define rot 1)
(define gelb 1)
(define blau 1)
(define gekruemmt 2)
(define geschweift 2)
(define gerade 2)
(define ellipitisch 3)
(define rhombisch 3)
(define hexagonal 3)


(define merkmale
   (list 
    (list 'sterne 'punkte 'streifen)           ; Musterung
    (list 'blau 'gruen 'gelb 'rot)              ; Farbe
    (list 'gekruemmt 'geschweift 'gerade)       ; Fühlerform
    (list 'elliptisch 'rhombisch 'hexagonal)))   ; Flügelform

; 2) Datenstruktur Schmetterling

; Für die Darstellung des Schmetterlings, bietet sich wieder eine Liste an.
; Die Liste sollte der Sortierung (Musterung, Farbe, Fühlerform, Flügelform),
; der obrigen Liste "Merkmale" entsprechen.
; Wobei wir hier nur 4 konkrete Elemente Übergeben (eins für jede Eigenschaft).
; Mit der Funktion show-butterfly können wir uns dann diesen Schmetterling anzeigen lassen.

; (define Schmetterling
;   (List Musterung, Farbe, Fühlerform, Flügelform)

; 3) Gliederung

; Diese Gliederung bezieht sich auf die Teilaufgaben in Aufgabe 1.2.
; 1)
; Für die erste Teilaufgabe muss jedes Elment was eine geringere Mächtigkeit hat, Ausgegeben werden.
; -> Eingabe: Merkmal

; 2)
; Herausfinden welche der beiden Merkmale einen höheren Stellenwert hat
; -> Eingabe: Merkmal 1 , Merkmal 2

; 3)
; Die Eigentliche Funktion die ein Schmetterling erzeugt, sollte hier nur die verschiedenen
; Merkmale entgegennehmen und sie auf die Schmetterling - Liste mappen.
; Zusätzlich sollte eine Hilfsmethode definiert werden, die zuällige Elemente aus der Merkmale-Liste
; wählt, die nicht dominant sind.
; -> Eingabe für Schmetterling erzeugen: dominant1, dominant2
; -> Eingabe für Hilfsmethode: Liste (Merkmale)

; 4)
; Rückgabe der erzeugten Liste, durch übergabe der Funktion Schmetterling erzeugen.
; -> Eingabe: Funktion --> schmetterlingErzeugen

; 5)
; Durch Funktionsaufruf show-butterfly
; -> Eingabe: Schmetterling (Liste)



; Aufgabe 1.2

; Findet alle rezessiven Merkmale zu einem gegebenen Merkmal
(define (finde-rezessive-m m)
 (cdr (member m (get-m-type-list m))))

(define (get-m-type-list m) (car(memf
                                 (lambda (arg) (member m arg))
                                 merkmale)))
; Gibt true zurück wenn m1 dominanter ist, sonst false. Wenn beide gleich sind dann auch true.
(define (vergleiche-dominanz m1 m2) (let ([types (get-m-type-list m1)])
                                      (<= (index-of types m1) (index-of types m2))))
(get-m-type-list 'rot)
(finde-rezessive-m 'gekruemmt)
(vergleiche-dominanz 'rot 'rot)

; Hilfsmethode: liefert zufälliges Element zurück
(define (randomElement liste)
  (list-ref liste (random (length liste))))

