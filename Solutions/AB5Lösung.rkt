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
; Denn es kann für jede Eigentschaft (Musterung, Flügelfarbe, Fühlerform, Flügelform) eine
; eigene Unterliste in der Liste erstellt werden.
; Hierbei könnte man die Unterlisten in einer bestimmten Rheinfolge anordnen.
; Es bietet sich an sie von dominant -> reszesiv zu ordnen.
; Denn jetzt könnte man durch verschiedene Operationen wie Beispielsweise car oder cdr
; durch die Liste navigieren und auf bestimmte Elemente zugreifen.

(define fluegelform 3)
(define fuellerform 2)
(define farbe 1)
(define musterung 0)

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
; Wobei wir in der Liste für jede Eigenschaft, je zwei Elmente Speichern (Für dominant und rezessiv).
; Somit hätten wir wieder 4 Unterlisten, mit jeweils 2 Elmenten.
; Mit der Funktion show-butterfly, können wir uns dann ganz einfach durch den Aufruf mit Car für jede Eigenschaft
; das dominante Merkmal, des Schmetterlings anzeigen lassen. 
; Die Liste sollte im späteren Verlauf der Aufgabe, durch die Funktion "cons-schmetterling"
; erstellt werden.

; (Musterung, Farbe, Fühlerform, Flügelform)
; Beispiel: '((blau.rot) (sterne.streifen) (gerade.gekruemmt) (hexagonal.elliptisch)) 

; 3) Gliederung

; Diese Gliederung bezieht sich auf die Teilaufgaben in Aufgabe 1.2.

; 1.

; Für die erste Teilaufgabe muss jedes Elment was eine geringere Mächtigkeit hat, Ausgegeben werden.
; -> Name: finde-rezessiv-m 
; -> Eingabe: m (m stellt ein Merkmal dar wie z.B. blau oder gerade)

; Hilfsfunktion)

; Für die obere Funktion, kann noch eine Hilfsmethode definiert werden, die für ein gegebenes Merkmal,
; die passende Unterliste findet.
; Diese kann man in unserer oberen Funktion Aufrufen. Mit cdr kann für das übergebeben Merkmal, die Restliste
; ermittelt werden, welche somit die resseziven Merkmale zu einem gegebenen m liefern würde.
; -> Name: get-m-type-list
; -> Eingabe: m (m steht wieder für Merkmal)

; 2.

; Herausfinden welche der beiden Merkmale einen höheren Stellenwert in der Liste hat bzw. welcher
; der Merkmale in der jeweiligen Unterliste, weiter links steht.
; In dieser Funktion könnten wir wieder die obrige Hilfsfunktion aufrufen mit dessen Hilfe wir die
; Unterliste erhalten.
; Anschließend könnten so 2 Merkmale auf ihre Indexnummern verglichen werden um herauszufinden, welche
; der Merkmale sich weiter links in der Unterliste befindet.
; -> Name: vergleiche-dominanz
; -> Eingabe: m1 m2 (Merkmal 1 und Merkmal 2)

; 3.

; Die Eigentliche Funktion die ein Schmetterling erzeugt, sollte hier nur die verschiedenen
; Merkmale (Farbe, Muster, Fühler, Flügel) entgegennehmen und so die Liste die in Aufgabe 1.2 gewählt wurde, zusammensetzen.
; Zusätzlich sollte eine Hilfsmethode definiert werden, die zuällige Elemente aus der Merkmale-Liste
; wählt die rezessiv sind.
; Diese könnten wir dann einfach in unserer Hauptfunktion "cons-schmetterling" aufrufen.
; -> Name: cons-schmetterling
; -> Eingabe: farbe, muster, fuehler, fluegel (Übergabe der Dominanten Eigenschaften)

; Hilfsfunktion)

; Hilfsfunktion für die Auswahl der rezessiven Merkmale
; -> Name: random-element
; -> Eingabe: liste (Wir übergeben unsere "merkmale" Liste).

; 4.
; Rückgabe der erzeugten Liste, durch übergabe der Funktion Schmetterling erzeugen.
; -> Eingabe: Funktion --> schmetterlingErzeugen

; 5.
; Durch die vordefinierte Funktion show-butterfly sollten wir unseren Schmetterling einfach
; Anzeigen lassen können.
; Wir sollten hierbei wie oben schon erwähnt durch car in der lage sein, show-butterfly die dominanten
; Merkmale unseres Schmetterlings zu übergeben.
; -> Name: show-butterfly
; -> Eingabe: Schmetterling (Liste)



; Aufgabe 1.2

(require racket/trace)
(require se3-bib/butterfly-module)
; 1)
; Findet alle rezessiven Merkmale zu einem gegebenen Merkmal
(define (finde-rezessive-m m)
 (cdr (member m (get-m-type-list m))))

; Hilfsmethode: Gibt zu einem Übergebenen Symbol die Liste zurück in der sie sich befindet
(define (get-m-type-list m)
  (car (memf (lambda (arg) (member m arg)) merkmale)))


; 2)
; Gibt true zurück wenn m1 dominanter ist, sonst false. Wenn beide gleich sind dann auch true.
(define (vergleiche-dominanz m1 m2)
  (let ([types (get-m-type-list m1)])
  (<= (index-of types m1) (index-of types m2))))


; 3) 
; Hilfsmethode: liefert zufälliges Element zurück.
; Wobei zunächst eine Unterliste ausgewählt wird, die anschließend gemischt wird.
; Abschließen zieht man das erste Element der gemischten liste.
(define (random-element liste)
  (list-ref liste (random (length liste))))

(define (cons-schmetterling farbe muster fuehler fluegel)
  (list (cons farbe (random-element (remove farbe (get-m-type-list farbe))))
        (cons muster (random-element (remove muster (get-m-type-list muster))))
        (cons fuehler (random-element (remove fuehler (get-m-type-list fuehler))))
        (cons fluegel (random-element (remove fluegel (get-m-type-list fluegel))))))

(cons-schmetterling 'red 'star 'curly 'hexagon)
