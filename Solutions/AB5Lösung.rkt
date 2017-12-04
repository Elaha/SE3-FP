#lang racket

; Jan Ouborny 7047561
; Ozan Boga 6689640
; Elaha Khaleqi 6947801

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

(define musterung 0)
(define farbe 1)
(define fuellerform 2)
(define fluegelform 3)


; 1.1 Analyse & Grobentwurf

; 1) Datenstruktur Merkmale

; Im Rahmen der Aufgabe, eignet sich eine Liste als Datenstruktur am besten.
; Denn es kann für jede Eigentschaft (Musterung, Flügelfarbe, Fühlerform, Flügelform) eine
; eigene Unterliste in der Liste erstellt werden.
; Hierbei könnte man die Unterlisten in einer bestimmten Rheinfolge anordnen.
; Es bietet sich an sie von dominant -> reszesiv zu ordnen.
; Denn jetzt könnte man durch verschiedene Operationen wie Beispielsweise car oder cdr
; durch die Liste navigieren und auf bestimmte Elemente zugreifen.
; Gleichzeitig sind vordefinierte Operationen wie "string-ref" oder "remove" die auf Listen
; angewant werden können, für die folgenden Aufgaben wichtig.


(define merkmale
   (list 
    (list 'star 'dots 'stripes)           ; Musterung
    (list 'blue 'green 'yellow 'red)      ; Farbe
    (list 'curved 'curly 'straight)       ; Fühlerform
    (list 'ellipse 'rhomb 'hexagon)))     ; Flügelform

; 2) Datenstruktur Schmetterling

; Für die Darstellung des Schmetterlings, bietet sich wieder eine Liste an.
; Die Liste sollte der Sortierung (Musterung, Farbe, Fühlerform, Flügelform),
; der obrigen Liste "Merkmale" entsprechen.
; Wobei wir in der Liste für jede Eigenschaft, je zwei Elemente Speichern (Für dominant und rezessiv).
; Somit hätten wir wieder 4 Unterlisten, mit jeweils 2 Elmenten.
; Mit der Funktion show-butterfly, können wir uns dann ganz einfach durch den Aufruf mit car für jede Eigenschaft
; das dominante Merkmal des Schmetterlings, anzeigen lassen. 
; Die Liste sollte im späteren Verlauf der Aufgabe, durch unsere Hauptfunktion "cons-schmetterling"
; erstellt werden.

; (Musterung, Farbe, Fühlerform, Flügelform)
; Beispiel: '((blau.rot) (sterne.streifen) (gerade.gekruemmt) (hexagonal.elliptisch)) 

; 3) Gliederung

; Diese Gliederung bezieht sich auf die Teilaufgaben in Aufgabe 1.2.

; 1.

; Für die erste Teilaufgabe muss jedes Elment was eine geringere Mächtigkeit hat, ausgegeben werden.
; -> Name: finde-rezessiv-m 
; -> Eingabe: m (m stellt ein Merkmal dar, wie z.B. blau oder gerade)

; Hilfsfunktion:
; Für die obere Funktion, kann noch eine Hilfsmethode definiert werden, die für ein gegebenes Merkmal,
; die passende Unterliste findet.
; Diese kann man in unserer oberen Funktion Aufrufen. Mit cdr kann für das übergebeben Merkmal, die Restliste
; ermittelt werden, welche somit die resseziven Merkmale zu einem gegebenen m liefern würde.
; -> Name: get-m-type-list
; -> Eingabe: m (m steht wieder für Merkmal)

; 2.

; Herausfinden welche der beiden Merkmale einen höheren Stellenwert in der Liste hat bzw. welcher
; der Merkmale in der jeweiligen Unterliste, weiter links steht.
; In dieser Funktion könnten wir wieder, die obrige Hilfsfunktion aufrufen mit dessen Hilfe wir die
; Unterliste erhalten.
; Anschließend könnten so zwei Merkmale auf ihre Indexnummern verglichen werden um herauszufinden, welche
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
; -> Eingabe: farbe muster fuehler fluegel (Übergabe der Dominanten Eigenschaften)

; Hilfsfunktion:
; Hilfsfunktion für die Auswahl der zuällig ausgewählten rezessiven Merkmale.
; -> Name: random-element
; -> Eingabe: liste (Wir übergeben unsere "merkmale" Liste).

; 4.
; Rückgabe der erzeugten Liste, welche alle Dominanten und zufällig erzeugten rezessiven Gene paarweise gespeichert haben sollte.
; -> Eingabe: Liste (Die Liste die durch die Hauptfunktion "cons-schmetterling" erzeugt wird, sollte hier übergeben werden.

; 5.
; Durch die vordefinierte Funktion show-butterfly sollten wir unseren Schmetterling einfach
; Anzeigen lassen können.
; Wir sollten hierbei wie oben schon erwähnt durch car in der lage sein, show-butterfly die dominanten
; Merkmale unseres Schmetterlings zu übergeben.
; -> Name: show-butterfly
; -> Eingabe: Schmetterling (Liste)

; NACHTRAG zu Punkt 4 & 5
; Bei diesen beiden Funktionen muss ermittelt werden, ob es mehr Sinn macht eigene Funktionen für sie zu schreiben
; oder ob es besser ist, sie direkt in der Hauptfunktion "cons-schmetterling" zu implementieren. Denn bei der zweiten Varianten
; also der direkten implementation in der Hauptfunktion, es wahrscheinlich leichter ist, auf die durch "cons-schmetterling"
; erzeugte List zuzugreifen.

; 6.
; Hier macht es Sinn, 2 Listen zu übergeben und zusätzlich die Anzahl der Kinder zu bestimmen, die erzeugt werden soll.
; Zusätzlich sollte man für jede Eigenschaft (farbe, Muster, Fühler, Flügel), eine der beiden Merkamle die zur Auswahl stehen,
; zufällig wählen.
; Hierzu kann man eine Hilfsfunktion definieren, die Zwei zufällige listen (Für Vater und Mutter)
; erstellt durch die, die Kinder später erzeugt werden.
; Aus diesen beiden Listen kann dann später in der erzeugeKind Funktion, mithilfe der oben definierten Hilfsfunktion "random-element" Funktion,
; zuällig aus beiden Listen gezogen werden.
; -> Name: erzeugeKind
; -> Eingabe: liste liste n (Jeweils eine Liste für den Vater und für die Mutter, n für die Anzahl der Kinder)

; Hilfsfunktion:
; Wir können hier wieder unsere random-element Funktion von oben nehmen und etwas modifizieren.
; So dass, aus allen 4 Unterlisten von der Liste "merkmale" einmal gezogen wird.
; Dieser Vorgang könnte mittels rekusion implementiert werden.
; -> Eingabe: Liste ("merkmale"-Liste)

; 7.
; Hier können wir mithilfe von der "show-butterfly" Funktion alle Kinder anzeigen.
; Durch print können dann zusätzlich die Merkmale der jeweiligen Schmetterlinge ausgegeben werden.
; -> Diese Funktionen können in der Hauptfunktion "erzeugeKind" implementiert werden.




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


; 3) Hauptfunktion: erstellt ein Schmetterling.
; Hilfsmethode: liefert ein zuälliges Element aus einer gegebenen Liste zurück.
(define (random-element liste)
  (list-ref liste (random (length liste))))

(define (cons-schmetterling farbe muster fuehler fluegel)
  (list (cons farbe (random-element (remove farbe (get-m-type-list farbe))))
        (cons muster (random-element (remove muster (get-m-type-list muster))))
        (cons fuehler (random-element (remove fuehler (get-m-type-list fuehler))))
        (cons fluegel (random-element (remove fluegel (get-m-type-list fluegel))))))
 
; 4) Alle dominanten und rezessiven Merkmale für ein Schmetterling werden ausgegeben.
(define (gib-dominante schmetterling) (map (lambda (merkmaltyp) (car merkmaltyp)) schmetterling))
(define (gib-rezessive schmetterling) (map (lambda (merkmaltyp) (cdr merkmaltyp)) schmetterling))

; 5) Anzeigen eines Schmetterlings
(define (show-schmetterling schmetterling) (show-butterfly (car(list-ref schmetterling 0)) (car(list-ref schmetterling 1)) (car(list-ref schmetterling 2)) (car(list-ref schmetterling 3))))

(define schmetterling1 (cons-schmetterling 'red 'star 'curly 'hexagon))
(gib-dominante schmetterling1)
(gib-rezessive schmetterling1)
(show-schmetterling schmetterling1)