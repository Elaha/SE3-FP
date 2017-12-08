#lang racket


; Aufgabe 1



; Endrekursion: Wenn Funktion nur aus einem elementaren Fall & einer rekusiven
;               Verwendung der Funktion besteht.
;               Hinter dem rekursivon Aufruf steht keine weitere Funktion.
;               Nach der Rekursion wird mit dem Ergebnis keine weitere Berechnung durchgeführt.
;                 

; Lineare rekusion: Eine Funktion die sich in jeder Fallunterscheidung selbst nur
;                   einmal verwendet.
;                   Also im else wird die Rekursion aufgerufen, aber danach kann noch
;                   etwas mit dem Ergebnis der Rekursion geschehen. Also es kann eine
;                   eine weitere Funktion hinter der Rekursion stehen.


; indirekte Rekursion: wenn 2 oder meherere Funktionen sich wechselseitig rekursiv verwenden.

; Baumartige Rekurison: Wenn in der Definition in einer Fallunterscheidung mehrfach
;                       auf die Definition bezug genommen wird.


; Aufgabe 1.1 

; 1) #take:
; -> lineare Rekursion: Denn die Rekursion verwendet sich selber nur ein Mal.
;                       Nach dem rekursiven Aufruf, werden (durch cons) mit dem Ergebnis der Rekursion,
;                       noch weitere Berechnungen durchgeführt.

; -> direkte Rekursion: Denn es wird mit keiner anderen Funktion, ein wechselseitiger Aufruf
;                       durchgeführt (indirekte Rekursion).


; 2) #drop:
; -> endrekursiv: Denn hier wird nach dem rekursiven Aufruf keine weitere Berechnung/Operation mehr
;                 mit dem Ergebnis durchgeführt.

; -> direkte Rekursion: Denn auch hier wird mit keiner anderen Funktion, ein wechselseitiger Aufruf
;                       durchgeführt.

; 3) #merge
; -> Lineare Rekursion: Durch die unterinanderreihung der rekursiven Aufrufe scheint es so
;                       als wäre es eine Baumrekursion, jedoch wird nach dem if für jede Fallunterscheidung,
;                       lediglich nur ein rekursiver Aufruf definiert.
;                       Ein weiterer Grund für die lineare Rekursion ist hierbei auch,
;                       dass nach der Rekursion, mit dem Ergebnis noch weiter gearbeitet wird.

; -> direkte Rekursion: Hier existiert wieder kein wechselseitiger Aufruf mit anderen Funktionen.
;  

; 4) #merge-sort
; -> Baumrekurison: Denn in diesem Fall, ruft sich merge-sort innerhalb einer Fallunterscheidung,
;                   zwei Mal selber auf.

; -> direkte Rekursion: Hier werden zwar andere rekursive Funktionen wie "take", "drop" oder "merge" aufgerufen,
;                       jedoch rufen diese merge-sort nicht wieder auf.
;                       Aus diesem Grund besteht kein wechselseitiger, sondern nur ein einseitiger Aufruf der Funktionen und somit
;                       auch keine indirekte Rekursion.


; Aufgabe 1.2
; Funktionen höherer Ordnung sind Funktionen, die andere Funktionen als Argument entgegennehmen
; oder als Ergebnis zurück liefern.

; Diese Eigenschaften, sind bei den Funktionen merge  und  merge-sort gegeben.
; Denn sie nehmen beide die Funktion rel<? entgegen, die eine Ordnungsrelation definiert,
; nach der die Listen sortiert werden sollen.

;Aufgabe 1.3

;Original take-Funktion
(define (take n xs) ;; das Kopfstueck einer Liste: die ersten n Elemente
  (cond ((null? xs) '()) ((= 0 n) '()) (else (cons (car xs) (take (- n 1) (cdr xs))))))


;endrekursive take-Funktion
;die Funktion arbeitet mit einem Akkumulator, der alle Ergebnisse zwischenspeichert 
;die Funktion take arbeitet mit einer Hilfsfunktion take-helper, die den Akkumulator enthält 
(define (take1 n xs)
    (letrec
      ([take-helper (lambda (n xs acc)
                    (cond
                      ((null? xs) acc)
                      ((= 0 n) acc)
                    (else  (cons (car xs) (take-helper (- n 1) (cdr xs)acc)))))])
                                                       
      (take-helper n xs '())))








   