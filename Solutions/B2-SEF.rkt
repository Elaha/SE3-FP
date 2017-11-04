#lang racket
; SE-Funktional Blatt-1
; Deniz Gül 6818556
; Jan Ouborny 7047561
; Ozan Boga 6689640

; Aufgabe 1
(define wuff 'Flocki)
(define Hund wuff)
(define Wolf 'wuff)

(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Zaphod)
        (PersonC PersonA))
    PersonC))

(define xs1 '(0 2 3 wuff Hund))
(define xs2 (list wuff Hund))
(define xs3 (cons Hund wuff))

; 1.  wuff             -> 'Flocki  Die Auswertung von wuff gibt 'Flocki zurück, da wuff als Abbildung auf 'Flocki definiert wurde.
; 2.  Hund             -> 'Flocki  Hund wird auf wuff abgebildet, was wiederum auf 'Flocki abgebildet wird.
; 3.  Wolf             -> 'wuff    Nach Definition ist Wolf eine Abbildung auf 'wuff.
; 4.  (quote Hund)     -> 'Hund    Das Apostroph vor Hund ist eine Abkürzung für das quote.
; 5.  (eval Wolf)      -> 'Flocki  Wolf wird auf 'wuff abgebildet. Das Apostroph bei 'wuff "blockiert die Evaluierung und gibt ihr Argument wörtlich zurück".
;                                  In diesem Fall ist das Argument wuff, das auf 'Flocki abgebildet wird.
; 6.  (eval Hund)      ->          Fehlermeldung da Flocki undefiniert ist und nicht ausgewertet werden kann.
; 7.  (eval 'Wolf)     -> 'wuff    Das Argument Wolf wird ausgewertet, das auf 'wuff abgebildet wird.
;
; 8.  (welcherNameGiltWo 'lily 'potter) -> 'lily PersonC wird lokal auf PersonA abgebildet, das als Eingabe 'lily entgegen nahm, weshalb 'lily ausgegeben wird.
;
; 9.  (cdddr xs1)      -> '(wuff Hund) Die list (0 2 3 wuff Hund) kann als pair von pair's gesehen werden. Drei mal wird das jeweils zweite pair genommen, was '(wuff Hund) entspricht.
; 10. (cdr xs2)        -> '(Flocki)    Gibt die Teilliste '(Hund) wieder. '(Hund) ausgewertet ergibt '(Flocki).
; 11. (cdr xs3)        -> 'Flocki      In diesem Fall liegt ein pair vor, bei dem der Körper nicht als Liste angesehen wird. Die Auswertung vom zweiten Element ergibt 'Flocki.
; 12. (sqrt 1/4)       -> 1/2          Die Funktion sqrt die rationale Zahl 1/4 entgegen und berechnet dessen Wurzel.
;
; 13. (eval '(welcherNameGiltWo 'Wolf 'Hund))  -> 'Wolf Entspricht der Auswertung des Ausdrucks (welcherNameGiltWo 'Wolf 'Hund).
;
; 14. (eval (welcherNameGiltWo 'Wolf 'Hund))   -> 'wuff Die Auswertung von (welcherNameGiltWo 'Wolf 'Hund) ergibt 'Wolf. Durch eval wird nochmal Wolf ausgewertet, was 'wuff ergibt.

; Aufgabe 2.1
; Berechnet die Fakultät einer Zahl.
; zahl ist ein Element der natürlichen Zahlen (inkl. 0).
(define (fakultät zahl)
  (if (= zahl 0)
       1
      (* (fakultät (- zahl 1)) zahl)))

; Aufgabe 2.2
; Berechnet die Potenz r hoch n.
; r ist ein Element der rationalen Zahlen.
; n ist ein Element der natürlichen Zahlen (inkl. 0).
(define (power r n)
  (if (= n 0)
      1
      (if (even? n)
          (sqr (power r (/ n
                           2)))
          (* (power r (- n 1)) r))))

; Aufgabe 2.3
; Dient als rekursive Hilfsfunktion für die Funktion 'euler'.
; Der Aufruf über 'euler' sollte mit 2 beginnen, um mit dem zweiten Glied der Reihe zu beginnen.
(define (euler-help e)
  (let ([x (/ e
              (fakultät (- e 1)))])
    (if (< x (/ 1
              (power 10 1000)))
        0
        (+ x (euler-help (+ e 1))))))

; Berechnet die Eulerzahl e auf 1000 Stellen genau
; Benutzt die Hilfsfunktion 'euler-help' und die gegebene Reihe, um die Eulerzahl auf die 1000-ste Stelle genau zu berechnen.
(define euler
  (* (/ (+ (euler-help 2) 1)
        2)(power 10 1001)))

; Aufgabe 2.4
; Dient als rekursive Hilfsfunktion für die Funktion 'pi2'.
; p sollte gleich 1 sein, um mit dem zweiten Glied der Formel zu beginnen.
; i entspricht einer Zählvariable und sollte zu Anfang gleich 0 sein. Zudem entscheidet es über die Auswahl zwischen zweier Rekursionsaufrufe.
; n ist eine natürliche Zahl und die Abbruchbedingung der Rekursion.
(define (pi-help p i n)
  (if (= i n)
      0
      (if (even? i)
          (- (/ 1
                p) (pi-help (+ p 2) (+ i 1) n))
          (+ (/ 1
                p) (pi-help (+ p 2) (+ i 1) n)))))

; Aufgabe 2.4
; Berechnet die Zahl pi nach der Formel von Gregory und Leibnitz auf eine einstellbare Genauigkeit.
; n ist das letzte Glied der Formel von Gregory und Leibnitz, das die Abbruchbedingung der rekursiven Hilfsfunktion 'pi-helper' darstellt.
(define (pi2 n)
  (* 4 (pi-help 1 0 n)))

; Aufgabe 3
; Gibt den Typ eines eingegebenen Ausdrucks wieder.
; ausd entspricht der Eingabe, dessen Typ ermittelt werden soll.
(define (type-of ausd)
  (cond [(boolean? ausd) "boolean"]
        [(pair? ausd) "pair"]
        [(list? ausd) "list"]
        [(symbol? ausd) "symbol"]
        [(number? ausd) "number"]
        [(char? ausd) "char"]
        [(string? ausd) "string"]
        [(vector? ausd) "vector"]
        [(procedure? ausd) "procedure"]))

;(type-of (* 2 3 4 ))            -> number, da das Ergebnis des Ausdrucks (* 2 3 4 ) eine Zahl ist.
;(type-of (not 42))              -> boolean, denn jedes Objekt außer #f wird zu #t ausgewertet und (not #t) wird dann zu #f abgebildet.
;(type-of '(eins zwei drei))     -> pair/list, (eins zwei drei) kann als list oder als pair von pair's gesehen werden.
;(type-of '())                   -> list, da () eine leere Liste ist.
;(define (id z) z)
;(type-of (id sin))              -> procedure, da id auf sin abgebildet wird und sin selbst eine Prozedur ist.
;(type-of (string-ref "SE3" 2))  -> char, da (string-ref "SE3" 2) den char '3' zurück gibt.
;(type-of (lambda (x) x))        -> procedure, da (lambda (x) x) eine anonyme Funktion/Prozedur ist.
;(type-of type-of)               -> procedure, denn type-of selbst ist gerade eine Prozedur.
;(type-of (type-of type-of))     -> string, weil die Ausgabe der Prozedur type-of entsprechend dem Ausdruck (type-of type-of) ein String ist.