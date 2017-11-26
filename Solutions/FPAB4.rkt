#lang racket

; Aufgabe 1

; 1. (max (min 5(- 87))6)                                  --> 6 = Als erstes wird das min ausgewertet wo der kleinste Wert die -87 ist.
;                                                                  Anschließend wird das max ausgewertet, wo der größte Wert, die 6 ist.

; 2. '(+ ,(- 13 11) 17)                                    --> '(+ ,(- 13 11) 17) = Durch das quote Zeichen, werden alle Elemente so ausgegeben wie sie dort stehen.

; 3. (cadr '(Good King Wenceslas))                         --> 'King = Denn, durch das car in cadr, wird dass "Good" also der fordere Teil abgeschnitten.
;                                                                      Durch das cdr in cadr, wird dass "Wenceslas" also der hintere Teil abgeschnitten.
;                                                                      Also bleibt der mittlere Teil 'King übrig.

; 4. (cdddr '(looked out (On the feast of Steven)))        --> ()' = 3 Elemente, (Wobei die Liste in der Liste, ein einzelnes Element darstellt) werden abgeschnitten,
;                                                                    was übrig bleibt, ist die leere Liste.

; 5. (cons 'When '(the show lay round about))              --> '(When the show lay round about) = Durch das cons, wird das Element "When" mit der vorhandenen List verbunden. 

; 6. (cons '(Deep and) 'crisp)                             --> '((Deep and) . crisp) = Aufgrundessen, dass das Symbol hier an 2. Stelle also hinter der Liste aufgeführt wird,
;                                                                                      erstellt cons explizit ein Paar aus den beiden Komponenten.

; 7. (equal? (list 'and 'even) '(and even))                --> true = Denn der Wert der in diesen Listen gespeichert wird, ist der selbe.

; 8. (eq? (list 'Rudolph 'the 'red-nosed 'reindeer)
;         (cons 'Rudolph  '(the 'red-nosed 'reindeer)))    --> false = Denn die Funktion eq? liefert nur true zurück, wenn zwei Objekte auf die selbe Referenz verweisen.
;                                                                      Bei zwei von einander unabhängig (und auf verschiedene Weise) erstellten listen, besteht jedoch keine Referenzgleichheit.



; Aufgabe 2.1
; Erweiterte Backus-Naur-Form:

; Überschrift       = 3 * Notzeichen, ("DELTA ECHO" | "HIER IST" )
;                            3 * Schiffsname, Rufzeichen, Notzeichen, Schiffsname, SchiffsnameBst, Rufzeichen;
; Standortangabe    = Satz;
; NotfallArt        = Satz;
; Hilfeleistung     = Satz;
; Peilzeichen       = "-";
; Unterschrift      = Schiffsname Rufzeichen;

; Notzeichen        = "MAYDAY";
; Schiffsname       = Wort;
; SchiffsnameBst    = Buchstabiert;
; Rufzeichen        = 4 * Codewort;



; Satz              = Wort Satz;
; Wort              = ( Ziffer | Buchstabe ), { (Ziffer | Buchstabe ) };
; Buchstabiert      = Codewort Buchstabiert;              
; Buchstabe         = "a" | "b" | "c" | ... | "z" | "A" | "B" | "C" | ... | "Z";
; Ziffer              = "0" | "1" | "2" |... | "8" | "9"; 
; Codewort          = "Alpha" | "Bravo" |" Chrarlie" | ... | "Novonine" | "Decimal" | "Stop";

; Notmeldung          = Überschrift, Standortangabe, NotfallArt, Hilfeleistung, 2 * Pfeilzeichen, Unterschrift, "OVER";
 
;Aufgabe 2.2
(require "AB3FINAL.rkt")
(define (Notmeldung notzeichen schiffsname rufzeichen position ArtDesNotfalls Hilfeleistung)
  (Ueberschrift notzeichen schiffsname rufzeichen))




(define (Ueberschrift notzeichen schiffsname rufzeichen)
  (string-append notzeichen notzeichen notzeichen
                 "\nDELTA ECHO \n"
                 schiffsname schiffsname schiffsname
                 (text->codeword rufzeichen)
                 "\n"
                 notzeichen schiffsname
                 (text->codeword (schiffsname)
                                 "\n"
                                 text->codeword rufzeichen)))



















; Bezugstransparenz = Ausdrücke die den selben Wert speichern können durch einander ersetzt werden.
; innere Reduktion ist wenn die Berechnung mit den Übergebenen Werten beginnt.
; äußere Reduktion ist wenn der Body zuerst ausgefürt wird und man für

; Aufgabe 3.1

(define (hoch3 x) (* x x x)) (hoch3 ( + 3 (hoch3 3)))

; innere Reduktion:   (Argumentauswertung --> Funtionsdefinition)
; Terme werden von innen nach außen reduziert.
 (hoch3 ( + 3 (hoch3 3)))
; --> (hoch3 30) //Hierbei wird als erstes der Wert der Übergeben wird ausgerechnet.
; --> (* 30 30 30) //Anschließend wird in den Rumpf gegangen und das Ergebnis wird ermittelt.
; --> 27000


; äußere Reduktion: (Funktionsdefinition --> Argumentauswertung)
; Terme werden von außen nach innen reduziert.
; Auswertung immererst dann wenn es nicht mehr anders geht)
 (hoch3 ( + 3 (hoch3 3)))
; --> (* (+ 3 (hoch3 3)) (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
; --> (* 30 (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
; --> (* 30 30 (+ 3 (hoch3 3)))
; --> (* 30 30 30)
; --> 27000

; Aufgabe 3.2
; Für Racket wird die innere Reduktion angewendet.
; Bei Spezialformen wie z.B. if wird von der festgelegten Rheienfolge abgewichen
; und die äußere Reduktion angewant.

; Aufgabe 3.3

; Bei einer Spezialform, wird für diese bestimmten Operationen eine gewisser Algorithmus festgelegt.
; Dieser definiert für die Spezialformen, dass zuerst die Funktionsdefinition betrachtet wird und die
; Auswertung der Argumente verschoben wird, bzw. erst dann geschieht wenn es keine andere Option mehr gibt (lazy evaluation).
; Dies ist Beispielsweise besonders wichtig bei if-else Operationen, wo als erstes die Bedingung getestet
; werden sollte, bevor die Argumente ausgewertet werden.
; Wird, wie in der Aufgabe 3.3 eine selbst definierte Funktion "faculty" für das if eingeführt, greift der Algorithmus
; der Spezialformen für die Funktion "faculty" nicht mehr.
; Demnach wird diese Funktion wie jede andere Funktion auch "normal" ausgewertet.
; Bei dieser Form der Auswertung, garantiert Racket nicht, dass zuerst die Bedingung des if geprüft wird.
; Hier ist es so, dass bevor die Bedingung betrachtet wird, die Argumente schon vorher ausgewertet vorliegen.
; Wenn bei faculty z.B. 115 übergeben wird, wird für jeden Fall, die Auswertung für die Argumente vorgenommen,
; obwohl nur ein Fall eintreffen wird.
; Die Argumente erst beim zutreffen des Falls (also nach dem die Bedingung für das if true ist), auszuwerten ist
; demnach wesentlich effizienter.
; Zweiteres wird eben durch die Speziallform (worunter auch die if Operation fällt) abgedeckt.
; Aus diesen Gründen ist die Definition einer eigenen if Operation problematisch.



