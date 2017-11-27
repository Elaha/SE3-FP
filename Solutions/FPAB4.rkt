#lang racket
; Jan Ouborny 7047561
; Ozan Boga 6689640

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
; Erweiterte Backus-Naur-Form ISO/IEC 14977:1996(E)
;
; Überschrift       = 3 * (Notzeichen, Space), Newline, 
;                         (("DELTA", Space, "ECHO") | ("HIER", Space, "IST")), Newline,
;                         3 * (Schiffsname Space), RufzeichenBst, Newline,
;                         Notzeichen, Space, Schiffsname, Space, ["ICH", Space, "BUCHSTABIERE"], Space, SchiffsnameBst, Newline,
;                         ["RUFZEICHEN"], RufzeichenBst, Newline;
; Unterschrift      = Schiffsname, Space, Rufzeichen, Newline;
; Standortangabe    = Satz;
; NotfallArt        = Satz;
; Hilfeleistung     = Satz;
; Notfallzeit       = Satz;
; Peilzeichen       = "-";


; Notzeichen        = "MAYDAY";
; Schiffsname       = Wort;
; SchiffsnameBst    = Buchstabiert;
; Rufzeichen        = 4 * (Buchstabe | Ziffer);
; RufzeichenBst     = 4 * Codewort;


; Satz              = Wort, (Space | Newline), Satz;
; Wort              = ( Ziffer | Buchstabe ), { (Ziffer | Buchstabe ) };
; Buchstabiert      = Codewort, Space, Buchstabiert;              
; Buchstabe         = "a" | "b" | "c" | ... | "z" | "A" | "B" | "C" | ... | "Z";
; Ziffer            = "0" | "1" | "2" |... | "8" | "9";
; Codewort          = "Alpha" | "Bravo" |" Chrarlie" | ... | "Novonine" | "Decimal" | "Stop";
; Space             = ? Leerzeichenzeichen ?;
; Newline           = ? Zeilenvorschubzeichen ?;

; Notmeldung        = Überschrift, Standortangabe, Newline,
;                       [Notfallzeit, NewLine],
;                       NotfallArt, Newline,
;                       Hilfeleistung, 2 * Pfeilzeichen, Newline,
;                       Unterschrift, Newline,
;                       "OVER";
 
;Aufgabe 2.2
;(require "AB3FP.rkt")
(require "AB2.rkt")
(define (notmeldung schiffsname rufzeichen position notfallArt hilfeleistung)
  (string-upcase (string-append (ueberschrift (string-upcase schiffsname) (string-upcase rufzeichen))
                                position "\n"
                                notfallArt "\n"
                                hilfeleistung "--\n"
                                (unterschrift schiffsname rufzeichen)
                                "\nOVER\n")))

(define (string-spell string) (codewords->string (text->codeword string)))
(define (string-no-space string) (letrec ([charlist (string->list string)]
                                                           [rec-help (lambda (input hresult) (if (null? input)
                                                                                              hresult
                                                                                              (rec-help (cdr input) (if (char=? #\space (car input))
                                                                                                                        hresult
                                                                                                                        (string-append hresult (make-string 1 (car input)))))))])
                                                    (rec-help charlist "")))

(define (codewords->string codewords [result ""]) 
  (if (null? codewords)
      result
      (codewords->string (cdr codewords) (string-append result (symbol->string (car codewords)) " "))))

(define (ueberschrift schiffsname rufzeichen)
  (string-append "MAYDAY MAYDAY MAYDAY \n"
                 "DELTA ECHO\n"
                 schiffsname " " schiffsname " "schiffsname " " (string-spell rufzeichen) "\n"
                 "MAYDAY " schiffsname " " (string-spell (string-no-space schiffsname)) "\n"
                 "RUFZEICHEN " (string-spell rufzeichen) "\n"))
(define (unterschrift schiffsname rufzeichen)
  (string-append schiffsname " " (string-spell rufzeichen)))
;Aufgabe 2.3
(display (notmeldung "Unicorn" "UCRN" "NOTFALLPOSITION UNGEFÄHR 5 SM NORDWESTLICH LEUCHTTURM ROTER SAND"
            "SCHWERE SCHLAGSEITE WIR SINKEN\nKEINE VERLETZTEN\nSECHS MANN GEHEN IN DIE RETTUNGSINSEL\nSCHNELLE HILFE ERFORDERLICH\nICH SENDE DEN TRÄGER"
            "SCHNELLE HILFE ERFORDERLICH "))

;Aufgabe 2.4
(display (notmeldung "Nautilus" "DEYJ" "NOTFALLPOSITION 10 sm östlich Point Nemo" "Ein Riesenkrake hat das Schiff umschlungen\nEin grosses Leck im Rumpf\n20 Personen an Bord\nTreiben antriebslosan der Wasseroberfläche"
                     "SCHNELLE HILFE ERFORDERLICH "))

(display (notmeldung "Maltese Falcon" "HUQ9" "Auf einer Sandpark im Norden" "10 Mann an Bord\ndas Schiff ist 88m lang" "Essen erforerlich"))





; Bezugstransparenz = Ausdrücke die den selben Wert speichern können durch einander ersetzt werden.
; Innere Reduktion ist wenn die Berechnung mit den Übergebenen Werten beginnt.
; Äußere Reduktion ist wenn der Body zuerst ausgefürt wird und man für

; Aufgabe 3.1

(define (hoch3 x) (* x x x)) (hoch3 ( + 3 (hoch3 3)))

; Innere Reduktion:   (Argumentauswertung --> Funtionsdefinition)
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

(require racket/trace)

(define (new-if condition? then-clause else-clause)
  (cond (condition? then-clause)
        (else else-clause)))

(define (faculty product counter max-count)
  (new-if (> counter max-count)
          product
          (faculty (* counter product)
                   (+ counter 1)
                   max-count)))

(trace faculty)

; Wenn man (faculty 1 1 5) eingibt, stürzt das Prgramm ab, aus dem Grund, dass kein Speicher mehr vorhanden ist.
; Dies liegt an der Rheinfolge der Auswertung:
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
; Wenn bei faculty z.B. 1 1 5 übergeben wird, wird für jeden Fall, die Auswertung für die Argumente vorgenommen,
; obwohl nur ein Fall eintreffen wird.
; Die Argumente erst beim zutreffen des Falls (also nach dem die Bedingung für das if true ist), auszuwerten ist
; demnach wesentlich effizienter.
; Zweiteres wird eben durch die Speziallform (worunter auch die if Operation fällt) abgedeckt.
; Aus diesen Gründen ist die Definition einer eigenen if Operation problematisch.




