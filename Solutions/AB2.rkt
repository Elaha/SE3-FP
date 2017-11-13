#lang racket
 ; SE3 Funktionale Programmierung / Blatt2
 ; Gruppe 5
 ; Jan Ouborny 7047561
 ; Ozan Boga 6689640


; Aufgabe 1
(define wuff 'Flocki)

(define Hund wuff)

(define Wolf 'wuff)

(define ( welcherNameGiltWo PersonA PersonB )
   (let ((PersonA 'Zaphod)
   (PersonC PersonA))
  PersonC))

(define xs1 '(0 2 3 wuff Hund))
(define xs2 (list wuff Hund))
(define xs3 (cons Hund wuff))

; 1. wuff --> 'Flocki = Der Wert der in wuff gespeichert wurde, wird ausgegeben.

; 2. Hund --> 'FLocki = der Wert der vorher in wuff gespeichert wurde wird ausgegeben, da wuff nun in Hund gespeichert ist.

; 3. Wolf --> 'wuff = denn durch das quote Zeichen findet keine weitere Auswertung statt.

; 4. (quote Hund) --> 'Hund = durch den Befehl "quote" wird Hund quotiert und nicht ausgewertet.

; 5. (eval Wolf) --> 'Flocki = durch das eval wird das quotierte 'wuff, welches in Wolf gespeichert ist, aufgehoben und wuff wird ausgewertet.

; 6. (eval Hund) --> Error = denn Flocki wurde nirgends defined. Durch das eval fällt das Quotierungszeichen weg und es wird zu einer undefinierten Variable.

; 7. (eval 'Wolf --> 'wuff = denn das eval, und das quote Zeichen bei Wolf heben sich gegenseitig auf. Da, dass in Wolf gespeicherte 'wuff ebenfalls quotiert ist,
;                            findet danach keine weitere Auswertung statt.

; 8. (welcherNameGiltWo 'lily 'potter) --> 'lily = Denn wir übergeben als Person A lily welche intern Person C zugewiesen wird. Anschließend wird Person C also 'lily ausgegeben.

; 9. (cdddr xs1) --> '(wuff, Hund) = Hierbei werden durch dass cdddr, die ersten 3 Elemente abgeschnitten und die Restliste wird zurück gegeben. 

; 10. (cdr xs2) --> '(Flocki) = denn durch das cdr bekommen wir den hinteren Teil, also die Teilliste (Hund) zurück. Diese wird anschließend ausgewertet, worauß '(Flocki) folgt.  

; 11. (cdr xs3) --> 'Flocki = denn durch das cons werden Hund und wuff zu einem Pair. Durch das cdr bekommen wir den hinteren Teil des Pairs "wuff". Dieser ausgewertet, ergibt 'Flocki.

; 12. (sqrt 1/4 --> 1/2 = sqrt ist die Wurzelfunktion. Angewand auf 1/4 ergibt es 1/2.

; 13. (eval '(welcherNameGiltWo 'Wolf 'Hund) --> 'Wolf = eval und das quote vor der Funktion heben sich auf. Wir geben 'Wolf für Person A ein und durch das quote bekommen wir wieder 'Wolf heraus,
;                                                        ohne das es ausgewertet wurde.

; 14. (eval (welcherNameGiltWo 'Hund 'Wolf) --> 'Flocki = Unsere Person A ist 'Hund. Durch das eval entfällt das quote bei 'Hund und wir erhalten bei der Ausgabe 'Flocki.

; Aufabe 2.1
; Eine Funktion, die die Fakultät zu einer gegebenen natürlichen Zahl berechnet.
; n stellt hierbei die natürliche Zahl dar, die übergeben wird.
(define (factorial n)
  (letrec ([calculate-factorial (lambda (m)
                                  (if (= m 0)
                                      1
                                      (* m (calculate-factorial (- m 1)))))])
  (if (< n 0)
      (error "Fakultäten können nicht für negative Zahlen berechnet werden.")
      (calculate-factorial n))))

; Aufgabe 2.2
; Berchnung von der Potenzfunktion r hoch n.
; r ist hierbei ein beliebiges Element der rationalen Zahlen.
; n ist hierbei ein beliebiges Element der natürlichen Zahlen.
(define (power r n)
  (if (= n 0)
      1
      (if(odd? n)
          (* r (power r (- n 1)))

          (sqr (power r (/ n 2))))))

; Aufgabe 2.3
(define euler
  (letrec
      ([calculate-fractures (lambda (e)
                                (let ([x (/ e
                                            (factorial (- e 1)))]
                                      [exitV (/ 1
                                                (power 10 1000))]) ; Festlegung der Genauigkeit der Berechnung (1000 Nachkommastellen)
                                  (if (< x exitV)
                                      0
                                      (+ x (calculate-fractures (+ e 1))))))])
    (* (/ (+ (calculate-fractures 2) 1)
          2)
       (power 10 1001))))

; Aufgabe 3
; Weist Eingabe einem bestimmten Typen zu und gibt diesen wieder.
; x steht hierbei für die Eingabe.
(define (type-of x)
  (cond [(boolean? x) "boolean"]
        [(symbol? x) "symbol"]
        [(number? x) "number"]
        [(pair? x) "pair"]
        [(list? x) "list"]
        [(char? x) "char"]
        [(string? x) "string"]
        [(vector? x) "vector"]
        [(procedure? x) "procedure"]))

; 1. (type-of (* 2 3 4))            --> x = number /Denn in diesem Fall ist X eine normale mathematische Auswertung dessen Ergebnis wieder eine Nummer ist.

; 2. (type-of (not 42))             --> x = boolean /Durch das "not" wird diese Eingabe zu einem Wahrheitswert, also einem boolean.

; 3. (type-of '(eins zwei drei))    --> x = pair / Hier sind offensichtlich Elemente enthalten, die durch cons verbunden und zu einer Liste hinzugefügt wurden.
;                                           Demnach kann dies hier auch als eine Liste von pairs angesehen werden.

; 4. (type-of '())                  --> x = list / Hier wird als eingabe die leere Liste übergeben, welches auch als Liste identfiziert wird.

;   (define (id z) z)
; 5. (type-of (id sin))              --> x = procedure / Da sin an sich schon eine Prozedur ist und hier als Wert keine konkrete Zahl mit übergeben wird, wird für x = procedure ausgegeben.

; 6. (type-of (string-ref "SE3" 2)) --> x = char / Die Funktion "string-ref" liefert von einem gegebenen String den Buchstaben (char) an einer gewünschten Stelle (an dieser Stelle an Positon 2 also den char "3").

; 7. (type-of (lambda (x) x))       --> x = procedure / Da lambda eine Funktion bzw. eine Prozedur ist, die bei einer Angabe etwas tut.

; 8. (type-of type-of)              --> x = procedure / Da die Funktion "type of" die wir geschrieben haben ja selber eine Funktion bzw. Prozedur ist (Also etwas, was ausgeführt werden kann).

; 9 .(type-of (type-of type-of))    --> x = string / Das (type-of type-of) welches sich innerhalb der Klammern befindet, liefert als Ergebnis den String "x = procedure".
;                                           Das Ergebnis der inneren Funktion, welches das string ist, wird der äußeren Funktion als X übergeben.
;                                           Da unser übegebnes x, als string vorliegt, ist auch unser endgültiges Ergebnis ein string. 
