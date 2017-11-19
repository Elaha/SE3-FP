#lang racket
; SE3 Funktionale Programmierung / Blatt 3 
; Gruppe 5
; Jan Ouborny 7047561
; Ozan Boga 6689640


; Aufgabe 1.1 (2.1) (3.1)
; Wir haben uns für die Aufgaben, für die Datenstrucktur der Liste entschieden. 
; Denn für eine Liste haben wir in Racket verschiedene Operationen, die bezüglich dieser Aufgabe sehr nützlich sein können.
; Mit assoc können wir in einer Liste für ein "Schloss" direkt den Schlüssel finden. 
; Durch car und cdr lässt sich eine Liste einfach rekusiv zerlegen und kann mittels cons ohne Probleme wieder zusammengefügt werden.
; Aus diesen Gründen, werden wir fortlaufen auch für die nächsten Aufgaben, die Liste als Datenstrucktur wählen.
(define buchstabentafel
  '(
   (#\A Alpha)
   (#\B Bravo)
   (#\C Charlie)
   (#\D Delta)
   (#\E Echo)
   (#\F Foxtrot)
   (#\G Golf)
   (#\H Hotel)
   (#\I India)
   (#\J Juliet)
   (#\K Kilo)
   (#\L Lima)
   (#\M Mike)
   (#\N November)
   (#\O Oscar)
   (#\P Papa)
   (#\Q Quebec)
   (#\R Romeo)
   (#\S Sierra)
   (#\T Tango)
   (#\U Uniform)
   (#\V Viktor)
   (#\W Whiskey)
   (#\X X-ray)
   (#\Y Yankee)
   (#\Z Zulu)
   (#\0 Nadazero)
   (#\1 Unaone)
   (#\2 Duotwo)
   (#\3 Terrathree)
   (#\4 Carrefour)
   (#\5 Pentafive)
   (#\6 Soxsix)
   (#\7 Setteseven)
   (#\8 oktoeight)
   (#\9 Novonine)
   (#\, Decimal)
   (#\. Stop)))  
   
; Aufgabe 1.2
; Findet zu einem gegebenen Char, dass in der Liste gespeicherte Codewort
(define(key->lock b)
    (cadr(assoc b buchstabentafel)))

; Aufgabe 1.3

; Übersetzt mit Hilfe der oberen Funktion "key->lock" eine Liste aus Chars, 
; in die entsprechenden Codewörter, welche in der Liste gespeichert sind.
(define (charlist->codeword charlist)
  (if (null? charlist)
     '()
     (cons(key->lock (car charlist)) (charlist->codeword (cdr charlist)))))
     

; Wandelt einen String in ein Codewort um.
(define (text->codeword text) ; Diese Funktion nimmt einen String entgegen. 
  (charlist->codeword (string->list text))) ; Dieser wird bei der Übergabe an unsere rekusive Funktion, zu einer Liste von Chars übersetzt.

                            

; Aufgabe 2
(require se3-bib/flaggen-module)
; Aufgabe 2.1
; Wahl der Datenstrucktur: Liste (Aufgrund der oben genannten Gründe).
; Auch bei dieser Aufgabe bietet sich die verwendung einer Liste an.
(define flaggenalphabet
 '(
   (#\A A)
   (#\B B)
   (#\C C)
   (#\D D)
   (#\E E)
   (#\F F)
   (#\G G)
   (#\H H)
   (#\I I)
   (#\J J)
   (#\K K)
   (#\L L)
   (#\M M)
   (#\N N)
   (#\0 O)
   (#\P P)
   (#\Q Q)
   (#\R R)
   (#\S S)
   (#\T T)
   (#\U U)
   (#\V V)
   (#\W W)
   (#\X X)
   (#\Y Y)
   (#\Z Z)
   (#\0 ZO)
   (#\1 Z1)
   (#\2 Z2)
   (#\3 Z3)
   (#\4 Z4)
   (#\5 Z5)
   (#\6 Z6)
   (#\7 Z7)
   (#\8 Z8)
   (#\9 Z9)))

; Aufgabe 2.2
; Findet zu einem übergebenden Char, die passende Flagge.
(define (char->flag x)
 (eval(cadr(assoc x flaggenalphabet))))

; Aufgabe 2.3
; Wandelt eine Liste aus Chars, durch reksuiven Aufruf, in eine Liste aus Flaggen um.
(define (charlist->flag charlist)
  (if(null? charlist)
     '()
     (cons(char->flag (car charlist)) (charlist->flag (cdr charlist)))))

; Wandelt einen String in eine Flaggencombination um.
(define(text->flagcode text)
  (charlist->flag (string->list text)))

; Aufgabe 3
; Begründung der Datenstruktur siehe 1.1 mit Zusatz:
; Anstatt Symbolen wird der Name der passenden Soundfile gespeichert.
(require racket/gui/base)
(define morsealphabet
 '(
   (#\A "Morse-A.wav")
   (#\B "Morse-B.wav")
   (#\C "Morse-C.wav")
   (#\D "Morse-D.wav")
   (#\E "Morse-E.wav")
   (#\F "Morse-F.wav")
   (#\G "Morse-G.wav")
   (#\H "Morse-H.wav")
   (#\I "Morse-I.wav")
   (#\J "Morse-J.wav")
   (#\K "Morse-K.wav")
   (#\L "Morse-L.wav")
   (#\M "Morse-M.wav")
   (#\N "Morse-N.wav")
   (#\0 "Morse-O.wav")
   (#\P "Morse-P.wav")
   (#\Q "Morse-Q.wav")
   (#\R "Morse-R.wav")
   (#\S "Morse-S.wav")
   (#\T "Morse-T.wav")
   (#\U "Morse-U.wav")
   (#\V "Morse-V.wav")
   (#\W "Morse-W.wav")
   (#\X "Morse-X.wav")
   (#\Y "Morse-Y.wav")
   (#\Z "Morse-Z.wav")))



(define (char->morse x)
 (play-sound (cadr(assoc x morsealphabet)) #f))

; Übersetzt mit Hilfe der oberen Funktion "key->lock" eine Liste aus Chars, 
; in die entsprechenden Codewörter, welche in der Liste gespeichert sind.
(define (charlist->morse charlist)
    (if (empty? charlist)
       (void)
       (begin
         (char->morse (car charlist))
         (charlist->morse (cdr charlist)))))
     
(define (text->morse text)
  (charlist->morse (string->list text)))
