#lang racket
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

(define(key->lock b)

    (cadr(assoc b buchstabentafel))
  
)

(define (charlist->codewort charlist)
  (if(null? charlist)
     '()
     (cons(key->lock (car charlist)) (charlist->codewort (cdr charlist)))))

(define(text->codewort text)
  
  (charlist->codewort (string->list text)))



                            


;Aufgabe 2
(require se3-bib/flaggen-module)

;Aufgabe 2.1
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

;Aufgabe 2.2
(define (char->flag x)
 (eval(cadr(assoc x flaggenalphabet
  ))))

;Aufgabe 2.3

(define (charlist->codewort2 charlist)
  (if(null? charlist)
     '()
     (cons(char->flag (car charlist)) (charlist->codewort2 (cdr charlist)))))

(define(text->flagcode text)
  
  (charlist->codewort2 (string->list text)))


(require racket/gui/base)




;Aufgabe 3

(require racket/gui/base)




