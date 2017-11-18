#lang racket
(define buchstabentafel
  '(
   (#\A A Alpha)
   (#\B B Bravo)
   (#\C C Charlie)
   (#\D D Delta)
   (#\E E Echo)
   (#\F F Foxtrot)
   (#\G G Golf)
   (#\H H Hotel)
   (#\I I India)
   (#\J J Juliet)
   (#\K K Kilo)
   (#\L L Lima)
   (#\M M Mike)
   (#\N N November)
   (#\0 O Oscar)
   (#\P P Papa)
   (#\Q Q Quebec)
   (#\R R Romeo)
   (#\S S Sierra)
   (#\T T Tango)
   (#\U U Uniform)
   (#\V V Viktor)
   (#\W W Whiskey)
   (#\X X X-ray)
   (#\Y Y Yankee)
   (#\Z Z Zulu)
   (#\0 ZO Nadazero)
   (#\1 Z1 Unaone)
   (#\2 Z2 Duotwo)
   (#\3 Z3 Terrathree)
   (#\4 Z4 Carrefour)
   (#\5 Z5 Pentafive)
   (#\6 Z6 Soxsix)
   (#\7 Z7 Setteseven)
   (#\8 Z8 oktoeight)
   (#\9 Z9 Novonine)
   (#\, () Decimal)
   (#\. () Stop)))

(define (key->lock b)
  
    (caddr (assoc b buchstabentafel))
)

(define (buchstabiere text)
  (letrec (
           [build-output (lambda (char-list [running-val ""]) (
                                               (if (empty? char-list)
                                                   running-val
                                                   (build-output
                                                    (cdr char-list)
                                                    (string-append running-val "A")))))])
                                                    ;(string-append running-val (symbol->string (key->lock (car char-list))))))))])
    (build-output (string->list text))))
(buchstabiere "Test")
(key->lock #\J)
#|(define char->generic (char mapping picker-func) ;[picker-func (lambda (map) (cadr map))]
  (let ([listPosition caar(mapping)])
    ((if(= char (caar mapping))
     (picker-func listPosition)
     (if (empty? listPosition)
         'Error ;(raise "Unbekanntes Zeichen")
         (char->generic (cdr mapping) picker-func))))))|#
 



(define (rkst x)
(if(> (length x) 0)
  (assoc(cadr(string->list x ))buchstabentafel)
  "keine list"
  ))

;Aufgabe 2
;(require se3-bib/flaggen-module)

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


(define (char->flagge xs)

(if (empty? xs)
    '()
    (cons(char->flag(car xs))

    (char->flagge (cdr xs)))))



(define(string->flagge x)

(char->flagge (string->list x )))














(require racket/gui/base)

