#lang racket
; SE-Funktional Blatt-1
; Deniz Gül 6818556
; Jan Ouborny 7047561
; Ozan Boga 6689640

; Aufgabe 1.1
; Wandelt Gradzahl in Bogenmaß um
; d = Gradzahl [°]
(define (deg->rad d)
  (/ (* d pi) 180))

; Wandelt Bogenmaß in Gradzahl um
; r = Bogenmaß [rad]
(define (rad->deg r)
  (/ (* r 180) pi))

; Aufgabe 1.2
; x = Seitenverhältnis im rechtwinkligen Dreieck
; Laut "https://de.wikipedia.org/wiki/Arkussinus_und_Arkuskosinus"(28.10.17) gilt für -1 < x <= 1 folgendes
(define (my-acos x)
  (* 2 (atan (sqrt (/ (- 1 x) (+ 1 x))))))

; Aufgabe 1.3
; Umrechnung von Seemeilen in Kilometer, wobei eine Seemeile 1.852 km entspricht.
; s = Seemeile [nm]
(define (nm2km s)
  (* s 1.852))

; Aufgabe 2.1
; Für die Funktionen 'berechne-dG' und 'distanzAB' gilt, wie in der Aufgabenstellung schon beschrieben - "nördliche Breiten
; und östliche Längen [werden] positiv gerechnet, südliche Breiten und westliche Längen negativ. Dementsprechend erwarten
; wir südliche Breiten und westliche Längen als negative Eingaben.

; Berechnet die Großkreisentfernung dG zweier Orte (A, B) und wird von distanzAB benutzt zur Entfernungsmessung zwischen A und B.
; A-Br = geographische Breite von Ort A [°]
; A-La = geographische Länge von Ort A [°]
; B-Br = geographische Breite von Ort B [°]
; B-La = geographsche Länge von Ort B [°]
(define (berechne-dG A-Br A-La B-Br B-La)
  (my-acos (+ (* (sin(deg->rad A-Br)) (sin(deg->rad B-Br))) (* (cos(deg->rad A-Br)) (cos(deg->rad B-Br)) (cos (- (deg->rad A-La) (deg->rad B-La)))))))

; Berechnet die Distanz zwischen zwei Orten
; A-Br = geographische Breite von Ort A [°]
; A-La = geographische Länge von Ort A [°]
; B-Br = geographische Breite von Ort B [°]
; B-La = geographsche Länge von Ort B [°]
(define (distanzAB A-Br A-La B-Br B-La)
  (nm2km (* (rad->deg (berechne-dG A-Br A-La B-Br B-La)) 60)))

; Distanz von Oslo (59.93°N, 10.75°E) nach Hongkong (22.20°N, 114.10°E) beträgt 8589 km.
(distanzAB 59.93 10.75 22.20 114.10)

; Distanz von San Francisco (37.75°N, 122.45°W) nach Honolulu (21.32°N, 157.83°W) beträgt 3842 km.
(distanzAB 37.75 -122.45 21.32 -157.8)

; Distanz von der Osterinsel (27.10°S, 109.40°W) nach Lima (12.10°S, 77.05°W) beträgt 3757 km.
(distanzAB -27.10 -109.40 -12.10 -77.05)

; Aufgabe 2.2
; Bestimmt anhand der geographischen Längen der Orte A und B, ob der Weg zu B über den Osten oder über den Westen kürzer ist.
; A-La = geographische Länge von Ort A [°]
; B-La = geographsche Länge von Ort B [°]
(define (EoderW A-La B-La)
  (if (< B-La A-La)
   (if (< (abs (- A-La B-La)) 180)
       "W"
       "E")
   (if (< (abs (- A-La B-La)) 180)
       "E"
       "W")))

; Ermittelt den Anfangskurs, den Winkel alpha-R, in Grad.
; A-Br = geographische Breite von Ort A [°]
; A-La = geographische Länge von Ort A [°]
; B-Br = geographische Breite von Ort B [°]
; B-La = geographsche Länge von Ort B [°]
(define (anfangskurs A-Br A-La B-Br B-La)
  (let ([dG (berechne-dG A-Br A-La B-Br B-La)]) ; Die Großkreisentfernung wird über eine lokale Variable dG referenziert.
    (rad->deg (my-acos (/ (- (sin (deg->rad B-Br)) (* (cos dG) (sin (deg->rad A-Br)))) (* (cos (deg->rad A-Br)) (sin dG)))))))

; Bestimmt in welcher Richtung das Ziel B von A aus liegt.
; A-Br = geographische Breite von Ort A [°]
; A-La = geographische Länge von Ort A [°]
; B-Br = geographische Breite von Ort B [°]
; B-La = geographsche Länge von Ort B [°]
(define (zielrichtung A-Br A-La B-Br B-La)
  (let ([alpha-R (anfangskurs A-Br A-La B-Br B-La)])
    (if (eqv? "E" (EoderW A-La B-La))
        alpha-R
        (- 360 alpha-R))))

; Aufgabe 2.3
; Gibt für eine Gradzahl zwischen 0° und 360° die entsprechende Himmelsrichtung an.
; d = Gradzahl [°]
(define (deg->dir d)
  (cond [(or (and (< 348.75 d) (<= d 360)) (and (<= 0 d) (<= d 11.25))) "N"]
        [(and (< 11.25 d) (<= d 33.75)) "NNE"]
        [(and (< 33.75 d) (<= d 56.25)) "NE"]
        [(and (< 56.25 d) (<= d 78.75)) "ENE"]
        [(and (< 78.75 d) (<= d 101.25)) "E"]
        [(and (< 101.25 d) (<= d 123.75)) "ESE"]
        [(and (< 123.75 d) (<= d 146.25)) "SE"]
        [(and (< 146.25 d) (<= d 168.75)) "SSE"]
        [(and (< 168.75 d) (<= d 191.25)) "S"]
        [(and (< 191.25 d) (<= d 213.75)) "SSW"]
        [(and (< 213.75 d) (<= d 236.25)) "SW"]
        [(and (< 236.25 d) (<= d 258.75)) "WSW"]
        [(and (< 258.75 d) (<= d 281.25)) "W"]
        [(and (< 281.25 d) (<= d 303.75)) "WNW"]
        [(and (< 303.75 d) (<= d 326.25)) "NW"]
        [(and (< 326.25 d) (<= d 348.75)) "NNW"]
        [else "Eingabewert muss zwischen 0 und 360 liegen!"]))

(define (dir->deg d)
  (cond [(eqv? d "N") "0°/360°"]
        [(eqv? d "NNE") "22.5°"]
        [(eqv? d "NE") "45°"]
        [(eqv? d "ENE") "67.5°"]
        [(eqv? d "E") "90°"]
        [(eqv? d "ESE") "112.5°"]
        [(eqv? d "SE") "135°"]
        [(eqv? d "SSE") "157.5°"]
        [(eqv? d "S") "180°"]
        [(eqv? d "SSW") "202.5°"]
        [(eqv? d "SW") "225°"]
        [(eqv? d "WSW") "247.5°"]
        [(eqv? d "W") "270°"]
        [(eqv? d "WNW") "292.5°"]
        [(eqv? d "NW") "315°"]
        [(eqv? d "NNW") "337.5°"]
        ))