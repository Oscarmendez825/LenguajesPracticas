#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define ventana (open-viewport "Qualifiers" 1200 700))
(define oculta (open-pixmap "Qualifiers" 1000 500))


((draw-solid-rectangle ventana) (make-posn 0 0) 1100 700 "white")
((draw-solid-rectangle ventana) (make-posn 555 5) 550 690 "green")
((draw-solid-rectangle ventana) (make-posn 0 5) 550 690 "green")
((draw-solid-rectangle ventana) (make-posn 0 205) 100 300 "white")
((draw-solid-rectangle ventana) (make-posn 0 210) 95 290 "green")
((draw-solid-rectangle ventana) (make-posn 1005 205) 100 300 "white")
((draw-solid-rectangle ventana) (make-posn 1000 215) 95 290 "green")