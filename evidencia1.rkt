#lang racket
;; Leer un archivo de texto (.txt) que contiene un automata en el lenguaje que diseñaste
(define archivo "automatas DFA.txt") ; archivo de entrada

(define in (open-input-file archivo) ) ; abrir el archivo
(define lineas (sequence->list (in-lines in)  )) ; leer las líneas del archivo como una sequence que convertimos en lista con sequence->list
(close-input-port in) ; cerrar el archivo

; Funcion que recorre e imprime la lista
;         con print, que permite ver el string con todo y comillas
(define (print-lines lista)
   (cond
     [(empty? (cdr lista) ) (print (car lista)) ]
     [else (print (car lista)) (newline)  (print-lines (cdr lista) ) ]
   )
)
(print-lines lineas)

;; Verificar el contenido del archivo acuerdo al léxico y sintaxis de tu lenguaje, utilizando expresiones regulares, tokenizacion y funciones
(define match-pos regexp-match-positions) ; acortar el nombre de la función regexp-match-positions