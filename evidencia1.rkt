#lang racket
;; 1. Leer un archivo de texto que contiene un autómata DFA
(define archivo "automatas DFA.txt")

; Abrir y leer el archivo línea por línea
(define in (open-input-file archivo))
(define lineas (sequence->list (in-lines in))) ; se guarda una lista donde cada posicion es una línea
(close-input-port in)

;; 2. Tokenizar línea por línea y guardar nueva lista con las posiciones y tipo
(require "lexico.rkt")

; Función recursiva que tokeniza todas las líneas y concatena los resultados
(define (tokenize_all lineas)
  (cond
    [(null? lineas) '()] ; caso base: si ya no hay líneas, devuelve los tokens acumulados
    [else
     (append (tokenize (car lineas)) ; tokeniza linea actual y append
             (list '("\n" "\n")) ; formateo del html
             (tokenize_all (cdr lineas)))])) ; regresa el resto de la lista

(define tokens (tokenize_all lineas))


;; 3. Verificar sintaxis y encontrar errores
(require "sintaxis2.rkt")
(verify-sintaxis tokens)

;; 4. Crear html
(require "css.rkt")
(create-css)
(create-html "automataDFA.html" tokens)

;; 5. Ejecutar automata (si se encuentra la instruccion en el txt)
(require "automaton.rkt")

; Función que verifica si la función de ejecutar esta presente en el archivo, de ser así lo intenta ejecutar
;(define run-if-automaton
;  (define automaton_name)
;  (list input)
;  (run-automata 'automaton_name input)
;)