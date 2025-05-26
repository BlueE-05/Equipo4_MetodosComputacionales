#lang racket
;; 1. Leer un archivo de texto que contiene un autómata DFA
(define archivo "testing_files/automatas DFA.txt")

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
(require "sintaxis.rkt")
(define-values (instrucciones resultados) (verify-sintaxis tokens))

;; 4. Ejecutar automata (si se encuentra la instruccion en el txt)
(define resultados-final (if (null? instrucciones) resultados (procesar-instrucciones instrucciones)))

;; 5. Crear html
(require "css.rkt")
(create-css)
(create-html "automatasDFA.html" tokens resultados-final)
