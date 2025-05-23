#lang racket

(provide create-automata run-automata)

; Estructura de los automatas
(struct automata (type states accept start transitions) #:transparent)

; Tabla global de autómatas
(define automatas (make-hash))

; Función recursiva para generar la lista de estados
(define (create-states n)
  (define (aux i acc)
    (if (< i 0)
        acc
        (aux (- i 1) (cons (string->symbol (format "q~a" i)) acc))))
  (aux (- n 1) '()))

; Función recursiva para inicializar la tabla de transiciones
(define (init-trans-table states table)
  (if (null? states)
      table
      (begin
        (hash-set! table (car states) '())
        (init-trans-table (cdr states) table))))

; Función recursiva para agregar las transiciones
(define (add-transitions transitions table)
  (cond
    [(null? transitions) table]
    [else
     (match (car transitions)
       [(list desde chars hacia) ; qInicial, chars, qSiguiente
        (define (agregar-chars cs)
          (cond
            [(null? cs) '()]
            [else
             (define actuales (hash-ref table desde))
             (hash-set! table desde (cons (cons (car cs) hacia) actuales))
             (agregar-chars (cdr cs))]))
        (agregar-chars chars)])
     (add-transitions (cdr transitions) table)]))

; Función principal para crear el automata
(define (create-automata name type state_qty accept_state transitions [alfabeto '()])
  (define states (create-states state_qty))
  (define start (car states))
  (define trans-table (make-hash))
  (init-trans-table states trans-table)
  (add-transitions transitions trans-table)
  (define aut (automata type states accept_state start trans-table))
  (hash-set! automatas name aut)
  (printf "[✓] Automaton ~a created successfully.\n" name))

; Función que ejecuta una entrada sobre un autómata guardado
(define (input->string lst)
  (string-append "[" (string-join (map symbol->string lst) " ") "]"))

(define (run-automata name input)
  (define aut (hash-ref automatas name #f))
  (define result
    (if (not aut)
        (begin
          (printf "[!] Automaton ~a not found.\n" name)
          'failure)
        (match aut
          [(automata type states accept start transitions)
           (define (traverse state input)
             (cond
               [(null? input)
                (if (member state accept) 'success 'failure)]
               [else
                (define options (hash-ref transitions state '()))
                (define next (assoc (car input) options))
                (if next
                    (traverse (cdr next) (cdr input))
                    'failure)]))
           (traverse start input)])))
  (printf "~a with input ~a -> ~a\n" name (input->string input) result) ; imprime el resultado con un formato
  ; result ; de esta forma también regresa el resultado
)

  ; Prueba
;  (create-automata 'termina-en-ab ; Acepta cadenas que terminan en "ab"
;                  'DFA
;                  3
;                  (list 'q2)
;                  (list
;                  (list 'q0 '(a) 'q1)
;                  (list 'q0 '(b) 'q0)
;                  (list 'q1 '(a) 'q1)
;                  (list 'q1 '(b) 'q2)
;                  (list 'q2 '(a) 'q1)
;                  (list 'q2 '(b) 'q0)))
  ; (newline)
  ; (run-automata 'termina-en-ab '(a b))       ; => success
  ; (run-automata 'termina-en-ab '(a a a b))   ; => success
  ; (run-automata 'termina-en-ab '(a b a))     ; => failure
  ; (run-automata 'termina '(a b))          ; => [!] Automaton termina-en not found. Test4: failure