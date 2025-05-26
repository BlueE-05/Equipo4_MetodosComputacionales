#lang racket

(require racket/match)
(require "automaton.rkt")
(provide verify-sintaxis procesar-instrucciones)

;; === Preprocesamiento ===

(define (filter-tokens lst)
  (filter
   (lambda (pair)
     (let ([label (car pair)])
       (and (not (equal? label "\n"))
            (not (equal? label "rg_comment")))))
   lst))

;; === Función principal ===
(define (verify-sintaxis lst)
  (define tokens (filter-tokens lst))
  
  (define (loop tokens instrucciones)
    (cond
      [(null? tokens)
       (displayln "\n[✓] Verificación terminada.\n")
       (reverse instrucciones)] ; devuelve instrucciones acumuladas
      [else
       (define token (first tokens))
       (define label (first token))
       (cond
         [(equal? label "rg_automatonStart")
          (define-values (inst rest) (parse-one-automaton tokens))
          (loop rest (cons inst instrucciones))]
         [(and (equal? label "rg_identifier")
               (>= (length tokens) 2)
               (equal? (first (second tokens)) "rg_action"))
          (define-values (inst rest) (parse-exec-instruction tokens))
          (loop rest (cons inst instrucciones))]
         [else
          (error "se esperaba el inicio de un autómata '{' o una instrucción de ejecución.")])]))
  
  (with-handlers ([exn:fail?
                   (lambda (ex)
                     (define mensaje (format "[!] Sintaxis error: ~a" (exn-message ex)))
                     (displayln mensaje)
                     (displayln "[✓] Verificación terminada.\n")
                     ;; En caso de error, devolver lista de instrucciones y solo el token de error
                     (values (reverse '())               ; instrucciones vacías o parciales
                             (list (list "rg_resultsError" mensaje))) )])
    ;; Si no hay error, devolver lista de instrucciones y tokens filtrados
    (values (loop tokens '()) tokens)))

;; === Parsers ===

(define (parse-one-automaton tokens)
  (match tokens
    [(list `("rg_automatonStart" ,_) rest ...)
     (parse-name-def rest)]
    [else (error "se esperaba '{' al inicio del autómata.")]))

(define (parse-name-def tokens)
  (match tokens
    [(list `("rg_DFAname" ,_)
           `("rg_colon" ,_)
           `("rg_identifier" ,id)
           rest ...)
     (printf "\nNombre del autómata: ~a\n" id)
     (define name (string->symbol id))
     (define-values (alfabeto states accept transiciones resto)
       (parse-alphabet-or-states rest))
     (values
      `(create-automata ',name 'DFA ,states ',accept ',transiciones ,''())
      resto)]
    [else (error "en la definición de nombre del autómata")]))

(define (parse-alphabet-or-states tokens)
  (match tokens
    [(list `("rg_alphabet" ,_)
           `("rg_colon" ,_)
           `("rg_listStart" ,_)
           rest ...)
     (define-values (ok? alfabeto remaining) (parse-char-list rest))
     (if ok?
         (parse-states-def remaining alfabeto)
         (error "en la lista de símbolos del alfabeto"))]

    [(list `("rg_statesDef" ,_) rest ...)
     (parse-states-def tokens '())] ; sin alfabeto explícito

    [else (error "se esperaba 'alphabet' o 'states' después del nombre del autómata")]))

(define (parse-states-def tokens alfabeto)
  (match tokens
    [(list `("rg_statesDef" ,_)
           `("rg_colon" ,_)
           `("rg_int" ,numero)
           rest ...)
     (let ([numero (string->number numero)])
       (printf "Cantidad de estados: ~a\n" numero)
       (parse-alphabet-or-accept rest alfabeto numero))]
    [else (error "en la definición de estados")]))

(define (parse-alphabet-or-accept tokens alfabeto estados)
  (cond
    [(and (pair? tokens)
          (equal? (car (first tokens)) "rg_alphabet"))
     (match tokens
       [(list `("rg_alphabet" ,_)
              `("rg_colon" ,_)
              `("rg_listStart" ,_)
              rest ...)
        (define-values (ok? new-alfabeto remaining) (parse-char-list rest))
        (if ok?
            (parse-accept-def remaining new-alfabeto estados)
            (error "en la lista de símbolos del alfabeto"))]
       [else (error "en definición de alfabeto")])]

    [(and (pair? tokens)
          (equal? (car (first tokens)) "rg_accept"))
     (parse-accept-def tokens alfabeto estados)]

    [else (error "se esperaba 'accept' o 'alphabet' después de la definición de estados")]))

(define (parse-accept-def tokens alfabeto estados)
  (match tokens
    [(list `("rg_accept" ,_)
           `("rg_colon" ,_)
           rest ...)
     (define-values (ok? accept remaining) (parse-state-list rest))
     (if ok?
         (begin
           (printf "Estados de aceptación: ~a\n" accept)
           (parse-transitions-def remaining alfabeto estados accept))
         (error "en la lista de estados de aceptación"))]
    [else (error "en la definición del estado de aceptación")]))

(define (parse-state-list tokens)
  (define (loop tokens acc)
    (cond
      [(null? tokens)
       (values #t (reverse acc) '())]

      [(equal? (car (car tokens)) "rg_state")
       (define estado (cadr (car tokens)))
       (define rest (cdr tokens))
       (if (and (pair? rest) (equal? (car (car rest)) "rg_comma"))
           (loop (cdr rest) (cons estado acc)) ; continúa
           (values #t (reverse (cons estado acc)) rest))] ; fin si no hay coma

      [else (values #t (reverse acc) tokens)])) ; terminó la lista
  (loop tokens '()))

(define (parse-transitions-def tokens alfabeto estados accept)
  (match tokens
    [(list `("rg_transitions" ,_)
           `("rg_colon" ,_)
           rest ...)
     (define-values (ok? transiciones remaining) (parse-transitions-list rest '()))
     (if ok?
         (values alfabeto estados accept transiciones remaining)
         (error "en la definición de 'transitions'"))]
    [else (error "en la definición de 'transitions'")]))

(define (parse-transitions-list tokens acc)
  (let* ([tokens (skip-commas tokens)])
    (match tokens
      [(list `("rg_automatonEnd" ,_) rest ...)
       (printf "Bloque de transiciones terminado correctamente.\n")
       (values #t (reverse acc) rest)]

      [_
       (define-values (ok? transicion rest) (parse-one-transition tokens))
       (if ok?
           (parse-transitions-list rest (cons transicion acc))
           (values #f '() '()))])))

(define (parse-one-transition tokens)
  (match tokens
    [(list `("rg_parentesisStart" ,_)
           `("rg_state" ,estado1)
           `("rg_comma" ,_)
           `("rg_listStart" ,_)
           rest ...)
     (define-values (ok? chars rest1) (parse-char-list rest))
     (if (not ok?) (values #f '() '())
         (match rest1
           [(list `("rg_comma" ,_)
                  `("rg_state" ,estado2)
                  `("rg_parentesisEnd" ,_)
                  rest2 ...)
            (values #t `(,estado1 ,chars ,estado2) rest2)]
           [else (values #f '() '())]))]
    [else (values #f '() '())]))

(define (parse-char-list tokens)
  (define (loop tokens acc)
    (match tokens
      ;; Fin de lista
      [(list `("rg_listEnd" ,_) rest ...)
       (values #t (reverse acc) rest)]

      ;; Caracter seguido de coma
      [(list `("rg_char" ,c) `("rg_comma" ,_) rest ...)
       (loop rest (cons c acc))]

      ;; Último carácter antes de cerrar lista
      [(list `("rg_char" ,c) `("rg_listEnd" ,_) rest ...)
       (values #t (reverse (cons c acc)) rest)]

      [else (values #f '() '())]))
  (loop tokens '()))

(define (parse-exec-instruction tokens)
  (match tokens
    [(list `("rg_identifier" ,id)
           `("rg_action" ,action)
           `("rg_listStart" ,_)
           rest ...)
     (define-values (ok? chars remaining) (parse-char-list rest))
     (if ok?
         (begin
           (printf "\nIdentificador: ~a\n" id)
           (printf "Acción detectada: ~a\n" action)
           (printf "Lista de caracteres: '~a\n" chars)
           (values `(run-automata ',(string->symbol id) ', chars) remaining))
         (error "en instrucción de ejecución"))]
    [else (error "en la instrucción de ejecución")]))

;; === Utilidades ===

(define (skip-commas tokens)
  (cond
    [(null? tokens) '()]
    [(equal? (first (first tokens)) "rg_comma")
     (skip-commas (cdr tokens))]
    [else tokens]))

(define (procesar-instrucciones instrucciones)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require '(file "automaton.rkt"))
    (define resultados '())
    (for-each
     (lambda (inst)
       (define res (eval inst))
       (when (and (list? inst)
                  (equal? (car inst) 'run-automata))
         (set! resultados (cons (list "rg_resultados" res) resultados))))
     instrucciones)
    (reverse resultados)))
