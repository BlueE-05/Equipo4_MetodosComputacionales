#lang racket

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
  (if (and (pair? tokens)
           (pair? (car tokens))
           (equal? (caar tokens) "rg_automatonStart"))
      (parse-name-def (cdr tokens))
      (error "se esperaba '{' al inicio del autómata.")))

(define (parse-name-def tokens)
  (if (and (>= (length tokens) 3)
           (equal? (caar tokens) "rg_DFAname")
           (equal? (caar (cdr tokens)) "rg_colon")
           (equal? (caar (cddr tokens)) "rg_identifier"))
      (let* ([id (cadar (cddr tokens))]
             [rest (cdddr tokens)])
        (printf "\nNombre del autómata: ~a\n" id)
        (define name (string->symbol id))
        (define-values (alfabeto states accept transiciones resto)
          (parse-alphabet-or-states rest))
        (values
         `(create-automata ',name 'DFA ,states ',accept ',transiciones ,''())
         resto))
      (error "en la definición de nombre del autómata")))

(define (parse-alphabet-or-states tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (caar tokens) "rg_alphabet")
          (equal? (caar (cdr tokens)) "rg_colon")
          (equal? (caar (cddr tokens)) "rg_listStart"))
     (define-values (ok? alfabeto remaining) (parse-char-list (cdddr tokens)))
     (if ok?
         (parse-states-def remaining alfabeto)
         (error "en la lista de símbolos del alfabeto"))]
    [(and (pair? tokens)
          (equal? (caar tokens) "rg_statesDef"))
     (parse-states-def tokens '())] ; sin alfabeto explícito
    [else (error "se esperaba 'alphabet' o 'states' después del nombre del autómata")]))

(define (parse-states-def tokens alfabeto)
  (if (and (>= (length tokens) 3)
           (equal? (caar tokens) "rg_statesDef")
           (equal? (caar (cdr tokens)) "rg_colon")
           (equal? (caar (cddr tokens)) "rg_int"))
      (let* ([numero (string->number (cadar (cddr tokens)))]
             [rest (cdddr tokens)])
        (printf "Cantidad de estados: ~a\n" numero)
        (parse-alphabet-or-accept rest alfabeto numero))
      (error "en la definición de estados")))

(define (parse-alphabet-or-accept tokens alfabeto estados)
  (cond
    [(and (pair? tokens)
          (equal? (caar tokens) "rg_alphabet"))
     (if (and (>= (length tokens) 3)
              (equal? (caar tokens) "rg_alphabet")
              (equal? (caar (cdr tokens)) "rg_colon")
              (equal? (caar (cddr tokens)) "rg_listStart"))
         (let* ([rest (cdddr tokens)])
           (define-values (ok? new-alfabeto remaining)
             (parse-char-list rest))
           (if ok?
               (parse-accept-def remaining new-alfabeto estados)
               (error "en la lista de símbolos del alfabeto")))
         (error "en definición de alfabeto"))]
    [(and (pair? tokens)
          (equal? (caar tokens) "rg_accept"))
     (parse-accept-def tokens alfabeto estados)]
    [else (error "se esperaba 'accept' o 'alphabet' después de la definición de estados")]))

(define (parse-accept-def tokens alfabeto estados)
  (if (and (>= (length tokens) 3)
           (equal? (caar tokens) "rg_accept")
           (equal? (caar (cdr tokens)) "rg_colon"))
      (let* ([rest (cddr tokens)])
        (define-values (ok? accept remaining)
          (parse-state-list rest))
        (if ok?
            (begin
              (printf "Estados de aceptación: ~a\n" accept)
              (parse-transitions-def remaining alfabeto estados accept))
            (error "en la lista de estados de aceptación")))
      (error "en la definición del estado de aceptación")))

(define (parse-state-list tokens)
  (define (loop tokens acc)
    (cond
      [(null? tokens)
       (values #t (reverse acc) '())]
      [(equal? (car (car tokens)) "rg_state")
       (define estado (cadr (car tokens)))
       (define rest (cdr tokens))
       (if (and (pair? rest) (equal? (car (car rest)) "rg_comma"))
           (loop (cdr rest) (cons estado acc))
           (values #t (reverse (cons estado acc)) rest))]
      [else (values #t (reverse acc) tokens)]))
  (loop tokens '()))

(define (parse-transitions-def tokens alfabeto estados accept)
  (if (and (>= (length tokens) 3)
           (equal? (caar tokens) "rg_transitions")
           (equal? (caar (cdr tokens)) "rg_colon"))
      (let* ([rest (cddr tokens)])
        (define-values (ok? transiciones remaining)
          (parse-transitions-list rest '()))
        (if ok?
            (values alfabeto estados accept transiciones remaining)
            (error "en la definición de 'transitions'")))
      (error "en la definición de 'transitions'")))

(define (parse-transitions-list tokens acc)
  (let* ([tokens (skip-commas tokens)])
    (cond
      [(and (pair? tokens)
            (equal? (caar tokens) "rg_automatonEnd"))
       (printf "Bloque de transiciones terminado correctamente.\n")
       (values #t (reverse acc) (cdr tokens))]
      [else
       (define-values (ok? transicion rest) (parse-one-transition tokens))
       (if ok?
           (parse-transitions-list rest (cons transicion acc))
           (values #f '() '()))])))

(define (parse-one-transition tokens)
  (if (and (>= (length tokens) 5)
           (equal? (caar tokens) "rg_parentesisStart")
           (equal? (caar (cdr tokens)) "rg_state")
           (equal? (caar (cddr tokens)) "rg_comma")
           (equal? (caar (cdddr tokens)) "rg_listStart"))
      (let* ([estado1 (cadar (cdr tokens))]
             [rest (cddddr tokens)])
        (define-values (ok? chars rest1)
          (parse-char-list rest))
        (if (not ok?) (values #f '() '())
            (if (and (>= (length rest1) 3)
                     (equal? (caar rest1) "rg_comma")
                     (equal? (caar (cdr rest1)) "rg_state")
                     (equal? (caar (cddr rest1)) "rg_parentesisEnd"))
                (let* ([estado2 (cadar (cdr rest1))]
                       [rest2 (cdddr rest1)])
                  (values #t `(,estado1 ,chars ,estado2) rest2))
                (values #f '() '()))))
      (values #f '() '())))

(define (parse-char-list tokens)
  (define (loop tokens acc)
    (cond
      [(null? tokens)
       (values #f '() '())]
      [(and (equal? (car (car tokens)) "rg_listEnd"))
       (values #t (reverse acc) (cdr tokens))]
      [(and (>= (length tokens) 2)
            (equal? (car (car tokens)) "rg_char")
            (equal? (car (car (cdr tokens))) "rg_comma"))
       (loop (cddr tokens) (cons (cadr (car tokens)) acc))]
      [(and (>= (length tokens) 2)
            (equal? (car (car tokens)) "rg_char")
            (equal? (car (car (cdr tokens))) "rg_listEnd"))
       (values #t (reverse (cons (cadr (car tokens)) acc)) (cdr (cdr tokens)))]
      [else (values #f '() '())]))
  (loop tokens '()))

(define (parse-exec-instruction tokens)
  (if (and (>= (length tokens) 3)
           (equal? (caar tokens) "rg_identifier")
           (equal? (caar (cdr tokens)) "rg_action")
           (equal? (caar (cddr tokens)) "rg_listStart"))
      (let* ([id (cadar tokens)]
             [action (cadar (cdr tokens))]
             [rest (cdddr tokens)])
        (define-values (ok? chars remaining)
          (parse-char-list rest))
        (if ok?
            (begin
              (printf "\nIdentificador: ~a\n" id)
              (printf "Acción detectada: ~a\n" action)
              (printf "Lista de caracteres: '~a\n" chars)
              (values `(run-automata ',(string->symbol id) ', chars) remaining))
            (error "en instrucción de ejecución")))
      (error "en la instrucción de ejecución")))

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
