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
  (define instrucciones '())
  (define (loop tokens)
    (cond
      [(null? tokens)
       (displayln "\n[✓] Verificación terminada.\n")
       (reverse instrucciones)] ; devolver las instrucciones
      [else
       (define token (first tokens))
       (define label (first token))
       (cond
         [(equal? label "rg_automatonStart")
          (define-values (inst rest) (parse-one-automaton tokens))
          (set! instrucciones (cons inst instrucciones))
          (loop rest)]
         [(and (equal? label "rg_identifier")
               (>= (length tokens) 2)
               (equal? (first (second tokens)) "rg_action"))
          (define-values (inst rest) (parse-exec-instruction tokens))
          (set! instrucciones (cons inst instrucciones))
          (loop rest)]
         [else
          (error "se esperaba el inicio de un autómata '{' o una instrucción de ejecución.")])]))
  (with-handlers ([exn:fail?
                   (lambda (ex)
                     (define mensaje (format "[!] Sintaxis error: ~a" (exn-message ex)))
                     (displayln mensaje)
                     (displayln "[✓] Verificación terminada.\n")
                     '())])
    (loop tokens)))

;; === Parsers ===

(define (parse-one-automaton tokens)
  (cond
    [(and (pair? tokens)
          (pair? (car tokens))
          (equal? (caar tokens) "rg_automatonStart"))
     (parse-name-def (cdr tokens))]
    [else (error "se esperaba '{' al inicio del autómata.")]))

(define (parse-name-def tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_DFAname")
          (equal? (car (cadr tokens)) "rg_colon")
          (equal? (car (caddr tokens)) "rg_identifier"))
     (let* ([id (cadr (caddr tokens))]
            [name (string->symbol id)]
            [rest (cdddr tokens)])
       (printf "\nNombre del autómata: ~a\n" id)
       (define-values (alfabeto states accept transiciones resto)
         (parse-alphabet-or-states rest))
       (values
        `(create-automata ',name 'DFA ,states ',accept ',transiciones ,''())
        resto)]
    [else (error "en la definición de nombre del autómata")]))


(define (parse-alphabet-or-states tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_alphabet")
          (equal? (car (cadr tokens)) "rg_colon")
          (equal? (car (caddr tokens)) "rg_listStart"))
     (define-values (ok? alfabeto remaining) (parse-char-list (cdddr tokens)))
     (if ok?
         (parse-states-def remaining alfabeto)
         (error "en la lista de símbolos del alfabeto"))]

    [(and (pair? tokens)
          (equal? (car (car tokens)) "rg_statesDef"))
     (parse-states-def tokens '())] ; sin alfabeto explícito

    [else (error "se esperaba 'alphabet' o 'states' después del nombre del autómata")]))

(define (parse-states-def tokens alfabeto)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_statesDef")
          (equal? (car (cadr tokens)) "rg_colon")
          (equal? (car (caddr tokens)) "rg_int"))
     (let* ([numero (string->number (cadr (caddr tokens)))]
            [rest (cdddr tokens)])
       (printf "Cantidad de estados: ~a\n" numero)
       (parse-alphabet-or-accept rest alfabeto numero))]
    [else (error "en la definición de estados")]))

(define (parse-alphabet-or-accept tokens alfabeto estados)
  (cond
    [(and (pair? tokens)
          (equal? (car (first tokens)) "rg_alphabet"))
     (if (and (>= (length tokens) 3)
              (equal? (car (first tokens)) "rg_alphabet")
              (equal? (car (second tokens)) "rg_colon")
              (equal? (car (third tokens)) "rg_listStart"))
         (let-values ([(ok? new-alfabeto remaining) (parse-char-list (cdddr tokens))])
           (if ok?
               (parse-accept-def remaining new-alfabeto estados)
               (error "en la lista de símbolos del alfabeto")))
         (error "en definición de alfabeto"))]

    [(and (pair? tokens)
          (equal? (car (first tokens)) "rg_accept"))
     (parse-accept-def tokens alfabeto estados)]

    [else (error "se esperaba 'accept' o 'alphabet' después de la definición de estados")]))

(define (parse-accept-def tokens alfabeto estados)
  (cond
    [(and (>= (length tokens) 2)
          (equal? (car (car tokens)) "rg_accept")
          (equal? (car (cadr tokens)) "rg_colon"))
     (let-values ([(ok? accept remaining) (parse-state-list (cddr tokens))])
       (if ok?
           (begin
             (printf "Estados de aceptación: ~a\n" accept)
             (parse-transitions-def remaining alfabeto estados accept))
           (error "en la lista de estados de aceptación")))]
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
  (cond
    [(and (>= (length tokens) 2)
          (equal? (car (car tokens)) "rg_transitions")
          (equal? (car (cadr tokens)) "rg_colon"))
     (define-values (ok? transiciones remaining) (parse-transitions-list (cddr tokens) '()))
     (if ok?
         (values alfabeto estados accept transiciones remaining)
         (error "en la definición de 'transitions'"))]
    [else (error "en la definición de 'transitions'")]))

(define (parse-transitions-list tokens acc)
  (let* ([tokens (skip-commas tokens)])
    (cond
      [(and (pair? tokens)
            (equal? (car (car tokens)) "rg_automatonEnd"))
       (printf "Bloque de transiciones terminado correctamente.\n")
       (values #t (reverse acc) (cdr tokens))]

      [else
       (define-values (ok? transicion rest) (parse-one-transition tokens))
       (if ok?
           (parse-transitions-list rest (cons transicion acc))
           (values #f '() '()))])))

(define (parse-one-transition tokens)
  (cond
    [(and (>= (length tokens) 4)
          (equal? (car (car tokens)) "rg_parentesisStart")
          (equal? (car (cadr tokens)) "rg_state")
          (equal? (car (caddr tokens)) "rg_comma")
          (equal? (car (cadddr tokens)) "rg_listStart"))
     (let-values ([(ok? chars rest1) (parse-char-list (cddddr tokens))])
       (if (not ok?) (values #f '() '())
           (if (and (>= (length rest1) 3)
                    (equal? (car (car rest1)) "rg_comma")
                    (equal? (car (cadr rest1)) "rg_state")
                    (equal? (car (caddr rest1)) "rg_parentesisEnd"))
               (values #t
                       (list (cadr (cadr tokens)) chars (cadr (cadr rest1)))
                       (cdddr rest1))
               (values #f '() '()))))]
    [else (values #f '() '())]))

(define (parse-char-list tokens)
  (define (loop tokens acc)
    (cond
      ;; Fin de lista
      [(and (pair? tokens)
            (equal? (car (car tokens)) "rg_listEnd"))
       (values #t (reverse acc) (cdr tokens))]

      ;; Caracter seguido de coma
      [(and (>= (length tokens) 2)
            (equal? (car (car tokens)) "rg_char")
            (equal? (car (cadr tokens)) "rg_comma"))
       (loop (cddr tokens) (cons (cadr (car tokens)) acc))]

      ;; Último carácter antes de cerrar lista
      [(and (>= (length tokens) 2)
            (equal? (car (car tokens)) "rg_char")
            (equal? (car (cadr tokens)) "rg_listEnd"))
       (values #t (reverse (cons (cadr (car tokens)) acc)) (cdr (cadr tokens)))]

      [else (values #f '() '())]))
  (loop tokens '()))

(define (parse-exec-instruction tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_identifier")
          (equal? (car (cadr tokens)) "rg_action")
          (equal? (car (caddr tokens)) "rg_listStart"))
     (let-values ([(ok? chars remaining) (parse-char-list (cdddr tokens))])
       (if ok?
           (begin
             (printf "\nIdentificador: ~a\n" (cadr (car tokens)))
             (printf "Acción detectada: ~a\n" (cadr (cadr tokens)))
             (printf "Lista de caracteres: '~a\n" chars)
             (values `(run-automata ',(string->symbol (cadr (car tokens))) ',chars) remaining))
           (error "en instrucción de ejecución")))]
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
    (namespace-require '(file "automaton.rkt")) ; <-- incluir el módulo
    (for-each
     (lambda (inst)
       (eval inst)) ; ya no se necesita pasar el namespace explícitamente
     instrucciones)))

;; === Ejemplo de uso (fuera del módulo): ===
;; (verify-sintaxis (filter-tokens tokens))
