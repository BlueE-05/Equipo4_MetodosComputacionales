#lang racket

(provide verify-sintaxis)

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
  (define (loop tokens)
    (cond
      [(null? tokens)
       (displayln "Verificación terminada.\n")]
      [else
       (define token (first tokens))
       (define label (first token))
       (cond
         [(equal? label "rg_automatonStart")
          (define rest (parse-one-automaton tokens))
          (loop rest)]
         [(and (equal? label "rg_identifier")
               (>= (length tokens) 2)
               (equal? (first (second tokens)) "rg_action"))
          (define rest (parse-exec-instruction tokens))
          (loop rest)]
         [else
          (error "Error: se esperaba el inicio de un autómata '{' o una instrucción de ejecución.")])]))
  (with-handlers ([exn:fail? 
                   (lambda (ex)
                     (printf "\nError detectado: ~a\n" (exn-message ex))
                     ;; Terminar sin crashear
                     (displayln "Verificación terminada.\n"))])
    (loop tokens)))

;; === Parsers ===

(define (parse-one-automaton tokens)
  (cond
    [(and (pair? tokens)
          (pair? (car tokens))
          (equal? (caar tokens) "rg_automatonStart"))
     (parse-name-def (cdr tokens))]
    [else (error "Error: se esperaba '{' al inicio del autómata.")]))

(define (parse-name-def tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_DFAname")
          (equal? (car (cadr tokens)) "rg_colon")
          (equal? (car (caddr tokens)) "rg_identifier"))
     (let ([id (cadr (caddr tokens))])
       (printf "\nNombre del autómata: ~a\n" id)
       (parse-alphabet-or-states (cdddr tokens)))]
    [else (error "Error de sintaxis en la definición de nombre del autómata")]))

(define (parse-alphabet-or-states tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_alphabet")
          (equal? (car (cadr tokens)) "rg_colon")
          (equal? (car (caddr tokens)) "rg_listStart"))
     (define-values (ok? remaining) (parse-char-list (cdddr tokens)))
     (if ok?
         (parse-states-def remaining)
         (error "Error en la lista de símbolos del alfabeto"))]

    [(and (pair? tokens)
          (equal? (car (car tokens)) "rg_statesDef"))
     (parse-states-def tokens)]

    [else (error "Error: se esperaba 'alphabet' o 'states' después del nombre del autómata")]))

(define (parse-states-def tokens)
  (cond
    [(and (>= (length tokens) 3)
          (equal? (car (car tokens)) "rg_statesDef")
          (equal? (car (cadr tokens)) "rg_colon")
          (equal? (car (caddr tokens)) "rg_int"))
     (let ([numero (cadr (caddr tokens))])
       (printf "Cantidad de estados: ~a\n" numero)
       (parse-alphabet-or-accept (cdddr tokens)))]
    [else (error "Error en la definición de estados")]))

(define (parse-alphabet-or-accept tokens)
  (cond
    ;; Si viene definición de alfabeto, debe venir luego accept
    [(and (pair? tokens)
          (equal? (car (first tokens)) "rg_alphabet"))
     (if (and (>= (length tokens) 3)
              (equal? (car (first tokens)) "rg_alphabet")
              (equal? (car (second tokens)) "rg_colon")
              (equal? (car (third tokens)) "rg_listStart"))
         (let-values ([(ok? _alphabet remaining) (parse-char-list (cdddr tokens))])
           (if ok?
               (parse-accept-def remaining)
               (error "Error en la lista de símbolos del alfabeto")))
         (error "Error de sintaxis en definición de alfabeto"))]

    ;; Si no hay alfabeto, debe venir directamente accept
    [(and (pair? tokens)
          (equal? (car (first tokens)) "rg_accept"))
     (parse-accept-def tokens)]

    [else
     (error "Error: se esperaba 'accept' o 'alphabet' después de la definición de estados")]))

(define (parse-accept-def tokens)
  (cond
    [(and (>= (length tokens) 2)
          (equal? (car (car tokens)) "rg_accept")
          (equal? (car (cadr tokens)) "rg_colon"))
     (let-values ([(ok? states remaining) (parse-state-list (cddr tokens))])
       (if ok?
           (begin
             (printf "Estados de aceptación: ~a\n" states)
             (parse-transitions-def remaining))
           (error "Error en la lista de estados de aceptación")))]
    [else (error "Error en la definición del estado de aceptación")]))

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

(define (parse-transitions-def tokens)
  (cond
    [(and (>= (length tokens) 2)
          (equal? (car (car tokens)) "rg_transitions")
          (equal? (car (cadr tokens)) "rg_colon"))
     (parse-transitions-list (cddr tokens))]
    [else (error "Error en la definición de 'transitions'")]))

(define (parse-transitions-list tokens)
  (let* ([tokens (skip-commas tokens)])
    (cond
      ;; Fin del bloque de transiciones
      [(and (pair? tokens)
            (equal? (car (car tokens)) "rg_automatonEnd"))
       (printf "Bloque de transiciones terminado correctamente.\n")
       (cdr tokens)]

      ;; Procesa una transición y continúa
      [else
       (define-values (ok? rest) (parse-one-transition tokens))
       (if ok?
           (parse-transitions-list rest)
           (error "Error en una transición"))])))

(define (parse-one-transition tokens)
  (cond
    [(and (>= (length tokens) 4)
          (equal? (car (car tokens)) "rg_parentesisStart")
          (equal? (car (cadr tokens)) "rg_state")
          (equal? (car (caddr tokens)) "rg_comma")
          (equal? (car (cadddr tokens)) "rg_listStart"))
     (let-values ([(ok? char-list tokens-rest) (parse-char-list (cddddr tokens))])
       (if (not ok?) (values #f '())
           (if (and (>= (length tokens-rest) 3)
                    (equal? (car (car tokens-rest)) "rg_comma")
                    (equal? (car (cadr tokens-rest)) "rg_state")
                    (equal? (car (caddr tokens-rest)) "rg_parentesisEnd"))
               (values #t (cdddr tokens-rest))
               (values #f '()))))]
    [else (values #f '())]))

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
             remaining)
           (error "Error en instrucción de ejecución")))]
    [else (error "Error en la instrucción de ejecución")]))

;; === Utilidades ===

(define (skip-commas tokens)
  (cond
    [(null? tokens) '()]
    [(equal? (first (first tokens)) "rg_comma")
     (skip-commas (cdr tokens))]
    [else tokens]))

;; === Ejemplo de uso (fuera del módulo): ===
;; (verify-sintaxis (filter-tokens tokens))
