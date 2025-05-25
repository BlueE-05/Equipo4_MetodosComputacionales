#lang racket

(require racket/match) ; Para patrones de match en listas

(provide verify-sintaxis)

;; Función principal: inicia el proceso sintáctico del archivo tokenizado.
;; Recorre todos los tokens y decide qué función de parsing llamar según el contexto.
(define (verify-sintaxis tokens)
  (cond
    [(null? tokens) (printf "Verificación terminada.\n")] ; Caso base: fin de archivo
    ;; Si inicia un autómata, procesa el bloque de autómata
    [(equal? (first (first tokens)) "rg_automatonStart")
     (define rest (parse-one-automaton tokens))
     (verify-sintaxis rest)]
    ;; Salta saltos de línea
    [(string=? (first (first tokens)) "\n")
     (verify-sintaxis (cdr tokens))]
    ;; Salta comentarios
    [(equal? (first (first tokens)) "rg_comment")
     (verify-sintaxis (cdr tokens))]
    ;; Si detecta una instrucción de ejecución (ej: automata2 -> [...])
    [(and (equal? (first (first tokens)) "rg_identifier")
          (>= (length tokens) 2)
          (equal? (first (second tokens)) "rg_action"))
     (define rest (parse-exec-instruction tokens))
     (verify-sintaxis rest)]
    ;; Si no coincide ningún patrón, lanza error
    [else (error "Error: se esperaba el inicio de un autómata '{' o fin de archivo.")]))

;; Paso 1: automaton :: nombre
;; Procesa la definición del nombre del autómata y delega a la siguiente sección
(define (parse-name-def tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      ;; Si encuentra la sección de nombre correctamente
      [(list `("rg_name" ,_)
             `("rg_colon" ,_)
             `("rg_identifier" ,id)
             rest ...)
       (printf "Nombre del autómata: ~a\n" id)
       (parse-alphabet-or-states rest)]
      [else (error "Error de sintaxis en la definición de nombre del autómata")])))

;; Decide si después del nombre sigue la sección alphabet o directamente states
(define (parse-alphabet-or-states tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      ;; Si hay sección alphabet, procesa la lista de símbolos
      [(list `("rg_alphabet" ,_)
             `("rg_colon" ,_)
             `("rg_listStart" ,_)
             rest ...)
       (define-values (ok? remaining) (parse-char-list rest))
       (if ok?
           (parse-states-def remaining)
           (error "Error en la lista de símbolos del alfabeto"))]
      ;; Si no hay sección alphabet, sigue con states
      [(list `("rg_statesDef" ,_) rest ...)
       (parse-states-def tokens)]
      [else (error "Error: se esperaba 'alphabet' o 'states' después del nombre del autómata")])))

;; Paso 2: alphabet :: [ 'a', 'b', ... ]
;; Procesa la sección de alfabeto (no se usa si el alfabeto es opcional)
(define (parse-alphabet-def tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (printf "Tokens en parse-alphabet-def: ~a\n" tokens) ; Línea de depuración
    (match tokens
      [(list `("rg_alphabet" ,_)
             `("rg_colon" ,_)
             `("rg_listStart" ,_)
             rest ...)
       (define-values (ok? remaining) (parse-char-list rest))
       (if ok?
           (parse-states-def remaining)
           (error "Error en la lista de símbolos del alfabeto"))]
      [else (error "Error en la definición de 'alphabet'")])))

;; Función auxiliar para procesar listas de caracteres del alfabeto o transiciones
;; Devuelve dos valores: #t y el resto de tokens si es correcto, #f y lista vacía si hay error
(define (parse-char-list tokens)
  (match tokens
    ;; Fin de la lista de caracteres
    [(list `("rg_listEnd" ,_) rest ...) (values #t rest)]
    ;; Caracter seguido de coma, sigue procesando
    [(list `("rg_char" ,_) `("rg_comma" ,_) rest ...)
     (parse-char-list rest)]
    ;; Último caracter antes de cerrar la lista
    [(list `("rg_char" ,_) `("rg_listEnd" ,_) rest ...)
     (values #t rest)]
    ;; Cualquier otro caso es error
    [else (values #f '())]))

;; Paso 3: states :: cantidad
;; Procesa la sección de cantidad de estados
(define (parse-states-def tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      [(list `("rg_statesDef" ,_)
             `("rg_colon" ,_)
             `("rg_int" ,numero)
             rest ...)
       (printf "Cantidad de estados: ~a\n" numero)
       (parse-accept-def rest)]
      [else (error "Error en la definición de estados")])))

;; Paso 4: accept :: qX
;; Procesa la sección del estado de aceptación
(define (parse-accept-def tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      [(list `("rg_accept" ,_)
             `("rg_colon" ,_)
             `("rg_state" ,estado)
             rest ...)
       (printf "Estado de aceptación: ~a\n" estado)
       (parse-transitions-def rest)]
      [else (error "Error en la definición del estado de aceptación")])))

;; Paso 5: transitions :: (...)
;; Procesa la sección de transiciones
(define (parse-transitions-def tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      [(list `("rg_transitions" ,_)
             `("rg_colon" ,_)
             rest ...)
       (parse-transitions-list rest)]
      [else (error "Error en la definición de 'transitions'")])))

;; Recorrido de la lista de transiciones, procesa cada transición individualmente
(define (parse-transitions-list tokens)
  (let* ([tokens (skip-nl-comments tokens)]
         [tokens (skip-commas tokens)]) ; Salta comas extra entre transiciones
    (match tokens
      ;; Fin del bloque de transiciones
      [(list `("rg_automatonEnd" ,_) rest ...)
       (printf "Bloque de transiciones terminado correctamente.\n")
       rest]
      ;; Procesa una transición y sigue con el resto
      [_
       (define-values (ok? rest) (parse-one-transition tokens))
       (if ok?
           (parse-transitions-list rest)
           (error "Error en una transición"))])))

;; Función auxiliar para saltar comas entre transiciones
(define (skip-commas tokens)
  (cond
    [(null? tokens) '()]
    [(equal? (first (first tokens)) "rg_comma")
     (skip-commas (cdr tokens))]
    [else tokens]))

;; Verifica una sola transición (ej: (q0, ["a", "b"], q1))
;; Espera el patrón: (rg_parentesisStart ...) (rg_state ...) (rg_comma ,) (rg_listStart ...) ... (rg_comma ,) (rg_state ...) (rg_parentesisEnd ...)
(define (parse-one-transition tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      [(list `("rg_parentesisStart" ,_)
             `("rg_state" ,estado1)
             `("rg_comma" ,_)
             `("rg_listStart" ,_)
             rest ...)
       ;; Procesa la lista de caracteres de la transición
       (define-values (ok? after-list) (parse-char-list rest))
       (if (not ok?) (values #f '())
           (match after-list
             [(list `("rg_comma" ,_)
                    `("rg_state" ,estado2)
                    `("rg_parentesisEnd" ,_)
                    maybe-comma-or-nl ...)
              (values #t maybe-comma-or-nl)]
             [else (values #f '())]))]
      ;; Si no coincide el patrón, error
      [else (values #f '())])))

;; Paso 6: ejecución :: nombre_accion ( ... )
;; Procesa una instrucción de ejecución (ej: automata2 -> ["-", "5", "7"])
(define (parse-exec-instruction tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      [(list `("rg_identifier" ,_) `("rg_action" ,_) `("rg_listStart" ,_) rest ...)
       (define-values (ok? remaining) (parse-char-list rest))
       (if ok? remaining (error "Error en instrucción de ejecución"))]
      [else tokens])))

;; Salta comentarios y nuevas líneas en la lista de tokens
(define (skip-nl-comments tokens)
  (cond
    [(null? tokens) '()]
    [(or (string=? (first (first tokens)) "\n")
         (equal? (first (first tokens)) "rg_comment"))
     (skip-nl-comments (cdr tokens))]
    [else tokens]))

;; Procesa un bloque de autómata completo, esperando que inicie con '{'
(define (parse-one-automaton tokens)
  (let ([tokens (skip-nl-comments tokens)])
    (match tokens
      [(list `("rg_automatonStart" ,_) rest ...)
       (parse-name-def rest)]
      [else (error "Error: se esperaba '{' al inicio del autómata.")])) 
)