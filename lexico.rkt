#lang racket
;; Verificar el contenido del archivo acuerdo al léxico y sintaxis de tu lenguaje, utilizando expresiones regulares, tokenizacion y funciones
(define match-pos regexp-match-positions) ; acortar el nombre de la función regexp-match-positions

; --definición de léxico--
(define rg_int #rx"^[0-9]+")
(define rg_identifier #rx"^[a-zA-Z_][a-zA-Z0-9_]*$")
(define rg_char #rx"^.$")
(define rg_comment #rx"^//[ ]*.*")

(define rg_state #rx"^q[0-9]+")
(define rg_automatonDefStart #rx"^\\{")
(define rg_automatonDefEnd #rx"^\\}")
(define rg_nameDef #rx"^automaton[ ]*::[ ]*") ; [ ]* permite espacios entre la palabra reservada y ::
(define rg_alphabetDef #rx"^alphabet[ ]*::[ ]*")
(define rg_statesDef #rx"^states[ ]*::[ ]*")
(define rg_acceptStatesDef #rx"^accept[ ]*::[ ]*")
(define rg_transitionsDef #rx"^transitions[ ]*::[ ]*")

  ; pruebas de léxico
  (displayln "pruebas de léxico")
  (match-pos rg_nameDef "automaton ::") ; => '((0 . 12))
  (match-pos rg_alphabetDef "alphabet ::") ; => '((0 . 11))
  (match-pos rg_statesDef "states::") ; => '((0 . 8))
  (match-pos rg_transitionsDef "transitions  ::") ; => '((0 . 15))
  (match-pos rg_acceptStatesDef "accept :  :") ; => #f

; --definición de sintaxis--
(define expr_definition ; auxiliar
  (list #rx"^\\[" #rx"^.$" #rx"^,?" #rx"^.$" #rx"^\\]"))

(define name_definition 
  (list rg_nameDef rg_identifier))

(define alphabet_definition 
  (list rg_alphabetDef expr_definition))

(define states_definition 
  (list rg_statesDef rg_int))

(define accept_definition 
  (list rg_acceptStatesDef #rx"^q[0-9]+([ ]*,[ ]*q[0-9]+)*$" ))

(define transition_definition 
  (list rg_transitionsDef rg_state expr_definition rg_state))

(define automaton_definition 
  (list rg_automatonDefStart
        name_definition
        alphabet_definition
        states_definition
        accept_definition
        transition_definition
        rg_automatonDefEnd))

; --tokenización--
(define (match-sequence rg_list input)
  ; función auxiliar para eliminar los espacios en blanco al principio del texto
  (define (strip-leading-whitespace str)
    (regexp-replace #rx"^[ \t\n\r]*" str ""))
  ; función auxiliar
  (define (match-helper rg_list input)
    (cond
      [(null? rg_list) ; caso base
       (if (regexp-match #rx"^[ \t\n\r]*$" input)
          'success
          #f)]
      [else
        (let* ([current (car rg_list)] ; tomar el primer elemento de rg_list, quitar espacios y renombrar como current [let* permite]
          [input (strip-leading-whitespace input)])
          
          (cond
            ; si current es una expresión regular
            [(regexp? current)
            (let ([m (regexp-match current input)])
              (if (and m (not (null? m)))
                  (match-helper (cdr rg_list)
                                (substring input (string-length (car m))))
                  #f))]
            ; si current es una sublista (grupo de regex)
            [(list? current)
              (let ([result (match-helper current input)])
                (if (equal? result 'success)
                  (match-helper (cdr rg_list)
                    (strip-leading-whitespace ; eliminar espacios en blanco
                      (foldl (lambda (rx acc)
                        (let ([m (regexp-match rx acc)])
                          (if (and m (not (null? m)))
                            (substring acc (string-length (car m)))
                              acc)))
                              input
                              current)))
                  #f))]
           [else
            (error "Elemento inválido en rg_list" current)]))]))

  (match-helper rg_list input))

  ; pruebas de sintaxis
  (displayln "\npruebas de sintaxis")
  ;(match-sequence name_definition "automaton::automata123") ; => success
  ;(match-sequence name_definition "automaton :: auTom4ta") ; => success
  ;(match-sequence name_definition "automaton  ::automata") ; => success
  ;(match-sequence name_definition "invalid :: automata") ; => #f

  (match-sequence expr_definition "[\"-\", \"+\", \"0\"]")
  
  ;(match-sequence states_definition "states :: 3") ; => success
  (match-sequence alphabet_definition "alphabet :: [\"-\", \"+\", \"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"0\"]") ; => success
  (match-sequence accept_definition "accept :: q2") ; => success
  (match-sequence accept_definition "accept :: q2, q3") ; => success
  (match-sequence transition_definition "transitions :: (q0, [\"a\", \"x\", \"y\"], q1), (q1, [\"a\", \"x\", \"y\"], q1)") ; => success

; --errores--
; error general (e.g. errores de sintaxis)
; error falta un elemento en la definicion de algo (e.g. falta estado de aceptacion después de accept:: o falta transitions en la definicion del automata)

; --html--

;; Reportar errores en el contenido del txt. Ej: referencias a estados o símbolos que no existen, falta de estado final, falta de comas, etc
;; Simular el automata, siendo capaz de computar si acepta o no cadenas especificas (incluidas en el txt de entrada)
;; Devolver un .html, cuyo contenidos sea el texto del archivo de entrada, formateado según la sintaxis del leguaje y la salida
  ; Paleta de color: #1E1E2E, #89DCEB, #F28DB2, #A4D99C, #F2DBAE, #89B4FA, #CDD6F4, #F5403B or #F5613B, #72788C, #C6A7F2
  ; Fondo = #1E1E2E
  ; { } :: = #A4D99C (en su defecto intercambiar con palabras reservadas)
  ; ( ) [ ] , = #F28DB2
  ; chars "" = #F2DBAE
  ; palabras reservadas como automaton o states = #C6A7F2 (o intercambiar con { } ::)
  ; identifiers = #89B4FA or #89DCEB
  ; ints = #CDD6F4
  ; comments = #72788C
  ; error = subrayar ondulado #F5403B or #F5613B

; --simular el automata--
; automata2 -> ["-", "5", "7"] => success
; automata2 -> ["-"] => failed