#lang racket
(define match-pos regexp-match-positions)

(define rg_int #rx"^[0-9]+")
(define rg_float #rx"^[0-9]+\\.[0-9]+")
(define rg_identifiers #rx"^[a-zA-Z_]\\w*\\b")
(define rg_string #rx"^\\"".*?")
(define rg_numReal #rx"^-?\\d*\\.\\d+(?:[eE][-+]?\\d+)?\\b")

(define rg_addition #rx"^\\+")
(define rg_subtraction #rx"^-")
(define rg_multiplication #rx"^\\*")
(define rg_division #rx"^/")

(define rg_exp #rx"^exp")
(define rg_sqr #rx"^sqr")


(define automaton_state #rx"^q[0-9]+")
(define automaton_transitions #rx"^#px"\\( automaton_state \\, exprReg \\, automaton_state \\)") ; to be defined

  
(define start_automaton_definition #rx"^{")
(define end_automaton_definition #rx"^}")
(define automaton_name_definition #rx"^automaton\\:\\:")
(define automaton_alphabet_definition #rx"^alphabet\\:\\:")
(define automaton_states_definition #rx"^states\\:\\:")
(define automaton_accept_definition #rx"^accept\\:\\:")
(define automaton_transition_definition #rx"^transitions\\:\\:")

(define rg_assign #rx"^->")
(define rg_compare #rx"^is")
(define rg_lessthan #rx"^<")
(define rg_grthan #rx"^>")
(define rg_lessequal_than #rx"^<=")
(define rg_grtequal_than #rx"^>=")
(define rg_different #rx"^isNot")
(define rg_and #rx"^and")
(define rg_or #rx"^or")
(define rg_not #rx"^not")