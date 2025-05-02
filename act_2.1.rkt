#lang racket
(define match-pos regexp-match-positions)

(define rg_int #rx"^[0-9]+")
(define rg_float #rx"^[0-9]+\\.[0-9]+")
(define rg_identifiers)
()
()
()

(define automaton_state #rx"^q[0-9]+")
(define start_automaton_definition #rx"^{")
(define automaton_name )