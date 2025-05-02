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

