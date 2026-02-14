#lang info

(define collection "rcron")

(define scribblings (list (list "main.scrbl" (list) (list 'library) "rcron")))

(define deps (list "rcron-lib" "base"))

(define build-deps (list "racket-doc" "scribble-lib" "rackunit-lib"))

(define license 'Apache-2.0)
