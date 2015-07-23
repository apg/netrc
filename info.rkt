#lang info
(define collection "netrc")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/netrc.scrbl" ())))
(define pkg-desc "find entries in .netrc files")
(define version "0.1")
(define pkg-authors '(apg))
