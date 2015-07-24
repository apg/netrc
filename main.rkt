#lang racket/base

(provide netrc-find-entry
         (struct-out netrc-entry))

(require racket/match
         racket/string
         racket/port)

(struct netrc-entry (machine login password)
        #:transparent)

(define (read-netrc #:netrc [netrc ""])
  (define path (expand-user-path (if (equal? netrc "") "~/.netrc" netrc)))
  (if (file-exists? path)
      (port->string (open-input-file path))
      ""))

(define (tokenize input-string)
  (string-split input-string #:trim? #t))

(define (parse tokens)
  (let loop ((tokens tokens)
             (entries '())
             (machine "")
             (login "")
             (password ""))
    (cond
     [(andmap (lambda (x) (not (equal? x ""))) (list machine login password))
      (loop tokens (cons (netrc-entry machine login password) entries) "" "" "")]
     [(null? tokens) entries]
     [else (match tokens
             [(list-rest "machine" name rest) (loop rest entries name login password)]
             ;; default is really just a special case of `machine`. Technically it has to
             ;; come last, but shh... for now it's fine.
             [(list-rest "default" rest) (loop rest entries "default" login password)]
             [(list-rest "login" name rest) #:when (not (equal? machine ""))
              (loop rest entries machine name password)]
             [(list-rest "password" password rest) #:when (not (equal? machine ""))
              (loop rest entries machine login password)]
             [else entries])])))

(define (netrc-find-entry machine #:use-default [use-default #f])
  (let loop ((entries (parse (tokenize (read-netrc))))
             (default #f))
    (cond
     [(null? entries) (and use-default default)]
     [(equal? (netrc-entry-machine (car entries)) machine) (car entries)]
     ;; if we discover the default, loop, but save it off for failure case.
     [(equal? (netrc-entry-machine (car entries)) "default") (loop (cdr entries) (car entries))]
     [else (loop (cdr entries) default)])))

;; would like to move this elsewhere, really.
(module+ test
  (require rackunit)

  (test-case "Test simple netrc"
             (define sample (tokenize "machine foo.bar login user password password"))
             (define entries (parse sample))
             (check = (length entries) 1)

             (define entry (car entries))

             (check-equal? (netrc-entry-machine entry) "foo.bar")
             (check-equal? (netrc-entry-login entry) "user")
             (check-equal? (netrc-entry-password entry) "password")))
