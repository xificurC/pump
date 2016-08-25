(import chicken scheme)
(use srfi-42 srfi-121 posix)
(declare (uses files))

(keyword-style #:prefix)

(define-syntax :gen
  (syntax-rules (index)
    [(_ cc var (index i) expr)
     (:parallel cc (:gen var expr) (:integers i))]
    [(_ cc var expr)
     (:do cc
          (let ([gen expr]))
          ([var (gen)])
          (not (eof-object? var))
          (let ())
          #t
          [(gen)])]))

;; TODO globbing with two stars **

;; TODO add guard that checks if directory exists
(define base-directory (make-parameter "."))

(define (dir str)
  "Sets the directory parameter"
  (base-directory str))

(define (rgx irx #!key recurse dotfiles follow-symlinks type)
  "Find files matching regular expression"
  (define entries (directory (base-directory) dotfiles))
  (define (path-of entry) (make-pathname (base-directory) entry))
  (define (drop-non-matching l)
    (drop-while (lambda (e)
                  (or (and type (not (eq? type (file-type (path-of e)))))
                      (not (irregex-search irx e))))
                l))
  (make-unfold-generator null?
                         car
                         (compose drop-non-matching cdr)
                         (drop-non-matching entries)))

(define (pump)
  "TODO")
