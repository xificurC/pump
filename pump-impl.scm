(import chicken scheme)
(use srfi-42 srfi-121 posix matchable)
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
  (define entries (map (cut make-pathname (base-directory) <>)
                       (directory (base-directory) dotfiles)))
  ;; TODO drop-non-matching discards directories but we might want to recurse
  ;; so we probably cannot use drop-while and should use recurse-append somewhere
  (define (drop-non-matching l)
    (drop-while (lambda (e)
                  (or (and type (not (eq? type (file-type e))))
                      (not (irregex-search irx (pathname-strip-directory e)))))
                l))
  (define (recurse-append l)
    (if recurse
        (match l
          [((? directory? f) . r)
           (append r (map (cut make-pathname f <>) (directory f)))]
          [(f . r)
           (display (string-append f " is not a directory"))
           r])
        (cdr l)))
  (make-unfold-generator null?
                         car
                         (compose drop-non-matching recurse-append)
                         (drop-non-matching entries)))

(define (pump)
  "TODO")
