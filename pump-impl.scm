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

;; (define (with-current-directory dir thunk)
;;   (let ([start-dir (current-directory)])
;;     (dynamic-wind
;;       (lambda () (current-directory dir))
;;       thunk
;;       (lambda () (current-directory start-dir)))))

(define (create-file name)
  (with-output-to-file name (constantly #t)))

(define (create-directory-tree dir spec)
  (match spec
    [(? string? s) (create-file (make-pathname dir s))]
    [(? symbol? s) (create-directory-tree dir (symbol->string s))]
    [(subdir subspecs)
     (let ([subdir (make-pathname dir
                                  (if (string? subdir)
                                      subdir
                                      (symbol->string subdir)))])
       (create-directory subdir)
       (for-each (cut create-directory-tree subdir <>) subspecs))]))

(define (all? proc lst)
  (match lst
    ['() #t]
    [(x . xs) (or (proc x) (all? proc xs))]))

(define (check-directory-tree dir spec)
  (match spec
    [(? string? s) (regular-file? s)]
    [(? symbol? s) (check-directory-tree dir (symbol->string s))]
    [(subdir subspecs)
     (let ([subdir (make-pathname dir
                                  (if (string? subdir)
                                      subdir
                                      (symbol->string subdir)))])
       (and (directory? subdir)
            (all? (cut check-directory-tree subdir <>) subspecs)))]))

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
  (define (drop-non-matching l)
    (let loop ([l l])
      (match l
        [(f . r) (if (and (or (not type)
                              (if (and (eq? type 'directory) (not follow-symlinks))
                                  (and (directory? f) (not (symbolic-link? f)))
                                  (eq? type (file-type f)))
                              (and (symbolic-link? f) follow-symlinks))
                          (irregex-search irx (pathname-strip-directory f)))
                     (cons f (recurse-append l))
                     (loop (recurse-append l)))]
        ['() '()])))
  (define (recurse-append l)
    (if recurse
        (match l
          [((? directory? f) . r)
           (if (and (symbolic-link? f) (not follow-symlinks))
               r
               (append r (map (cut make-pathname f <>) (directory f dotfiles))))]
          [(f . r)
           r])
        (cdr l)))
  (make-unfold-generator null?
                         car
                         (o drop-non-matching cdr)
                         (drop-non-matching entries)))

(define (pump)
  "TODO")
