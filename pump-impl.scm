(import chicken scheme)
(use srfi-42 srfi-121 matchable)
(declare (uses files posix))

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
  (define (filename? s) (or (string? s) (symbol? s)))
  (match spec
    [(? filename? file)
     (create-file (make-pathname dir (->string file)))]
    [((? filename? path) . options)
     (let ([path (make-pathname dir (->string path))])
       (let loop ([args options]
                  [mode 0]
                  [owner #f]
                  [group #f]
                  [symlink #f]
                  [thunk #f])
         (match args
           [('#:mode mode . rest)
            (loop rest mode owner group symlink thunk)]
           [('#:owner owner . rest)
            (loop rest mode owner group symlink thunk)]
           [('#:group group . rest)
            (loop rest mode owner group symlink thunk)]
           [('#:symlink sympath . rest)
            (loop rest mode owner group sympath thunk)]
           [((? procedure? proc))
            (loop '() mode owner group symlink
                  (lambda ()
                    (call-with-output-file path proc)))]
           [((? string? str))
            (loop '() mode owner group symlink
                  (lambda ()
                    (with-output-to-file path (lambda () (display str)))))]
           ['()
            (let ([thunk (or thunk (lambda ()
                                     (with-output-to-file path (constantly #t))))])
              (if symlink
                  (create-symbolic-link symlink path)
                  (begin
                    (thunk)
                    (when (not (zero? mode))
                      (change-file-mode path mode))
                    (when (or owner group)
                      (let ([owner (or owner (current-user-id))]
                            [group (or group (current-group-id))])
                        (change-file-owner path owner group))))))]
           [(subspecs)
            (create-directory path)
            (when (not (zero? mode))
              (change-file-mode path mode))
            (when (or owner group)
              (let ([owner (or owner (current-user-id))]
                    [group (or group (current-group-id))])
                (change-file-owner path owner group)))
            (for-each (cut create-directory-tree path <>) subspecs)])))]))

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
