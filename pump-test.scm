(use test scsh-process)
(declare (uses extras))
(include "pump-impl")

(define-syntax probe
  (syntax-rules ()
    [(probe var)
     (let ([val var])
       (printf "~s is ~s\n" 'var val)
       (flush-output)
       val)]))

(test "base directory is ." "." (base-directory))

(parameterize ([base-directory "foo"])
  (test "base directory is foo" "foo" (base-directory)))

(let ([root-dir (create-temporary-directory)])
  (test-assert "simple create-directory-tree works"
    (begin
      (create-directory-tree root-dir '(foo (bar)))
      (and (directory? (make-pathname root-dir "foo"))
           (regular-file? (make-pathname (list root-dir "foo") "bar")))))
  (delete-directory root-dir #:recurse))

(let ([root-dir (create-temporary-directory)]
      [perm #o755])
  (test-assert "create-directory-tree file permissions work"
        (begin
          (create-directory-tree root-dir `(foo #:mode ,perm))
          (= perm
             (bitwise-and (file-permissions (make-pathname root-dir "foo"))
                          perm))))
  (delete-directory root-dir #:recurse))

(let ([root-dir (create-temporary-directory)]
      [str "hi there\n"])
  (test "create-directory-tree with string file content works"
        str
        (begin
          (create-directory-tree root-dir `(foo ,str))
          (with-input-from-file (make-pathname root-dir "foo") read-string)))
  (delete-directory root-dir #:recurse))

(let* ([root-dir (create-temporary-directory)]
       [str "hi there\n"]
       [proc (lambda (port) (display str port))])
  (test "create-directory-tree with proc for file content works"
        str
        (begin
          (create-directory-tree root-dir `(foo ,proc))
          (with-input-from-file (make-pathname root-dir "foo") read-string)))
  (delete-directory root-dir #:recurse))

(let ([root-dir (create-temporary-directory)]
      [perm #o777])
  (test "create-directory-tree directory permissions work"
        perm
        (begin
          (create-directory-tree root-dir `(foo #:mode ,perm ()))
          (bitwise-and perm
                       (file-permissions (make-pathname root-dir "foo"))))))

(let ([root-dir (create-temporary-directory)])
  (test-assert "simple check-directory-tree works"
    (begin
      (create-directory-tree root-dir '(foo (bar)))
      (check-directory-tree root-dir '(foo (bar)))))
  (delete-directory root-dir #:recurse))

(let ([root-dir (create-temporary-directory)])
  (test-assert "check-directory-tree with #:mode works"
    (begin
      (create-directory-tree root-dir '(foo #:mode #o777))
      (check-directory-tree root-dir '(foo #:mode #o777))))
  (delete-directory root-dir #:recurse))

(let ([root-dir (create-temporary-directory)]
      [spec '(foo (bar (baz #:symlink "bar")))])
  (test-assert "check-direcotry-tree with #:symlink works"
    (begin
      (create-directory-tree root-dir spec)
      (check-directory-tree root-dir spec)
      ))
  (delete-directory root-dir #:recurse))

;; add tests for check-directory-tree
;; mode
;; owner
;; group
;; symlink

(let ([root-dir (create-temporary-directory)]
      [spec '(foo ((bar ((baz (file))))))])
  (test-assert "create-directory-tree for files and folders works"
    (begin
      (create-directory-tree root-dir spec)
      (check-directory-tree root-dir spec)))
  (delete-directory root-dir #:recurse))

(parameterize ([base-directory (create-temporary-directory)])
  (define (strings-sort l) (sort l string<?))
  (do-ec (:gen n (make-iota-generator 10))
         (run (touch ,(make-pathname (base-directory) (sprintf "foo-~s.tmp" n)))))
  (run (mkdir ,(make-pathname (base-directory) "foo-0-dir")))
  (run (mkdir ,(make-pathname (list (base-directory) "foo-0-dir") "foo-0.tmp")))
  (run (touch ,(make-pathname (base-directory) ".hidden-foo-0.tmp")))
  (run (touch ,(make-pathname (list (base-directory) "foo-0-dir") ".hidden-foo-0.tmp")))
  (run (touch ,(make-pathname (list (base-directory) "foo-0-dir") "foo-1.tmp")))
  (run (ln -s ,(make-pathname (base-directory) "foo-0.tmp") ,(make-pathname (base-directory) "foo-0.lnk")))
  (run (ln -s ,(make-pathname (base-directory) "foo-0-dir") ,(make-pathname (base-directory) "foo-dir-link")))
  (test "rgx PCRE works"
        (map (cut make-pathname (base-directory) <>) '("foo-0.tmp" "foo-1.tmp"))
        (strings-sort (generator->list (rgx "foo-[01]\\.tmp$"))))
  (test "rgx SRE works"
        (map (cut make-pathname (base-directory) <>) '("foo-0.tmp" "foo-1.tmp"))
        (strings-sort (generator->list (rgx '(: "foo-" ("01") ".tmp" eos)))))
  (test "rgx directory filter works"
        (list (make-pathname (base-directory) "foo-0-dir"))
        (strings-sort (generator->list (rgx "" #:type 'directory))))
  (test "rgx file filter works"
        (list (make-pathname (base-directory) "foo-0.tmp"))
        (strings-sort (generator->list (rgx '(: bos "foo-0.tmp" eos) #:type 'regular-file))))
  (test "rgx recursion works"
        (list (make-pathname (list (base-directory) "foo-0-dir")
                             "foo-0.tmp")
              (make-pathname (base-directory) "foo-0.tmp"))
        (strings-sort (generator->list (rgx "^foo-0\\.tmp$" #:recurse #t))))
  (test "rgx dotfiles work"
        (list (make-pathname (base-directory) ".hidden-foo-0.tmp"))
        (strings-sort (generator->list (rgx '(: bos ".") #:dotfiles #t))))
  (test "rgx recursive dotfiles work"
        (list (make-pathname (base-directory) ".hidden-foo-0.tmp")
              (make-pathname (list (base-directory) "foo-0-dir")
                             ".hidden-foo-0.tmp"))
        (strings-sort (generator->list (rgx '(: bos ".") #:dotfiles #t #:recurse #t))))
  (test "rgx follow-symlinks works"
        (list (make-pathname (base-directory) "foo-0.lnk"))
        (strings-sort (generator->list (rgx '(: "lnk" eos) #:follow-symlinks #t))))
  (test "rgx follow-symlinks with directories works"
        (list (make-pathname (list (base-directory) "foo-0-dir") "foo-1.tmp")
              (make-pathname (base-directory) "foo-1.tmp")
              (make-pathname (list (base-directory) "foo-dir-link") "foo-1.tmp"))
        (strings-sort (generator->list (rgx '(: bos "foo-1.tmp" eos) #:type 'regular-file #:follow-symlinks #t #:recurse #t))))
  (delete-directory (base-directory) #:recurse))

(test-exit)
