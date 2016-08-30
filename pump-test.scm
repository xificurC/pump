(use test scsh-process)
(declare (uses extras))
(include "pump-impl")

(test "base directory is ." "." (base-directory))

(parameterize ([base-directory "foo"])
  (test "base directory is foo" "foo" (base-directory)))

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
