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
        (strings-sort (generator->list (rgx "foo-0" #:type 'regular-file))))
  (test "rgx recursion works"
        (list (make-pathname (base-directory) "foo-0.tmp")
              (make-pathname (list (base-directory) "foo-0-dir")
                             "foo-0.tmp"))
        (strings-sort (generator->list (rgx "^foo-0\\.tmp$" #:recurse #t))))
  (delete-directory (base-directory) #:recurse))
