;;; Definition of functions that will be used to get MIME types.

;; Fetch the funnies.
(load "file-al.scm")
(use-modules (ice-9 regex))

;; Gotta store them somewhere!, arbitrary number btw.
(define *mime-types* (make-hash-table 1000))

;; Regex we shall use.
(define name-regex (make-regexp "[A-Za-z/]+"))
(define white-regex (make-regexp "^[[:blank:]]*$"))

;; Nice utility, should simplify the following parse-line function somewhat
(define (regexp-exec+substring the-regex the-string . the-flags)
  (let ((the-output (apply regexp-exec the-regex the-string the-flags)))
    (if the-output
        (letrec ((substring-bounds (array-ref the-output 1)) ; I bothered enough to get a substring
                                                             ; as output too.
                 (lower-bound (car substring-bounds))
                 (upper-bound (cdr substring-bounds)))
          (list (array-ref the-output 0)
                (array-ref the-output 1)
                (substring the-string lower-bound upper-bound)))
        #f))) ; If the regex returned false return false too.
