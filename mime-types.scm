;;; Definition of functions that will be used to get MIME types.

;; Fetch the funnies.
(load "file-al.scm")
(use-modules (ice-9 regex))

;; Gotta store them somewhere!, arbitrary number btw.
(define *mime-types* (make-hash-table 1000))

;; Regex we shall use.
(define name-regexp (make-regexp "[A-Za-z/\\.]+"))
(define white-regexp (make-regexp "^[[:blank:]]*$"))

;; Match a single line & return the substring & the suffix, in the following
;; structure: (substring suffix), returning #f if there was no match.
(define (regexp-fetch compiled-regexp the-string)
  (let ((matched-regexp (regexp-exec compiled-regexp the-string)))
    (if matched-regexp
        (list (match:substring matched-regexp)
              (match:suffix matched-regexp))
        #f)))

;; Iterate & return a list of the elements that form the string in terms of the provided
;; regexp.
(define (regexp-scan compiled-regexp the-string)
  (let ((the-output (cons '() '()))) ; I must LOOOOOOOOOOOOOP.
    (let iteration ((initial-fetch (regexp-fetch compiled-regexp the-string)))
      (if initial-fetch
          (let ((the-substring (car initial-fetch))
                (rest-string (caddr initial-fetch)))
            (list-add! the-output the-substring)
            (iteration (regexp-fetch compiled-regexp rest-string)))
          (cdr the-output)))))
