;;; Definition of functions that will be used to get MIME types.

;; Fetch the funnies.
(load "file-al.scm")
(use-modules (ice-9 regex))

;; Gotta store them somewhere!, arbitrary number btw.
(define *mime-types* (make-hash-table 1000))

;; Regex we shall use.
(define name-regexp (make-regexp "[A-Za-z/\\.]+"))
(define white-regexp (make-regexp "^[[:blank:]]*$"))

;; Utility we shall use, looping through list on terms of loop is too repetitive, going
;; against my doctrine, some other functions are bound to be rewritten on terms of macros &
;; abstractions, this one is inspired by the common lisp implementation of dolist.
(define-syntax dolist
  (λ (x)
    (syntax-case x ()
      ((_ (var the-list) expr expr* ...) ; First syntax form.  
       (with-syntax ((the-iterator
                      (datum->syntax x 'the-iterator)))
         #'(let the-iterator ((var (car the-list))
                              (the-rest (cdr the-list)))
             expr expr* ...
             (unless (null? the-rest)
               (the-iterator (car the-rest)
                             (cdr the-rest))))))

      ((_ (var the-list ret-var) expr expr* ...) ; Second syntax form.
       (with-syntax ((the-iterator
                      (datum->syntax x 'the-iterator))
                     (the-result
                      (datum->syntax x 'the-result)))
         #'(let ((ret-var #f))
             (let the-iterator ((var (car the-list))
                                (the-rest (cdr the-list)))
               expr expr* ...
               (if (null? the-rest)
                   ret-var
                   (the-iterator (car the-rest)
                                 (cdr the-rest))))))))))

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
                (rest-string (cadr initial-fetch)))
            (list-add! the-output the-substring)
            (iteration (regexp-fetch compiled-regexp rest-string)))
          (cdr the-output)))))
