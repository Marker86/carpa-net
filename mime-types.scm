;;; Definition of functions that will be used to get MIME types.

;; Fetch the funnies.
(load "file-al.scm")
(load "utils.scm")
(use-modules (ice-9 regex))

;; Gotta store them somewhere!, arbitrary number btw.
(define *mime-types* (make-hash-table 1000))

;; Regex we shall use.
(define name-regexp (make-regexp "[A-Za-z/\\.-]+"))
(define white-regexp (make-regexp "^[[:blank:]]*$"))
(define comment-regexp (make-regexp "^#.*"))

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
      (when initial-fetch
          (let ((the-substring (car initial-fetch))
                (rest-string (cadr initial-fetch)))
            (list-add! the-output the-substring)
            (iteration (regexp-fetch compiled-regexp rest-string)))))
    (if (null? (cdr the-output))
        #f
        (cdr the-output))))

;; Filter the list of strings in terms of the regexp object provided.
(define (regexp-filter compiled-regexp string-list)
  (filter (λ (x)
            (if x
                (regexp-exec compiled-regexp x)
                #f))
          string-list))

;; Filter the list of strings by the opposite result of the regexp provided.
(define (regexp-filter-if-not compiled-regexp string-list)
  (filter-if (λ (x)
            (if x
              (regexp-exec compiled-regexp x)
              #f))
          string-list))

;; Parse the lines
(define (parse-lines lines)
  (compose
   lines
   (λ (x)
     (map (λ (y)
            (if y
                (regexp-scan name-regexp y)
                #f))
          x))
   (λ (x)
     (regexp-filter-if-not comment-regexp x))))

;; Shove the matches list in MIME types table following this rule:
;; (type ext1 ext2 ...) -> (hash-set! hash-table ext1 type) ...
(define (shove-in-mime-types the-list)
  (let ((the-extensions (cdr the-list))
        (the-type (car the-list)))
    (unless (null? the-extensions)
      (map (λ (x)
             (hash-set! *mime-types* x the-type))
           the-extensions))))

;; Get/update MIME types database.
(define (update-mime-types path)
  (map shove-in-mime-types
       (filter-if (λ (x)
                    (eq? x #f))
                  (parse-lines (get-file path #:list-toggle #t)))))
