;;; Define the silly file access control functions we will use later.

;; Use goated module.
(use-modules (ice-9 rdelim))

;; Read file into a string, then return it, returns #f if EOF.
(define (read-file port)
  (if (eof-object? (peek-char port))
      #f
      (do ((output (open-output-string))
	   (current-line (read-line port 'concat) (read-line port 'concat)))
	  ((eof-object? current-line) (get-output-string output))
	(display current-line output))))

;; Add element to the end of list, abstraction intended to be used in read-directory.
(define (list-add! l1 element)
  (append! l1 (list element)))

;; Read directory into a list, then return it, returns #f if EOF.
(define (read-directory dir-stream)
  (let ((output-list (cons '() '())) ; A little hack, gonna need to find a way to make it work
                                     ; without it shitting itself, maybe an if form for list-add!.
	(check-eof (readdir dir-stream)))
    (if (eof-object? check-eof) ; So i can check for EOF & return #f if it's true.
	#f
	(begin
          (list-add! output-list check-eof)
	  (do ((current-element (readdir dir-stream)
				(readdir dir-stream)))
	      ((eof-object? current-element) (cdr output-list))
            (list-add! output-list current-element))))))

(define (read-file:list port)
  (let ((output-list (cons '() '())) ; Same thing as the above one, repeat code is no good though.
        (check-eof (peek-char port)))
    (if (eof-object? check-eof)
        #f
        (do ((current-element (read-line port 'concat) ; Not sure if i should use vectors instead,
                                        ; vectors would certainly be better.
                              (read-line port 'concat)))
            ((eof-object? current-element) (cdr output-list))
          (list-add! output-list current-element)))))

;; Open & read file, then close it.
(define (get-file name . list-toggle)
  (let ((the-port (open-file name "r")))
    (call-with-values
        (λ ()
          (if (null? list-toggle)
              (read-file the-port)
              (read-file:list the-port)))
      (λ vals
        (close-port the-port)
        (apply values vals)))))

;; Open & read directory, then close it.
(define (get-directory name)
  (let ((the-directory (opendir name)))
    (call-with-values
        (λ ()
          (read-directory the-directory))
      (λ vals
        (closedir the-directory)
        (apply values vals)))))

;; Open path & return 2 values, one being the data itself & the second being the file type symbol.
(define (get-path-file path)
  (let ((file-type (stat:type (stat path))))
    (values
     (case file-type
       ((regular) (get-file path))
       ((directory) (get-directory path))
       (else #f))
     file-type)))
