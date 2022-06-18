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

;; Open & read file, then close it.
(define (get-file name)
  (let ((the-port (open-file name "r")))
    (call-with-values
        (λ ()
          (read-file the-port))
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
