
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

;; Read directory into a list, then return it, returns #f if EOF.
(define (read-directory dir-stream)
  (let ((output-list '())
	(check-eof (readdir dir-stream)))
    (if (eof-object? check-eof) ; So i can check for EOF & return #f if it's true.
	#f
	(begin
	  (set! output-list
		(append! output-list (list check-eof)))
	  (do ((current-element (readdir dir-stream)
				(readdir dir-stream)))
	      ((eof-object? current-element) output-list)
	    (set! output-list
		  (append! output-list (list current-element))))))))
