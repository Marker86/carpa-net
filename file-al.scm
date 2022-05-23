;;; Define the silly file access control functions we will use later.

;; Use goated module.
(use-modules (ice-9 rdelim)
	     (ice-9 streams))

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

;; A cache.
(define group-cache '())

;; Do what the name of the function says.
(define (update-group-cache)
  (setgr)
  (do ((current-element (getgr) (getgr))
       (output '()))
      ((not current-element) (set! group-cache output))
    (set! output (append! output (list current-element)))))

;; Obtain file permissions from octal integer value.
(define (check:rwx integer read write exec)
  (define (nor x y)
    (not (or x y)))

  (define (the-funny x y) ; Wicked logic needed: #t #t = #t, #f #t = #f, #t #f = #t, #f #f = #t.
    (or (and x y) (nor x y) (and x (not y))))

  (define (rwx integer read write exec)
    (list (the-funny (positive? (bit-extract integer 2 3)) read) ; Read bit.
          (the-funny (positive? (bit-extract integer 1 2)) write) ; Write bit.
          (the-funny (positive? (bit-extract integer 0 1)) exec))) ; Execute bit.
  (rwx integer read write exec))
