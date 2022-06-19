;;; Define useful SXML & (HT)XML functions.

;; Use more goated modules.
(use-modules (sxml simple))

;; Main template.
(define (templarize-sxml title body . extra-head)
  `(html (head (title ,title) ,@extra-head)
         (body ,@body)))

;; Helper function we will use on another fun thing.
(define (interleave! l1 l2)
  (define (interleave-iterator! x y a) ;; Helper for the helper.
    (cond
     ((and (null? x) y)
      (append! a y))
     ((and (null? y) x)
      (append! a x))
     ((and (null? x) (null? y))
      a)
     (else
      (interleave-iterator! (cdr x)
                            (cdr y)
                            (append! a
                                     (list (car x) (car y)))))))
  (interleave-iterator! l1 l2 '()))

;; Same as above but with fresh lists.
(define (interleave l1 l2)
  (interleave! (list-copy l1) (list-copy l2)))

;; Create SXML of the list, separated by HTML newlines.
(define (linebreak-sxml input-list)
  (let ((html-breaks (make-list (length input-list) '(br))))
    (interleave input-list html-breaks)))

;; Create href's with a name & a link.
(define (href-sxml name link)
  `(a (@ (href ,link)) ,name))
