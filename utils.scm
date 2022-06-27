;;; Utilities that couldn't fit on any category or shouldn't be in some other
;;; places.

;; Add element to the end of list, abstraction intended to be used in read-directory.
(define (list-add! l1 element)
  (append! l1 (list element)))

;; Gives some more leverage on filter.
(define-syntax filter-if
  (λ (x)
    (syntax-case x ()
      ((_ f l1)
       #'(filter (complement f) l1)))))

;; Complement of a function.
(define-syntax complement
  (λ (x)
    (syntax-case x ()
      ((_ f)
       (with-syntax ((args (datum->syntax x 'in)))
         #'(λ (. in)
             (not (apply f in))))))))

;; Pipe but reversed, simpler also.
(define-syntax compose
  (λ (x)
    (syntax-case x ()
      ((_ input f)
       #'(f input))
      ((_ input f g)
       #'(f (g input)))
      ((_ input f g h ...)
       #'(f (pipe input g h ...))))))

;; Same as above but returns a function, opposed of executing them.
(define-syntax compose-lambda
  (λ (x)
    (syntax-case x ()
      ((_ f)
       (with-syntax ((in (datum->syntax x 'in)))
         #'(lambda (in)
             (f in))))
      ((_ f g)
       (with-syntax ((in (datum->syntax x 'in)))
         #'(lambda (in)
             (compose in f g))))
      ((_ f g h ...)
       (with-syntax ((in (datum->syntax x 'in)))
         #'(lambda (in)
             (compose in f g h ...)))))))
