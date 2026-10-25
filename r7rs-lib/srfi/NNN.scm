(define (compound-accessor* predicate accessor obj)
  (let* ((dummy (cons #f #f))
         (r (compound-access predicate accessor dummy obj)))
    (if (eq? dummy r)
        (error "Not a simple or compound condition"
               obj)
        r)))

(define-syntax define-condition-type
  (syntax-rules ()
    ((_ name (cstr field ...) predicate? (field-name accessor-name) ...)
     (define-condition-type "generate-temporaries" #t
       (name (cstr field ...) predicate?)
       ()
       ((field-name accessor-name) ...)))
    ((_ "generate-temporaries" #t data (tmp ...) ((field-name accessor-name) rest ...))
     (define-condition-type "generate-temporaries" #t
       data
       ((tmp-name field-name accessor-name) tmp ...)
       (rest ...)))
    ((_ "generate-temporaries" #t
        (name (cstr field ...) predicate?)
        ((tmp-name field-name accessor-name) ...)
        ())
     ;; Avoids an issue with creating temporaries in toplevel contexts.
     ;; Unfortunately, does not bind `name` to what the implementation
     ;; binds it to.
     (define-values (name cstr predicate? accessor-name ...)
       (let ()
         (define-record-type dummy-name
           (cstr field ...)
           tmp-predicate?
           (field-name tmp-name) ...)
         (define (predicate? obj)
           (compound-predicate tmp-predicate? obj))
         (define (accessor-name obj)
           (compound-accessor* tmp-predicate?
                               tmp-name
                               obj)) ...
         (values #(name field ...)
                 cstr
                 predicate?
                 accessor-name ...))))))

(define condition? (lambda (x) #t))
(define condition
  (case-lambda
    ((x) x)
    (x (apply make-compound x))))

(define (error-object? obj)
  (compound-predicate r7rs:error-object? obj))
(define (error-object-message obj)
  (compound-predicate r7rs:error-object-message obj))
(define (error-object-irritants obj)
  (compound-predicate r7rs:error-object-irritants obj))
(define (file-error? obj)
  (compound-predicate r7rs:file-error? obj))
(define (read-error? obj)
  (compound-predicate r7rs:read-error? obj))

(define-condition-type &warning
  (make-warning)
  warning?)

(define-condition-type &simple-message
  (make-message-condition message)
  simple-message-condition?
  (message %condition-message))

(define (message-condition? obj)
  (compound-predicate (lambda (x)
                        (or (r7rs:error-object? x)
                            (simple-message-condition? x)))
                      obj))

(define (condition-message obj)
  (compound-accessor* (lambda (x)
                        (or (r7rs:error-object? x)
                            (simple-message-condition? x)))
                      (lambda (x)
                        (if (r7rs:error-object? x)
                            (r7rs:error-object-message x)
                            (%condition-message x)))
                      obj))

(define-condition-type &simple-irritants
  (make-irritants-condition irritants)
  simple-irritants-condition?
  (irritants %condition-irritants))

(define (irritants-condition? obj)
  (compound-predicate (lambda (x)
                        (or (r7rs:error-object? x)
                            (simple-irritants-condition? x)))
                      obj))

(define (condition-irritants obj)
  (compound-accessor* (lambda (x)
                        (or (r7rs:error-object? x)
                            (simple-irritants-condition? x)))
                      (lambda (x)
                        (if (r7rs:error-object? x)
                            (r7rs:error-object-irritants x)
                            (%condition-irritants x)))
                      obj))

(define-condition-type &serious
  (make-serious-condition)
  %serious-condition?)

(define (serious-condition? obj)
  (compound-predicate (lambda (x)
                        (or (r7rs:error-object? x)
                            (r7rs:file-error? x)
                            (r7rs:read-error? x)
                            (%serious-condition? x)))
                      obj))

(define-condition-type &error
  (make-simple-error)
  simple-error?)

(define (make-external-error)
  (condition (make-simple-error) (make-serious-condition)))
(define (external-error? obj)
  (or (and (simple-error? obj) (serious-condition? obj))
      (compound-predicate (lambda (x)
                            (or (r7rs:file-error? x)
                                (r7rs:read-error? x)))
                          obj)))

(define-condition-type &assertion
  (make-simple-assertion-violation)
  simple-assertion-violation?)

(define (make-assertion-violation)
  (condition (make-simple-assertion-violation) (make-serious-condition)))
(define (assertion-violation? obj)
  (and (simple-assertion-violation? obj)
       (serious-condition? obj)))
