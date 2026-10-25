(define (make-condition-type name fields)
  (srfi-35:make-condition-type name
                               &condition
                               (vector->list fields)))

(define-syntax define-condition-type
  (syntax-rules ()
    ((_ name supertype
        (cstr field-name ...)
        predicate?
        (field-name2 accessor) ...)
     (begin
       (srfi-35:define-condition-type name supertype
         predicate?
         (field-name2 accessor) ...)
       (define (cstr field-name ...)
         (letrec-syntax ((property-list
                          (syntax-rules ...* ()
                            ((_ acc ()) acc)
                            ((_ (acc ...*)
                                (%field-name rest ...*))
                             (property-list (acc ...*
                                             (quote %field-name)
                                             %field-name)
                                            (rest ...*))))))
           (property-list (make-condition name)
                          (field-name ...))))))
    ((_ name (cstr field-name ...)
        predicate?
        (field-name2 accessor) ...)
     (define-condition-type name &condition
       (cstr field-name ...)
       predicate?
       (field-name2 accessor) ...))))

(define-condition-type &warning
  (make-warning)
  warning?)

(define (make-external-error)
  (make-condition &error))

(define-condition-type &irritants
  (make-irritants-condition irritants)
  irritants-condition?
  (irritants condition-irritants))

(define (make-serious-condition)
  (make-condition &serious))

(define-condition-type &assertion &serious
  (make-assertion-violation)
  assertion-violation?)
