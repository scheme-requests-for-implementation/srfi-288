(library (srfi :NNN)
  (export make-condition-type
          (rename (define-condition-type* define-condition-type))
          condition
          simple-conditions
          condition?
          make-warning warning?
          make-message-condition condition-message message-condition?
          make-irritants-condition condition-irritants irritants-condition?
          (rename (make-error make-external-error))
          (rename (error? external-error?))
          serious-condition? make-serious-condition
          make-assertion-violation assertion-violation?)
  (import (rnrs))
  (define (make-condition-type name fields)
     (let ((fields
            (if (pair? fields)
                (do ((v (make-vector (length fields)))
                     (i 0 (+ i 1))
                     (fields fields (cdr fields)))
                    ((= i (vector-length v)) v)
                  (vector-set! v
                               i
                               `(immutable ,(car fields))))
                (vector-map (lambda (field)
                              `(immutable ,field))
                            fields))))
       (define rtd
         (make-record-type-descriptor
          name
          (record-type-descriptor &condition)
          #f #f #f fields))
       (define rcd
         (make-record-constructor-descriptor
          rtd
          (record-constructor-descriptor &condition)
          #f))
       (define accessors
         (do ((i 0 (+ i 1))
              (accessors (make-vector (vector-length fields))))
             ((= i (vector-length fields)) accessors)
           (vector-set! accessors i
                        (condition-accessor rtd
                                            (record-accessor rtd i)))))
       (values rtd
               (record-constructor rcd)
               (condition-predicate rtd)
               accessors)))
  (define-syntax define-condition-type*
    (lambda (x)
      (define (accessor-list->alist accessors)
        (map (lambda (x)
               (cons (syntax->datum (car x))
                     (cadr x)))
             accessors))
      (define (rearrange-accessors fields accessors)
        (syntax-case fields ()
          (() '())
          ((name rest ...)
           (cond
             ((assq (syntax->datum #'name) accessors)
              =>
              (lambda (pair)
                (cons (list #'name (cdr pair))
                      (rearrange-accessors #'(rest ...) accessors))))
             (else
              (syntax-violation 'define-condition-type
                                "unknown field"
                                x
                                #'name))))))
      (syntax-case x ()
        ((_ name (cstr field ...) predicate? (name2 accessor) ...)
         (with-syntax ((((name2 accessor) ...)
                        (rearrange-accessors #'(field ...)
                                             (accessor-list->alist
                                              #'((name2 accessor) ...)))))
           #'(define-condition-type name &condition
               cstr predicate?
               (name2 accessor) ...)))
        ((_ ctype stype cstr pred (field accessor) ...)
         #'(define-condition-type ctype stype cstr pred
             (field accessor) ...))))))
