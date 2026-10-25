; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: GPL-2.0-or-later

(define-library (srfi 288)
  (import (rename (only (mit legacy runtime)
                        condition condition? make-condition-type
                        simple-conditions
                        condition-type:warning
                        condition-type:error
                        condition-type:file-error
                        condition-type:port-error
                        condition-type:not-loading
                        condition-type:illegal-datum
                        condition-type:variable-error
                        condition-type:arithmetic-error
                        condition-type:control-error
                        condition-type:primitive-procedure-error
                        condition-type:simple-error
                        condition-type:simple-warning
                        condition-type:simple-condition
                        condition-type:serious-condition
                        condition-accessor
                        condition-predicate
                        condition-constructor)
                  (make-condition-type mit:make-condition-type))
          (scheme base))
  (export make-condition-type define-condition-type
          simple-conditions condition? condition
          make-warning warning?
          make-message-condition condition-message message-condition?
          make-irritants-condition condition-irritants irritants-condition?
          make-serious-condition serious-condition?
          make-external-error external-error?
          make-assertion-violation assertion-violation?
          error-object? error-object-message error-object-irritants
          file-error? read-error?)
  (begin
    (define (simple-condition-constructor type)
      (let ((constructor (condition-constructor type)))
        (lambda args
          (call-with-current-continuation
            (lambda (k)
              (apply constructor k 'bound-restarts args))))))
    (define (make-condition-type name fields)
      (define type
        (mit:make-condition-type name
                                 #f
                                 (vector->list fields)
                                 #f))
      (values type
              (simple-condition-constructor type)
              (condition-predicate type)
              (vector-map
               (lambda (field-name)
                 (condition-accessor type field-name))
               fields)))
    (define-syntax define-condition-type
      (er-macro-transformer
       (lambda (exp rename compare)
         (let* ((type-name (list-ref exp 1))
                (cstr-expr (list-ref exp 2))
                (cstr-name (car cstr-expr))
                (field-names (cdr cstr-expr))
                (predicate (list-ref exp 3))
                (field-info (list-tail exp 4))
                (field-accessors (map cadr field-info))
                (accessors (rename 'accessors)))
           (define (index-of-field field-info)
             (let ((field-name (symbol->string (car field-info))))
               (let loop ((i 0) (field-names field-names))
                 (cond
                   ((null? field-names)
                    (error "field name not known" field-name))
                   ;; Comparison by name
                   ((string=? (symbol->string (car field-names)) field-name)
                    i)
                   (else (loop (+ i 1) (cdr field-names)))))))
           (define expr
             `(,(rename 'define-values) (,type-name
                                         ,cstr-name
                                         ,predicate
                                         .
                                         ,field-accessors)
                   (,(rename 'let-values)
                      (((,type-name ,cstr-name ,predicate ,accessors)
                        (,(rename 'make-condition-type)
                         (,(rename 'quote) ,type-name)
                         #(,@(cdr cstr-expr)))))
                      (,(rename 'values)
                       ,type-name
                       ,cstr-name
                       ,predicate
                       .
                       ,(map (lambda (single-field-info)
                               `(,(rename 'vector-ref)
                                 ,accessors
                                 ,(index-of-field single-field-info)))
                          field-info)))))
           expr))))

    (define make-warning
      (simple-condition-constructor condition-type:warning))
    (define warning?
      (condition-predicate condition-type:warning))

    (define &simple-assertion-violation
      (mit:make-condition-type 'assertion-violation
                               condition-type:error
                               '()
                               #f))
    (define make-assertion-violation
      (simple-condition-constructor &simple-assertion-violation))

    (define assertion-violation?
      (let ((simple-assertion-violation?
             (condition-predicate &simple-assertion-violation))
            (illegal-datum?
             (condition-predicate condition-type:illegal-datum))
            (variable-error?
             (condition-predicate condition-type:variable-error))
            (arithmetic-error?
             (condition-predicate condition-type:arithmetic-error))
            (control-error?
             (condition-predicate condition-type:control-error))
            (primitive-procedure-error?
             (condition-predicate condition-type:primitive-procedure-error)))
        (lambda (obj)
          (or (simple-assertion-violation? obj)
              (illegal-datum? obj)
              (variable-error? obj)
              (arithmetic-error? obj)
              (control-error? obj)
              (primitive-procedure-error? obj)))))

    (define simple-error?
      (condition-predicate condition-type:simple-error))
    (define simple-warning?
       (condition-predicate condition-type:simple-warning))
    (define simple-condition?
       (condition-predicate condition-type:simple-condition))

    (define-condition-type &message
      (make-message-condition message)
      %message-condition?
      (message %condition-message))

    (define (message-condition? obj)
      (or (%message-condition? obj)
          (simple-error? obj)
          (simple-warning? obj)
          (simple-condition? obj)))

    (define condition-message
      (let ((simple-error-message (condition-accessor
                                   condition-type:simple-error
                                   'message))
            (simple-warning-message (condition-accessor
                                     condition-type:simple-warning
                                     'message))
            (simple-condition-message (condition-accessor
                                       condition-type:simple-condition
                                       'message)))
        (lambda (obj)
          (cond
            ((message-condition? obj)
             (%condition-message obj))
            ((simple-error? obj)
             (simple-error-message obj))
            ((simple-warning? obj)
             (simple-warning-message obj))
            (else (error "cannot access message" obj))))))

    (define-condition-type &irritants
      (make-irritants-condition irritants)
      %irritants-condition?
      (irritants %condition-irritants))

    (define (irritants-condition? obj)
      (or (%irritants-condition? obj)
          (simple-error? obj)
          (simple-warning? obj)
          (simple-condition? obj)))

    (define condition-irritants
      (let ((simple-error-irritants (condition-accessor
                                     condition-type:simple-error
                                     'irritants))
            (simple-warning-irritants (condition-accessor
                                       condition-type:simple-warning
                                       'irritants))
            (simple-condition-irritants (condition-accessor
                                         condition-type:simple-condition
                                         'irritants)))
        (lambda (obj)
          (cond
            ((irritants-condition? obj)
             (%condition-irritants obj))
            ((simple-error? obj)
             (simple-error-irritants obj))
            ((simple-warning? obj)
             (simple-warning-irritants obj))
            (else (error "cannot access irritants" obj))))))

    (define make-serious-condition
      (simple-condition-constructor condition-type:serious-condition))
    (define serious-condition?
      (condition-predicate condition-type:serious-condition))

    (define &external-error
      (mit:make-condition-type 'external-error
                               condition-type:error
                               '()
                               #f))
    (define make-external-error
      (simple-condition-constructor &external-error))
    (define external-error?
      (let ((external-error?
             (condition-predicate &external-error))
            (file-error?
             (condition-predicate condition-type:file-error))
            (port-error?
             (condition-predicate condition-type:port-error))
            (not-loading?
             (condition-predicate condition-type:not-loading)))
        (lambda (obj)
          (or (external-error? obj)
              (file-error? obj)
              (port-error? obj)
              (not-loading? obj)))))
  )
)

(define-library (srfi 288 base)
  (import (srfi 288))
  (export condition condition? make-condition-type
          make-warning warning?
          make-message-condition condition-message message-condition?
          make-irritants-condition condition-irritants irritants-condition?
          make-serious-condition serious-condition?
          make-external-error external-error?
          make-assertion-violation assertion-violation?))

(define-library (srfi 288 r7rs)
  (import (srfi 288))
  (export error-object? error-object-message error-object-irritants
          file-error? read-error?))

(define-library (srfi 288 simple-conditions)
  (import (srfi 288))
  (export simple-conditions))

(define-library (srfi 288 make-condition-type)
  (import (srfi 288))
  (export make-condition-type))
