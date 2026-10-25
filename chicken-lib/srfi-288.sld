(define-library (srfi 288)
  (import (scheme base) (chicken base) (except (chicken condition) condition))
  (export define-condition-type condition?
          condition
          error-object? error-object-message error-object-irritants
          file-error? read-error?
          make-warning warning?
          message-condition? make-message-condition condition-message
          make-irritants-condition simple-irritants-condition?
          condition-irritants
          make-serious-condition serious-condition?
          make-external-error external-error?
          make-assertion-violation assertion-violation?)
  (begin

(define condition make-composite-condition)

(define (make-condition-type name fields)
  (let ((name (string->uninterned-symbol (symbol->string name)))
        (fields (cond
                  ((pair? fields) (list->vector fields))
                  ((vector? fields) (vector-copy fields))
                  (else (error "not a valid fields" fields)))))
    (values name
            (lambda arguments
              (unless (= (length arguments) (vector-length fields))
                (error "invalid number of arguments" arguments))
              (apply make-property-condition
                     name
                     (let loop ((i 0) (arguments arguments))
                       (if (null? arguments)
                           '()
                           (cons (vector-ref fields i)
                                 (cons (car arguments)
                                       (loop (+ i 1) (cdr arguments))))))))
            (condition-predicate name)
            (vector-map (lambda (field)
                          (condition-property-accessor name
                                                       field))
                        fields))))

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

(define (true? obj)
  (not (eq? obj #f)))

(define-condition-type &warning
  (make-warning)
  warning?)

(define-condition-type &simple-message
  (make-message-condition message)
  simple-message-condition?
  (message simple-condition-message))

(define exn? (condition-predicate 'exn))

(define (message-condition? obj)
  (or (simple-message-condition? obj)
      (and (exn? obj)
           (true? ((condition-property-accessor 'exn 'message #f)
                   obj)))))

(define condition-message
  (let ((exn-message (condition-property-accessor 'exn 'message)))
    (lambda (obj)
      (cond
        ((simple-message-condition? obj)
         (simple-condition-message obj))
        (else (exn-message obj))))))

(define-condition-type &simple-irritants
  (make-irritants-condition irritants)
  simple-irritants-condition?
  (irritants simple-condition-irritants))

(define exn? (condition-predicate 'exn))

(define (irritants-condition? obj)
  (or (simple-irritants-condition? obj)
      (and (exn? obj)
           (true? ((condition-property-accessor 'exn 'arguments #f)
                   obj)))))

(define condition-irritants
  (let ((exn-irritants (condition-property-accessor 'exn 'arguments)))
    (lambda (obj)
      (cond
        ((simple-irritants-condition? obj)
         (simple-condition-irritants obj))
        (else (exn-irritants obj))))))

(define-condition-type &serious
  (make-serious-condition)
  simple-serious-condition?)

(define (serious-condition? obj)
  (or (simple-serious-condition? obj)
      (exn? obj)))

(define-condition-type &external-error
  (make-external-error)
  simple-external-error?)

(define external-error?
  (let ((i/o? (condition-predicate 'i/o)))
    (lambda (obj)
      (or (simple-external-error? obj)
          (i/o? obj)))))

(define-condition-type &assertion
  (make-assertion-violation)
  simple-assertion-violation?)

(define assertion-violation?
  (let ((arity? (condition-predicate 'arity))
        (type? (condition-predicate 'type))
        (arithmetic? (condition-predicate 'arithmetic))
        (bounds? (condition-predicate 'bounds))
        (runtime? (condition-predicate 'runtime))
        (match? (condition-predicate 'match))
        (syntax? (condition-predicate 'syntax)))
    (lambda (obj)
      (or (simple-assertion-violation? obj)
          (arity? obj)
          (type? obj)
          (arithmetic? obj)
          (bounds? obj)
          (match? obj)
          (syntax? obj)))))
))
