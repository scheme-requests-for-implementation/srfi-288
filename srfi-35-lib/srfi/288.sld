(define-library (srfi 288)
  (import (scheme base)
          (rename (srfi 35)
                  (make-condition-type srfi-35:make-condition-type)
                  (define-condition-type srfi-35:define-condition-type)))
  (export make-condition-type
          define-condition-type
          condition?
          (rename make-compound-condition condition)
          make-warning warning?
          make-serious-condition serious-condition?
          make-assertion-violation assertion-violation?
          make-external-error (rename error? external-error?)
          make-message-condition message-condition? condition-message
          make-irritants-condition condition-irritants irritants-condition?
          error-object? error-object-message error-object-irritants
          read-error? file-error?)
  (include "288.scm"))
