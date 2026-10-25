; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 288)
  (import (rename (scheme base)
                  (error-object? r7rs:error-object?)
                  (error-object-message r7rs:error-object-message)
                  (error-object-irritants r7rs:error-object-irritants)
                  (read-error? r7rs:read-error?)
                  (file-error? r7rs:file-error?))
          (scheme case-lambda) (srfi 222))
  (export define-condition-type condition?
          condition
          (rename compound-subobjects simple-conditions)
          error-object? error-object-message error-object-irritants
          file-error? read-error?
          make-warning warning?
          message-condition? make-message-condition condition-message
          make-irritants-condition simple-irritants-condition?
          condition-irritants
          make-serious-condition serious-condition?
          make-external-error external-error?
          make-assertion-violation assertion-violation?)
  (include "288.scm"))
