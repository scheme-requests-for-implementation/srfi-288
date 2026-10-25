; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 288 r7rs)
  (import (srfi 288))
  (export error-object? error-object-message error-object-irritants
          file-error? read-error?))
