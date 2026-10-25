; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

#!r6rs
(import (except (rnrs) define-condition-type) (srfi :288) (srfi :64))

(test-begin "SRFI 288")

(test-begin "base")
(let-values (((type cstr predicate accessors)
              (make-condition-type 'my-condition '#(a b c d))))
  (test-assert "type is rtd" (record-type-descriptor? type))
  (test-eq "record name" 'my-condition (record-type-name type))
  (test-assert "record is generative" (record-type-generative? type))
  (let ((value (cstr 0 1 2 3)))
    (test-assert "value satisfies predicate" (predicate value))
    (test-assert "value is condition" (condition? value))
    (test-assert "value satisfies predicate from rtd"
                 ((record-predicate type) value))
    (test-assert "value is record" (record? value))
    (test-eqv "rtd is type" type (record-rtd value))
    (test-eqv "slot 0" 0 ((vector-ref accessors 0) value))
    (test-eqv "slot 1" 1 ((vector-ref accessors 1) value))
    (test-eqv "slot 2" 2 ((vector-ref accessors 2) value))
    (test-eqv "slot 3" 3 ((vector-ref accessors 3) value))
    (let ((compound (condition value (make-external-error))))
      (test-assert "compound condition satisfies predicate"
                   (predicate compound))
      (test-assert "compound condition is other condition"
                   (external-error? compound))
      (test-eqv "slot 0, compound" 0 ((vector-ref accessors 0) compound))
      (test-eqv "slot 1, compound" 1 ((vector-ref accessors 1) compound))
      (test-eqv "slot 2, compound" 2 ((vector-ref accessors 2) compound))
      (test-eqv "slot 3, compound" 3 ((vector-ref accessors 3) compound))
      (let ((simple (simple-conditions compound)))
        (test-assert "has simple condition"
                     (predicate (list-ref simple 0)))))))

(define-condition-type &foo
  (foo bar baz)
  foo?
  (bar foo-bar)
  (baz foo-baz))

(test-assert "syntactically defined condition satisfies predicate"
             (foo? (foo 1 2)))
(test-equal "accessor 1"
            1
            (foo-bar (foo 1 2)))
(test-equal "accessor 2"
            2
            (foo-baz (foo 1 2)))
(let ((comp (condition (make-warning) (foo 1 2))))
  (test-assert "compound condition is condition"
               (condition? comp))
  (test-assert "compound condition is foo?"
               (foo? comp))
  (test-equal "accessor 1"
              1
              (foo-bar comp))
  (test-equal "accessor 2"
              2
              (foo-baz comp)))

(test-end "base")

(test-end "SRFI 288")
