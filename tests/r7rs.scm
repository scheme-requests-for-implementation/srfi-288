#!no-fold-case ; For MIT-Scheme, which starts out in case folding mode
(test-begin "SRFI NNN")

(define (any f lst)
  (if (null? lst)
      #f
      (or (f (car lst))
          (any f (cdr lst)))))

(define-condition-type &foo
  (make-foo bar baz)
  foo?
  (bar foo-bar)
  (baz foo-baz))

(test-group "user simple conditions"
  (test-assert "define-condition-type constructor makes conditions"
               (condition? (make-foo 1 2)))
  (test-assert "define-condition-type predicate"
               (foo? (make-foo 1 2)))
  
  (let ((foo (make-foo 1 2)))
    (test-equal "accessor on simple condition, 1"
                1
                (foo-bar (make-foo 1 2)))
    (test-equal "accessor on simple condition, 2"
                2
                (foo-baz (make-foo 1 2)))))

(test-group "Compound conditions"
  (let ((comp (condition (make-external-error) (make-foo 10 20))))
    (test-assert "compound condition type with conditions"
                 (condition? comp))
    (test-assert "predicate on compound condition"
                 (external-error? comp))
    (test-assert "user predicate on compound condition"
                 (foo? comp))
    (test-equal "accessor on compound condition, 1"
                10
                (foo-bar comp))
    (test-equal "accessor on compound condition, 2"
                20
                (foo-baz comp))
    (test-group "Simple conditions"
      (cond-expand
        ((library (srfi NNN simple-conditions))
         (let ((lst (simple-conditions comp)))
           (test-assert "simple-conditions is list" (list? lst))
           (test-assert "A condition that is foo? exists somewhere"
                        (any foo? lst))))
        (else)))
    (test-group "Multiple conditions in compound condition"
      (let ((comp (condition (make-foo 10 20) (make-foo 100 200))))
        (test-assert "Predicate"
                     (foo? comp))
        (test-equal "Accessor 1 accesses first instance"
                    10
                    (foo-bar comp))
        (test-equal "Accessor 2 accesses first instance"
                    20
                    (foo-baz comp))))))

(cond-expand
  ((library (srfi NNN make-condition-type))
   (test-group "make-condition-type"
     (let-values (((type cstr is-c? accessors)
                   (make-condition-type 'c '#(a b))))
       (let ((get-a (vector-ref accessors 0))
             (get-b (vector-ref accessors 1)))
         (test-assert "constructor makes conditions"
                      (condition? (cstr 'a 'b)))
         (test-assert "predicate returns true on simple condition"
                      (is-c? (cstr 'a 'b)))
         (test-equal "accessor 1"
                     'a
                     (get-a (cstr 'a 'b)))
         (test-equal "accessor 2"
                     'b
                     (get-b (cstr 'a 'b)))
         (let ((comp (condition (make-foo 1 2)
                                (cstr 'a 'b))))
           (test-assert "compound condition returns true for predicate"
                        (is-c? comp))
           (test-equal "accessor 1 on compound condition"
                       'a
                       (get-a comp))
           (test-equal "accessor 2 on compound condition"
                       'b
                       (get-b comp)))
         (let-values (((type2 cstr2 is-c2? accessors2)
                       (make-condition-type 'c '#(a b))))
           (test-assert "make-condition-type is generative"
                        (not (is-c? (cstr2 'a 'b)))))))))
  (else))

(test-end "SRFI NNN")
