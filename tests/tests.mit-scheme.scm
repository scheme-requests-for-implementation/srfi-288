;;;; This is a SRFI 64 shim for MIT-Scheme.

(define tests-passed 0)
(define tests-run 0)
(define tests-failed 0)
(define tests-passed-in-group (make-settable-parameter 0))
(define tests-run-in-group (make-settable-parameter 0))
(define tests-failed-in-group (make-settable-parameter 0))
(define test-group-stack (make-settable-parameter '()))
(define test-verbose? (make-settable-parameter #f))

(define (test-report!)
  (display "tests run: ")
  (write tests-run)
  (newline)
  (display "tests passed: ")
  (write tests-passed)
  (newline)
  (display "tests failed: ")
  (write tests-failed)
  (newline))

(define (reset-all-tests!)
  (set! tests-passed 0)
  (set! tests-run 0)
  (set! tests-failed 0))

(define (test-failed!)
  (set! tests-run (+ tests-run 1))
  (tests-run-in-group (+ (tests-run-in-group) 1))
  (set! tests-failed (+ tests-failed 1))
  (tests-failed-in-group (+ (tests-failed-in-group) 1)))

(define (test-passed!)
  (set! tests-run (+ tests-run 1))
  (tests-run-in-group (+ (tests-run-in-group) 1))
  (set! tests-passed (+ tests-passed 1))
  (tests-passed-in-group (+ (tests-passed-in-group) 1)))

(define (in-test-group!)
  (when (pair? (test-group-stack))
    (display "in group ")
    (display (car (test-group-stack)))
    (display ": ")))

(define (display-name! name)
  (if name
      (begin
        (display name)
        (display ": "))))

(define (condition-handler name expr)
  (lambda (condition)
            (test-failed!)
            (in-test-group!)
            (write expr)
            (display " failed due to exception: ")
            (display condition)
            (newline)))

(define-syntax test-assert
  (syntax-rules ()
    ((_ expression)
     (test-assert #f expression))
    ((_ %name expression)
     (let ((name %name))
       (bind-condition-handler
        '()
        (condition-handler name (quote expression))
        (lambda ()
          (if expression
	      (begin
		(test-passed!)
		(when (test-verbose?)
		  (in-test-group!)
		  (display-name! name)
		  (write (quote expression))
		  (display " returned non-false")
		  (newline)))
              (begin
                (test-failed!)
                (display "test failed\n")
                (in-test-group!)
                (display-name! name)
                (write (quote expression))
                (display " returned false")
                (newline)))))))))

(define-syntax test-values
  ;; (test-predicate [name] (procedure args ...))
  (syntax-rules ()
    ((_ (procedure args ...) values)
     (test-values #f (procedure args ...) values))
    ((_ name (procedure args ...) values)
     (test-values name () (procedure args ...) values))
    ((_ name (tmp ...) (itr1 itr2 ...) values)
     (test-values name (tmp ... (tmpnew itr1)) (itr2 ...) values))
    ((_ %name ((tmp1 itr1) (tmp2 itr2) ...) () values)
     (let ((name %name))
       (bind-condition-handler
        '()
        (condition-handler name (quote (predicate args ...)))
        (lambda ()
          (let ((%values values) (tmp1 itr1) (tmp2 itr2) ...)
            (let-values ((returned (tmp1 tmp2 ...)))
            (if (equal? returned %values)
		(begin
		  (test-passed!)
		  (when (test-verbose?)
		    (in-test-group!)
		    (display-name! name)
		    (write (quote (itr1 itr2 ...)))
		    (display " evaluated with arguments ")
		    (write (list itr2 ...))
		    (display " returned something equal? to ")
		    (write returned)
		    (newline)))
                (begin
                  (test-failed!)
                  (in-test-group!)
                  (display-name! name)
		  (display "test failed\n")
                  (write (quote (itr1 itr2 ...)))
                  (display " evaluated with arguments ")
                  (write (list tmp2 ...))
                  (display ", expected ")
                  (write %values)
                  (display ", got ")
                  (write returned)
                  (newline)))))))))))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ (predicate args ...))
     (test-values (predicate args ...) '(#t)))
    ((_ name (predicate args ...))
     (test-values name (predicate args ...) '(#t)))))

(define-syntax test-equal
  (syntax-rules ()
    ((_ name expected got)
     (test-values name (equal? expected got) '(#t)))
    ((_ expected got)
     (test-values (equal? expected got) '(#t)))))

(define-syntax test-not-predicate
  (syntax-rules ()
    ((_ (predicate args ...))
     (test-values (predicate args ...) '(#f)))
    ((_ name (predicate args ...))
     (test-values name (predicate args ...) '(#f)))))

(define (test-begin str)
  (test-group-stack (cons str (test-group-stack))))

(define (test-end str)
  (let ((name (car (test-group-stack))))
    (unless (equal? name str)
      (error "Unequal test-begin and test-end" str name))
    (test-group-stack (cdr (test-group-stack)))))

(define-syntax test-group
  (syntax-rules ()
    ((_ %name body ...)
     (let ((name %name))
       (parameterize ((test-group-stack (cons name (test-group-stack)))
                      (tests-passed-in-group 0)
                      (tests-failed-in-group 0)
                      (tests-run-in-group 0))
         body ...
         (display "summary for ")
         (display name)
         (newline)
         (display "tests passed: ")
         (display (tests-passed-in-group))
         (newline)
         (display "tests failed: ")
         (display (tests-failed-in-group))
         (newline)
         (display "tests total: ")
         (display (tests-run-in-group))
         (newline))))))

(define-syntax test-approximate
  (syntax-rules ()
    ((_ expected expr error)
     (test-approximate #f expected expr error))
    ((_ name expected expr error)
     (test-predicate name
		     (<= (- expected (abs error))
			 expr
			 (+ expected (abs error)))))))

(define-syntax test-many-approximate
  (syntax-rules ()
    ((_ function error (expect input) ...)
     (begin
       (test-approximate expect (function input) error)
       ...))))

(define default-property-tests (make-parameter 30))

(define (test-property tester generators)
  (let ((call (lambda (x) (x))))
    (do ((values (map call generators)
		 (map call generators))
	 (i 0 (+ i 1)))
	((= i (default-property-tests)))
      (display "test #")
      (display i)
      (display " ")
      (write values)
      (newline)
      (test-assert (apply tester values)))))

(define (list-generator-of subgenerator)
  (lambda ()
    (map (lambda (ignored) (subgenerator))
	 (iota (random-integer 30)))))