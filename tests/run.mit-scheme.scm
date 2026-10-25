(load "../mit-scheme-lib/srfi-NNN.sld")
(load "tests.mit-scheme.scm")
,(import (srfi |NNN|))
(load "r7rs.scm")

,(import (scheme process-context))
(display "Tests failed: ")
(display tests-failed)
(newline)
(exit (if (zero? tests-failed)
          0
          1))
