; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: GPL-2.0-or-later

(load "../mit-scheme-lib/srfi-288.sld")
(load "tests.mit-scheme.scm")
,(import (srfi |288|))
(load "r7rs.scm")

,(import (scheme process-context))
(display "Tests failed: ")
(display tests-failed)
(newline)
(exit (if (zero? tests-failed)
          0
          1))
