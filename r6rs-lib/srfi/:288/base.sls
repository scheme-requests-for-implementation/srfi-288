(library (srfi :NNN base)
  (export make-condition-type define-condition-type
          condition condition?
          make-warning warning?
          make-message-condition condition-message message-condition?
          make-irritants-condition condition-irritants irritants-condition?
          make-error error?
          make-assertion-violation assertion-violation?)
  (import (srfi :NNN)))
