;; Provide only "gutman-edit" so that primitive functions of "gutman"
;; does not pollute the namespace.
(define-module (gutsys edit)
  :use-module (gutman)
  #:re-export (gutman-edit))

