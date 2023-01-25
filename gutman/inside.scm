;; Provide only "gutman-edit" so that primitive functions of "gutman"
;; does not pollute the namespace.
(define-module (gutman inside)
  :use-module (gutman core)
  #:re-export (gutman-inside-edit gutman-inside-set))

