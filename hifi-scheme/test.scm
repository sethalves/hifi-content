(declare (standard-bindings)
         (extended-bindings))

(define (print x)
  (##inline-host-statement "console.log(@1@);" x))

(print "HERE\n")
