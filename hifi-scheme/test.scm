(declare (standard-bindings)
         (extended-bindings))

(define (print x) (##inline-host-statement "console.log(g_scm2host(@1@));" x))

(print "HERE\n")
