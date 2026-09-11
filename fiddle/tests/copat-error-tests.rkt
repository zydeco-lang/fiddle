#lang racket/base

;; Error-path tests for copattern matching, driven from rackunit.
;; The Fiddle side lives in an inner module; a Fiddle thunk is a 1-arg
;; Racket procedure over the stack (a list), so we can call it directly.

(module m fiddle
  (require fiddle/prelude)
  (provide bad zero-clauses)
  (define bad (thunk (copat [((= 1)) (ret 'one)])))
  (define zero-clauses (thunk (copat))))

(require 'm rackunit)

(check-equal? (bad '(1)) 'one)
(check-exn #rx"copattern-match-error" (λ () (bad '(2))))
;; the message must mention the remaining args
(check-exn #rx"\\(2\\)" (λ () (bad '(2))))
(check-exn #rx"copattern-match-error" (λ () (zero-clauses '())))

;; ---------------------------------------------------------------------
;; Expansion-time errors. Expand a whole module in a fresh namespace so
;; a bad pattern can't take this file down with it. Everything an
;; expression mentions is bound, so the only possible failure is the
;; one under test. Note: a fiddle module's top-level expressions are
;; wrapped in (main ...) which demands a computation, so we `define` a
;; thunk instead of writing a bare expression.

(define (expand-fiddle-module body-forms)
  (parameterize ([current-namespace (make-base-namespace)])
    (expand `(module probe fiddle
               (require fiddle/prelude)
               ,@body-forms))))

;; control: a valid module expands
(check-not-exn
 (λ () (expand-fiddle-module
        '((define t (thunk (copat [(x) (ret x)] [((rest r)) (ret r)])))))))

;; (@ e x) was dropped from the pattern language
(check-exn exn:fail:syntax?
 (λ () (expand-fiddle-module
        '((define q 1)
          (define t (thunk (copat [((@ q x)) (ret x)])))))))

;; (#:kw p) was dropped from the pattern language
(check-exn exn:fail:syntax?
 (λ () (expand-fiddle-module
        '((define t (thunk (copat [((#:k x)) (ret x)])))))))

;; nothing may follow (rest xs) — not a pattern, not #:bind
(check-exn #rx"rest xs\\) must be the last"
 (λ () (expand-fiddle-module
        '((define t (thunk (copat [((rest xs) y) (ret y)])))))))
(check-exn #rx"rest xs\\) must be the last"
 (λ () (expand-fiddle-module
        '((define t (thunk (copat [((rest xs) #:bind) (ret xs)])))))))
