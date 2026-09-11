#lang turnstile

;; A CBPV Scheme-like
;; 
;; Fiddle has two kinds of terms: values and computations.

;; - Values are elaborated to pure Racket terms that return a value.

;; - Computations are elaborated to Racket terms that have access to a
;;   variable holding the stack (which is bound as current-stack).

;;   Every stack conceptually ends with a continuation, which is just
;;   implemented as the ambient Racket continuation for the term.

;;   Thunking creates a procedure that explicitly takes the stack as an argument, and forcing passes the current stack to the procedure.

;;   Returning returns a value to the ambient continuation but only if the stack is empty. Bind executes the computation with an empty stack but continues with its result and the old stack. Conceptually this is creating a continuation that captures the current stack.

;;   Application is just pushing a value onto the current stack, and case-λ pattern matches on the stack to check if there are any arguments left.

;; A stack is a list of Methods where each element is one of
;; - a plain value (an argument pushed on)
;; - a `method` struct (a nominal method frame with its args and remaining tail)
;;

;; If we add something like opaque stack types, we would probably need
;; to pass the continuation explicitly as the end of the list, rather
;; than what we do now which is use the ambient Racket continuation. This
;; would make it a very CPS-like implementation. Would that have any
;; performance downside?

(require racket/stxparam
         "initialize.rkt"
         (for-syntax syntax/parse))
(provide (all-defined-out)
         (rename-out (many-app #%app))
         matches-tag?)

(define-base-type value)
(define-base-type computation)

;; The stack is two lexically bound values (see initialize.rkt):
;;   current-vals   — the open segment: values pushed since the last delimiter
;;   current-frames — the delimiters beneath it, each with its own segment
(define-syntax-parameter current-vals
  (λ (stx)
    (raise-syntax-error 'current-vals "used outside with-stack / with-vals / thunk-λ" stx)))
(define-syntax-parameter current-frames
  (λ (stx)
    (raise-syntax-error 'current-frames "used outside with-stack / thunk-λ" stx)))

;; Run body with both halves of the stack rebound. The new-* expressions
;; are evaluated in the OUTER binding (plain let), so they may mention
;; current-vals / current-frames to mean the old values.
(define-syntax-rule (with-stack new-vals new-frames body ...)
  (let- ([vs new-vals] [fs new-frames])
    (syntax-parameterize ([current-vals   (make-rename-transformer #'vs)]
                          [current-frames (make-rename-transformer #'fs)])
      body ...)))

;; Run body with only the open segment rebound (the common case).
(define-syntax-rule (with-vals new-vals body ...)
  (let- ([vs new-vals])
    (syntax-parameterize ([current-vals (make-rename-transformer #'vs)])
      body ...)))

;; The one place a computation becomes a closure: a 2-argument procedure
;; over (vals frames).
(define-syntax-rule (thunk-λ body ...)
  (lambda (vs fs)
    (syntax-parameterize ([current-vals   (make-rename-transformer #'vs)]
                          [current-frames (make-rename-transformer #'fs)])
      body ...)))

(define-syntax (require-wrapped-provide stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (rkt->fiddle x-tmp))
         (define-primop x x-wrapped : value)
         (provide x))]))
(define-syntax (require-fo-wrapped-provide stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (fo-rkt->fiddle x-tmp))
         (define-primop x x-wrapped : value)
         (provide x))]))
(define-syntax (require-fo-kw-wrapped-provide stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (fo-kw-rkt->fiddle x-tmp))
         (define-primop x x-wrapped : value)
         (provide x))]))

(define-syntax (require-wrapped stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (rkt->fiddle x-tmp))
         (define-primop x x-wrapped : value))]))
#;
(define-syntax (from-racket stx)
  (syntax-parse stx
    [(_ lib x)
     ]))

;; `error` is wrapped WITHOUT the delimiter check: it runs on failure
;; paths exactly when a delimiter is on top, and its message must not be
;; masked by the "primitive applied with a delimiter" error.
(begin-
  (require (only-in racket/base [error error-rkt]))
  (define error-wrapped (fo-nocheck-rkt->fiddle error-rkt))
  (define-primop error error-wrapped : value)
  (provide error))
(require-fo-wrapped-provide "initialize.rkt" new-method)
(require-fo-wrapped-provide "initialize.rkt" new-tag)
(require-fo-wrapped-provide "initialize.rkt" Tag)

(require-fo-wrapped-provide racket/base box)
(require-fo-wrapped-provide racket/base unbox)
(require-fo-wrapped-provide racket/base set-box!)
(require-fo-wrapped-provide racket/base +)
(require-fo-wrapped-provide racket/base abs)
(require-fo-wrapped-provide racket/base *)
(require-fo-wrapped-provide racket/base truncate)
(require-fo-wrapped-provide racket/base modulo)
(require-fo-wrapped-provide racket/base quotient)
(require-fo-wrapped-provide racket/base /)
(require-fo-wrapped-provide racket/base gcd)
(require-fo-wrapped-provide racket/base lcm)
(require-fo-wrapped-provide racket zero?)
(require-fo-wrapped-provide racket/base -)
(require-fo-wrapped-provide racket/base <)
(require-fo-wrapped-provide racket/base <=)
(require-fo-wrapped-provide racket/base =)
(require-fo-wrapped-provide racket/base >)
(require-fo-wrapped-provide racket/base >=)
(require-fo-wrapped-provide racket/base not)
(require-fo-wrapped-provide racket number?)
(require-fo-wrapped-provide racket cons?)
(require-wrapped-provide racket null)
(require-fo-wrapped-provide racket null?)
(require-fo-wrapped-provide racket/base car)
(require-fo-wrapped-provide racket/base cdr)
(require-fo-wrapped-provide racket/base equal?)
(require-fo-wrapped-provide racket/base symbol?)

(require-fo-wrapped-provide racket/base string<=?)
(require-fo-wrapped-provide racket/base string-append)
(require-fo-wrapped-provide racket/base string-length)
(require-fo-wrapped-provide racket/base string-ref)
(require-fo-wrapped-provide racket/base substring)

;; IO
(require-fo-wrapped-provide racket with-output-to-string)
(require-fo-wrapped-provide racket system/exit-code)
(require-fo-wrapped-provide racket open-input-file)
(require-fo-wrapped-provide racket close-input-port)
(require-fo-wrapped-provide racket/base read-line)
(require-fo-wrapped-provide racket/base read-char)
(require-fo-wrapped-provide racket/base read)
(require-fo-kw-wrapped-provide racket open-output-file)
(require-fo-wrapped-provide racket close-output-port)
(require-fo-wrapped-provide racket/base displayln)
(require-fo-wrapped-provide racket/base display)
(require-fo-wrapped-provide racket current-command-line-arguments) ;; returns a vector

(require-fo-wrapped-provide racket/base number->string)
(require-fo-wrapped-provide racket/base string->list)
(require-fo-wrapped-provide racket/base list->string)
(require-fo-wrapped-provide racket/base char-upcase)
(require-fo-wrapped-provide racket/base char-downcase)
(require-fo-wrapped-provide racket/base char->integer)
(require-fo-wrapped-provide racket/base integer->char)
(require-fo-wrapped-provide racket/base string?)
(require-fo-wrapped-provide racket/base char?)
(require-fo-wrapped-provide racket/base eof-object?)
(require-fo-wrapped-provide racket/base hash?)
(require-fo-wrapped-provide racket/base hash)
(require-fo-wrapped-provide racket/base hash-set)
(require-fo-wrapped-provide racket/base append)
(require-fo-wrapped-provide racket/base member)
(require-fo-wrapped-provide racket/base reverse)
(require-fo-wrapped-provide racket/base hash-ref)
(require-fo-wrapped-provide racket/base hash-remove)
(require-fo-wrapped-provide racket/base hash-empty?)
(require-fo-wrapped-provide racket/base hash-has-key?)
(require-fo-wrapped-provide racket/base hash-count)
(require-fo-wrapped-provide racket/base hash->list)
(require-fo-wrapped-provide racket list->set)
(require-fo-wrapped-provide racket set->list)
(require-fo-wrapped-provide racket make-polar)
(require-fo-wrapped-provide racket make-rectangular)
(require-fo-wrapped-provide racket real-part)
(require-fo-wrapped-provide racket imag-part)
(require-fo-wrapped-provide racket angle)

;; mutable vector stuff
(require-fo-wrapped-provide racket/base make-vector)
(require-fo-wrapped-provide racket/base vector?)
(require-fo-wrapped-provide racket/base vector-ref)
(require-fo-wrapped-provide racket/base vector-length)
(require-fo-wrapped-provide racket/base vector-set!)
(require-fo-wrapped-provide racket/base vector-fill!)
(require-fo-wrapped-provide racket/base list->vector)
(require-wrapped-provide racket/base sort)
(require-wrapped-provide racket pi)

;; bytestrings

(require-fo-wrapped-provide racket/base bytes?)
(require-fo-wrapped-provide racket/base byte?)
(require-fo-wrapped-provide racket/base bytes->list)
(require-fo-wrapped-provide racket/base list->bytes)
(require-fo-wrapped-provide racket/base bytes-ref)
(require-fo-wrapped-provide racket/base bytes-set!)
(require-fo-wrapped-provide racket/base bytes-length)

;; Values
;;

(define-typed-syntax quoth
  [(_ . e) ≫
   -----------
   (⊢ (quote- . e) ⇒ value)])

(define-typed-syntax #%datum
  [(_ . d) ≫
   -----------------
   (⊢ (#%datum- . d) ⇒ value)])

(define-typed-syntax (if e e1 e2) ≫
  (⊢ e ≫ e- ⇐ value)
  (⊢ e1 ≫ e1- ⇐ computation)
  (⊢ e2 ≫ e2- ⇐ computation)
  ------------------
  (⊢ (if- e- e1- e2-) ⇒ computation))

(define-typed-syntax (ret e) ≫
  (⊢ e ≫ e- ⇐ value)
  ----------------
  (⊢
   (if- (and- (null?- current-vals) (null?- current-frames))
        e-
        (error- (format "expected a return address on the stack but got ~a"
                        (cons- current-vals current-frames))))
   ⇒ computation))

(define-typed-syntax (bind (x:id e) e^) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ e^ ≫ e^- ⇐ computation)
  -----------------
  (⊢ (let- ([x- (with-stack '() '() e-)])  ;; run e against an empty stack
       e^-)                                ;; run e^ against the ambient stack
     ⇒ computation))

(define-typed-syntax (let ([x e] ...) e^) ≫
  (⊢ e ≫ e- ⇐ value) ...
  ((x ≫ x- : value) ... ⊢ e^ ≫ e^- ⇐ computation)
  -----------------
  (⊢ (let- ([x- e-] ...) e^-)
     ⇒ computation))

(define-typed-syntax let*
  [(_ () ebod) ≫
   --------------
   (≻ ebod)]
  [(_ ([x e] rst ...) ebod) ≫
   --------------
   (≻ (let ([x e]) (let* (rst ...) ebod)))])

(define-typed-syntax (cons e es) ≫
  (⊢ e ≫ e- ⇐ value)
  (⊢ es ≫ es- ⇐ value)
  ----------------------
  (⊢ (cons- e- es-) ⇒ value))

(define-typed-syntax (thunk e) ≫
  (⊢ e ≫ e- ⇐ computation)
  ----------------
  ;; The one place a computation becomes a closure.
  (⊢ (thunk-λ e-) ⇒ value))

(define-typed-syntax (basic-! e) ≫
  (⊢ e ≫ e- ⇐ value)
  ----------------
  ;; force = apply the thunk to the current stack
  (⊢ (e- current-vals current-frames) ⇒ computation))

(define-typed-syntax (! e es ...) ≫
  ------------------------
  (≻ (many-app (basic-! e) es ...)))

(define-typed-syntax (copat-arg [(x:id) ex] [() e]) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  ----------------------------------------
  (⊢ (cond- [(pair?- current-vals)
             (let- ([x- (car- current-vals)])
               (with-vals (cdr- current-vals) ex-))]
            [else e-])
     ⇒ computation))

;; #:bind — the open segment is empty AND there is no delimiter: we are
;; returning to a bind.
(define-typed-syntax (copat-bind [(#:bind) e] [() eelse]) ≫
  (⊢ e ≫ e- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------------
  (⊢ (cond- [(and- (null?- current-vals) (null?- current-frames)) e-]
            [else eelse-])
     ⇒ computation))

;; A delimiter is on top: the open segment is empty and there is a frame.
;; Bind d to the delimiter (a ctype); its segment becomes the open one.
(define-typed-syntax (copat-delim [((~literal %) d:id) ed] [() eelse]) ≫
  ((d ≫ d- : value) ⊢ ed ≫ ed- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------
  (⊢ (cond- [(and- (null?- current-vals) (pair?- current-frames))
             (let- ([d- (frame-name (car- current-frames))])
               (with-stack (frame-vals (car- current-frames)) (cdr- current-frames) ed-))]
            [else eelse-])
     ⇒ computation))

;; A SPECIFIC delimiter is on top. On mismatch the stack is untouched.
(define-typed-syntax (copat-method [((~literal %) (v)) ex] [() eelse]) ≫
  (⊢ v ≫ v- ⇐ value)
  (⊢ ex ≫ ex- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------
  (⊢ (cond- [(and- (null?- current-vals)
                   (pair?- current-frames)
                   (eq?- (frame-name (car- current-frames)) v-))
             (with-stack (frame-vals (car- current-frames)) (cdr- current-frames) ex-)]
            [else eelse-])
     ⇒ computation))

;; A SPECIFIC delimiter is on top, checked WITHOUT popping it: the stack
;; is unchanged in both arms. Used by (upto xs (% m)), which leaves the
;; delimiter in place for the remainder.
(define-typed-syntax (copat-at-method [((~literal %) (v)) ex] [() eelse]) ≫
  (⊢ v ≫ v- ⇐ value)
  (⊢ ex ≫ ex- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------
  (⊢ (cond- [(and- (null?- current-vals)
                   (pair?- current-frames)
                   (eq?- (frame-name (car- current-frames)) v-))
             ex-]
            [else eelse-])
     ⇒ computation))

;; Bind the whole open segment to xs and continue with an empty one.
;; Total; O(1).
(define-typed-syntax (copat-rest [(xs:id) e]) ≫
  ((xs ≫ xs- : value) ⊢ e ≫ e- ⇐ computation)
  ----------------------------------
  (⊢ (let- ([xs- current-vals])
       (with-vals '() e-))
     ⇒ computation))

(define-typed-syntax (pat-tag v [((~literal @) (vt x:id)) ex] [_ eelse]) ≫
  (⊢ v ≫ v- ⇐ value)
  (⊢ vt ≫ vt- ⇐ value)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------
  (⊢ (cond- [(matches-tag? v- vt-)
             (let- ([x- (tagged-args v-)])
               ex-)]
            [else eelse-])
     ⇒ computation)
  )

(define-typed-syntax (case-λ [(#:bind) e] [(x) ex]) ≫
   -----------------------------
   [≻
    (copat-arg
     [(x) ex]
     [() (copat-bind
          [(#:bind) e]
          [() (! error "expected an argument or a returning context, but got some method I've never heard of")])])])

;; (define-typed-syntax (case-λ [(#:bind) e] [(x:id) ex]) ≫
;;   (⊢ e ≫ e- ⇐ computation)
;;   ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
;;   --------------------------------
;;   (⊢ (let- ()
;;        (define- cur (unbox- stack))
;;        (cond-
;;          [(null?- cur) e-]
;;          [else
;;           (define- x- (car- cur))
;;           (set-box!- stack (cdr- cur))
;;           ex-]))
;;      ⇒ computation))

(define-typed-syntax λ
  ([_ () ebod] ≫
   ---------------
   [≻ ebod])
  [(_ (x xs ...) ebod) ≫
   ------------------
   [≻ (copat-arg
       [(x) (λ (xs ...) ebod)]
       [() (! error "expected more arguments but didn't get them")])]])

(define-typed-syntax (^ e1 e2) ≫
  (⊢ e1 ≫ e1- ⇐ computation)
  (⊢ e2 ≫ e2- ⇐ value)
  ----------------
  (⊢ (with-vals (cons- e2- current-vals) e1-)
     ⇒ computation))

;; Push a LIST of values onto the open segment (source order). O(1) when
;; the segment is empty — the common case for `apply` and for restoring
;; a segment on backtrack.
(define-typed-syntax (^@ e xs) ≫
  (⊢ e ≫ e- ⇐ computation)
  (⊢ xs ≫ xs- ⇐ value)
  ----------------
  (⊢ (with-vals (let- ([new xs-] [old current-vals])
                  (if- (null?- old) new (append- new old)))
       e-)
     ⇒ computation))

;; `% m`: close the open segment under the delimiter m and open an empty
;; one. No arity — the delimiter's "arguments" are the segment beneath it.
(define-typed-syntax (^% e vcty) ≫
  (⊢ e ≫ e- ⇐ computation)
  (⊢ vcty ≫ vcty- ⇐ value)
  ----------------
  (⊢ (let- ([m vcty-])
       (unless- (ctype? m)
                (error- "tried to apply something that wasn't a method: " m))
       (with-stack '() (cons- (frame m current-vals) current-frames) e-))
     ⇒ computation))

;; what should be the semantics here?
;; either overwrite the register regardless of if it's set
;; or fail if it's already set
(define-typed-syntax (^: e1 k e2) ≫
  (⊢ e1 ≫ e1- ⇐ computation)
  (⊢ k ≫ k- ⇐ value)
  (⊢ e2 ≫ e2- ⇐ value)
  ----------------
  (⊢ (let- ()
       (unless- (keyword?- k-)
                (error- (format "expected a keyword to assign, but got ~a" k-)))
       (hash-set!- regs k- e2-)
       e1-)
     ⇒ computation))

(define-typed-syntax kw-case-λ
  [(_ [(k:keyword x:id) esucc] [() eelse]) ≫
   --------------------
   [≻ (kw-case-λ [((quoth k) x) esucc] [() eelse])]]
  [(_ [(k x:id) esucc] [() eelse]) ≫
   ((x ≫ x- : value) ⊢ esucc ≫ esucc- ⇐ computation)
   (⊢ k ≫ k- ⇐ value)
   (⊢ eelse ≫ eelse- ⇐ computation)
   ----------------------------------
   (⊢
    (cond-
     [(hash-has-key?- regs k-)         ;; if the register is set
      (let- ([x- (hash-ref- regs k-)]) ;; bind its value to x
        (hash-remove!- regs k-)        ;; and unset it
        esucc-)]
     [(keyword?- k-) eelse-]
     [else
      (error- (format "expected a keyword to match on, but got ~a" k-))])
    ⇒ computation)])

(define-typed-syntax many-app
  [(_ e) ≫
   -------
   [≻ e]]

  [(_ e k:keyword x xs ...) ≫
   ---------------------------
   [≻ (many-app (^: e (quoth k) x) xs ...)]]

  [(_ e (~literal %) v xs ...) ≫
   ---------------------------
   [≻ (many-app (^% e v) xs ...)]]
  
  [(_ e x xs ...) ≫
   -----------------
   [≻ (many-app (^ e x) xs ...)]])

#;
(define-typed-syntax (error e ...) ≫
  (⊢ e ≫ e- ⇐ value) ...
  ---------------------------
  (⊢ (error- e- ...) ⇒ computation))

(define-typed-syntax (main e) ≫
  (⊢ e ≫ e- ⇐ computation)
  ----------------
  (⊢ (let- ([x- (with-stack '() '() e-)]) (void)) ⇒ computation))

(define-typed-syntax (define! x e) ≫
  (⊢ e ≫ e- ⇐ computation)
  #:with x-tmp (generate-temporary #'x)
  --------------------------------------
  (≻
   (begin-
     (define x-tmp (with-stack '() '() e-))    ;; run the computation once against an empty stack
     (define-syntax x (make-variable-like-transformer (assign-type
                                                       #'x-tmp #'value
                                                       #:wrap? #f))))))

(define-typed-syntax (typed-define x e) ≫
  (⊢ e ≫ e- ⇐ value)
  #:with x-tmp (generate-temporary #'x)
  --------------------
  (≻
   (begin-
     (define x-tmp e-)
     (define-syntax x (make-variable-like-transformer (assign-type
                                                       #'x-tmp #'value
                                                       #:wrap? #f))))))
(define-typed-syntax (letrec ([x:id ex] ...) e) ≫
  ((x ≫ x- : value) ... ⊢ (ex ≫ ex- ⇐ value) ... (e ≫ e- ⇐ computation))
  ------------------------------------
  (⊢ (letrec- ([x- ex-] ...) e-) ⇒ computation))
#;
(define-typed-syntax mutual-recursive
  [(_ (define-thunk (! x:id y:id ...) e) ...)] ≫
  ((x ≫ x- : value) ... ⊢ (define-thunk (! x y ...) e) ≫ e- : value) ...
  --------------------------------
  [≻ (begin- e- ...)])

(module+ test
  (require
    rackunit/turnstile
    rackunit)
  
  (check-type #t : value)
  (check-type #f : value)
  (check-type (bind (x (ret #t)) (ret x)) : computation)
  (typecheck-fail (if #t #t #f))
  (check-equal? ((thunk (bind (x (ret #t)) (ret x))) '() '()) #t)
  (check-type (! 3) : computation)
  (check-type (many-app (! 3) 4) : computation)
  (check-type (many-app (! 3) 4 5 6) : computation)
  (check-type (kw-case-λ
               [(#:test x) (ret #t)]
               [() (ret #f)])
              : computation)
  (check-type (copat-arg
               [(x) (ret #t)]
               [() (ret #f)])
              : computation)
  )
