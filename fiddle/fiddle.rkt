#lang turnstile

;; A CBPV Scheme-like
;; 
;; See initialize.rkt for a description of the runtime state
(require (rename-in racket/function (thunk thunk-))
         (only-in racket/unsafe/ops
                  unsafe-unbox* unsafe-set-box*!
                  unsafe-car unsafe-cdr
                  unsafe-cons-list)
         "initialize.rkt"
         (for-syntax syntax/parse))
(provide (all-defined-out)
         (rename-out (many-app #%app))
         matches-method? matches-tag?
         invoke-method)
(define (force- th) (th))

(define-base-type value)
(define-base-type computation)

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

(require-fo-wrapped-provide racket/base error)
(require-fo-wrapped-provide "initialize.rkt" new-method)
(require-fo-wrapped-provide "initialize.rkt" new-tag)
(require-fo-wrapped-provide "initialize.rkt" Tag)

;; Copat matcher struct types (see copat-structs.rkt).
(require-fo-wrapped-provide "copat-structs.rkt" arg-copat)
(require-fo-wrapped-provide "copat-structs.rkt" arg-copat?)
(require-fo-wrapped-provide "copat-structs.rkt" arg-copat-pat)
(require-fo-wrapped-provide "copat-structs.rkt" arg-copat-rest)
(require-fo-wrapped-provide "copat-structs.rkt" upto-copat)
(require-fo-wrapped-provide "copat-structs.rkt" upto-copat?)
(require-fo-wrapped-provide "copat-structs.rkt" upto-copat-sigil)
(require-fo-wrapped-provide "copat-structs.rkt" upto-copat-rest)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-copat)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-copat?)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-copat-sigils)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-copat-end?)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-copat-rest)
(require-fo-wrapped-provide "copat-structs.rkt" kw-copat)
(require-fo-wrapped-provide "copat-structs.rkt" kw-copat?)
(require-fo-wrapped-provide "copat-structs.rkt" kw-copat-kw)
(require-fo-wrapped-provide "copat-structs.rkt" kw-copat-pat)
(require-fo-wrapped-provide "copat-structs.rkt" kw-copat-rest)
(require-fo-wrapped-provide "copat-structs.rkt" method-copat)
(require-fo-wrapped-provide "copat-structs.rkt" method-copat?)
(require-fo-wrapped-provide "copat-structs.rkt" method-copat-m)
(require-fo-wrapped-provide "copat-structs.rkt" method-copat-pat)
(require-fo-wrapped-provide "copat-structs.rkt" method-copat-rest)
(require-fo-wrapped-provide "copat-structs.rkt" upto-syn)
(require-fo-wrapped-provide "copat-structs.rkt" upto-syn?)
(require-fo-wrapped-provide "copat-structs.rkt" upto-syn-sigil)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-syn)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-syn?)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-syn-sigils)
(require-fo-wrapped-provide "copat-structs.rkt" upto-multi-syn-end?)
(require-fo-wrapped-provide "copat-structs.rkt" kw-syn)
(require-fo-wrapped-provide "copat-structs.rkt" kw-syn?)
(require-fo-wrapped-provide "copat-structs.rkt" kw-syn-kw)
(require-fo-wrapped-provide "copat-structs.rkt" kw-syn-pat)
(require-fo-wrapped-provide "copat-structs.rkt" method-syn)
(require-fo-wrapped-provide "copat-structs.rkt" method-syn?)
(require-fo-wrapped-provide "copat-structs.rkt" method-syn-m)
(require-fo-wrapped-provide "copat-structs.rkt" method-syn-pat)
(require-fo-wrapped-provide "copat-structs.rkt" lit-pat)
(require-fo-wrapped-provide "copat-structs.rkt" lit-pat?)
(require-fo-wrapped-provide "copat-structs.rkt" lit-pat-v)
(require-fo-wrapped-provide "copat-structs.rkt" cons-pat)
(require-fo-wrapped-provide "copat-structs.rkt" cons-pat?)
(require-fo-wrapped-provide "copat-structs.rkt" cons-pat-car)
(require-fo-wrapped-provide "copat-structs.rkt" cons-pat-cdr)
(require-fo-wrapped-provide "copat-structs.rkt" list-pat)
(require-fo-wrapped-provide "copat-structs.rkt" list-pat?)
(require-fo-wrapped-provide "copat-structs.rkt" list-pat-ps)
(require-fo-wrapped-provide "copat-structs.rkt" method-pat)
(require-fo-wrapped-provide "copat-structs.rkt" method-pat?)
(require-fo-wrapped-provide "copat-structs.rkt" method-pat-m)
(require-fo-wrapped-provide "copat-structs.rkt" method-pat-pat)
(require-fo-wrapped-provide "copat-structs.rkt" method-only)
(require-fo-wrapped-provide "copat-structs.rkt" method-only?)
(require-fo-wrapped-provide "copat-structs.rkt" method-only-m)

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
   (let- ([x (unsafe-unbox* stack)])
     (if- (null?- x)
          e-
          (error- (format "expected a return address on the stack but got stack ~a" x))))
   ⇒ computation))

(define-typed-syntax (bind (x:id e) e^) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ e^ ≫ e^- ⇐ computation)
  -----------------
  (⊢ (let- ()
       (define tmp (unsafe-unbox* stack)) ;; Save the current stack
       (unsafe-set-box*! stack '())       ;; Hide the stack from e
       (define x- e-)          ;; run e
       (unsafe-set-box*! stack tmp)       ;; restore the stack
       e^-)
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
  (⊢ (thunk- e-) ⇒ value))

(define-typed-syntax (basic-! e) ≫
  (⊢ e ≫ e- ⇐ value)
  ----------------
  (⊢ (force- e-) ⇒ computation))

(define-typed-syntax (! e es ...) ≫
  ------------------------
  (≻ (many-app (basic-! e) es ...)))

(define-typed-syntax (copat-arg [(x:id) ex] [() e]) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  ----------------------------------------
  (⊢ (let- ()
           (define- cur (unsafe-unbox* stack))
           (cond- [(pair?- cur)
                  (define- x- (unsafe-car cur))
                  (unsafe-set-box*! stack (unsafe-cdr cur))
                  ex-]
                 [else e-]))
     ⇒ computation))

(define-typed-syntax (copat-bind [(#:bind) e] [() eelse]) ≫
  (⊢ e ≫ e- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------------
  (⊢ (let- ()
           (define- cur (unsafe-unbox* stack))
           (cond- [(null?- cur) e-]
                  [else         eelse-]))
     ⇒ computation))

(define-typed-syntax (copat-method [((~literal %) (v x:id)) ex] [() eelse]) ≫
  (⊢ v ≫ v- ⇐ value)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------
  (⊢ (let- ()
           (define- cur (unsafe-unbox* stack))
           (cond- [(matches-method? cur v-)
                   (define- x- (method-args cur))
                   (unsafe-set-box*! stack (method-tl cur))
                   ex-]
                  [else         eelse-]))
     ⇒ computation)
  )

(define-typed-syntax (pat-tag v [((~literal @) (vt x:id)) ex] [_ eelse]) ≫
  (⊢ v ≫ v- ⇐ value)
  (⊢ vt ≫ vt- ⇐ value)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  (⊢ eelse ≫ eelse- ⇐ computation)
  ----------------------------------
  (⊢ (let- ()
           (cond- [(matches-tag? v- vt-)
                   (define- x- (tagged-args v-))
                   ex-]
                  [else eelse-]))
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
  (⊢ (let- ()
           (unsafe-set-box*! stack (unsafe-cons-list e2- (unsafe-unbox* stack)))
           e1-)
     ⇒ computation))

(define-typed-syntax (^% e vcty) ≫
  (⊢ e ≫ e- ⇐ computation)
  (⊢ vcty ≫ vcty- ⇐ value)
  ----------------
  (⊢ (let- ()
           (define- cur (unsafe-unbox* stack))
           (unsafe-set-box*! stack (invoke-method cur vcty-))
           e-)
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
      (define- x- (hash-ref- regs k-)) ;; bind its value to x
      (hash-remove!- regs k-)          ;; and unset it
      esucc-]
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
  (⊢ (let- ([x- e-]) (void)) ⇒ computation))

(define-typed-syntax (define! x e) ≫
  (⊢ e ≫ e- ⇐ computation)
  #:with x-tmp (generate-temporary #'x)
  --------------------------------------
  (≻
   (begin-
     (define x-tmp e-)
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
  (check-equal? (bind (x (ret #t)) (ret x)) #t)
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
