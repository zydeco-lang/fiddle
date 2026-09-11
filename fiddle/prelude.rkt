#lang fiddle

(require (for-syntax syntax/parse
                     (except-in racket/base quote)
                     (only-in "main.rkt" quote)))

(provide Y do do^ ifc define-rec-thunk define-thunk def-thunk def/copat
         pop1 Cons List .n .v $ swap const abort
         list last first second third fourth fifth sixth empty? rest grab-stack dot-args
         rev-apply apply reverse
         copat pm patc pat length Ret Thunk cond
         and or foldl foldl^ foldl1 foldl1^ foldr foldr^ map filter ~ ~! @> @>>

         even? odd?
         ;; "Calling conventions: call-by-value, call-by-name, and method style"
         <<v <<n vo no idiom idiom^

         CBV v> v$
         CBN n> n$

         ;; debugging stuff
         displayall debug
         ;; testing
         test-equal!

         ;; vector stuff
         list<-vector
         apply/vector
         )
(define-syntax (~ syn)
  (syntax-parse syn [(_ e) #'(thunk e)]))
(define-syntax (~! syn)
  (syntax-parse syn [(_ e ...) #'(~ (! e ...))]))
;; A Y combinator to get us moving
(define Y
  (thunk
   (copat-arg
    [(f)
     (let ([self-app (thunk (λ (x) (! f (thunk (! x x)))))])
       ((! self-app) self-app))]
    [() (! error "Y combinator expects one argument, but got none")])))

(define-syntax (do syn)
  (syntax-parse syn
    [(_ [x:id (~literal <-) m] e es ...)
     #`(bind (x m) (do e es ...))]
    [(_ [x:id (~literal =) m] e es ...)
     #`(let ([x m]) (do e es ...))]
    [(_ m) #`m]
    [(_ m e es ...)
     #`(bind (x m) (do e es ...))]))

(define-syntax (ifc syn)
  (syntax-parse syn
    [(_ c e1 e2) #`(bind (x c) (if x e1 e2))]))
(define-syntax (cond syn)
  (syntax-parse syn
    [(_ [#:else e ...]) #`(do e ...)]
    [(_ [(~literal else) e ...]) #`(do e ...)]
    [(_ [b e1 ...] es ...) #`(ifc b (do e1 ...) (cond es ...))]
    [(_) #`(! error 'cond-error "all cond conditions failed: TODO")]))

(define-syntax (define-rec-thunk syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id x:id ...) e)
     #`(define f
         (thunk (letrec ([f (thunk (λ (x ...) e))]) (! f))))]))
(define-syntax (define-thunk syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id x:id ...) e)
     #`(define f (thunk (λ (x ...) e)))]))
;; (define-rec loop (thunk (! loop)))

(define pop1 (thunk (λ (x) (ret x))))
(define Cons (thunk (λ (hd tl) (ret (cons hd tl)))))
#;
(define-thunk (! Y2 f1 f2)
  (let ([f1+2
         (thunk (λ (f b)
                  (ifc (! equal? b 0)
                       (! f1 (thunk (! f 0)) (thunk (! f 1)))
                       (! f2 (thunk (! f 0)) (thunk (! f 1))))))])
    (! Cons (thunk (! f1+2 0)) (thunk (! f1+2 1)))))

;; CBN Composition
(define-thunk (! .n f g x)
  (! f (thunk (! g x))))

;; CBV Composition
(define-thunk (! .v f g x)
  (do [y <- (! g x)]
      (! f y)))

; C combinator
(define-thunk (! swap k x y) (! k y x))

(define-rec-thunk (! const x)
  (case-λ
   [(#:bind) (ret x)]
   [(_) (! const x)]))
(define abort const)
;(! const 3 4 5 6 7 8 9 10) ; 3

(define-rec-thunk (! and)
  (case-λ
   [(#:bind) (ret #t)]
   [(th)
    (cond
      [(! th) (! and)]
      [#:else (! abort #f)])]))
(define-rec-thunk (! or)
  (case-λ
   [(#:bind) (ret #f)]
   [(th)
    (cond
      [(! th) (! abort #t)]
      [#:else (! or)])]))

(define-syntax (list syn)
  (syntax-parse syn
    [(_) #`'()]
    [(_ e es ...)
     #`(cons e (list es ...))]))
(define first car)
(define empty? null?)
(define rest cdr)

(define-rec-thunk (! last lst)
  (do [tl <- (! cdr lst)]
      (cond [(! null? tl) (! car lst)]
            [else         (! last tl)])))

(define-thunk (! second lst)
  (do [tl <- (! cdr lst)]
      (! car tl)))
(define-thunk (! third lst)
  (do [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      (! car lst)))
(define-thunk (! fourth lst)
  (do [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      (! car lst)))
(define-thunk (! fifth lst)
  (do [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      (! car lst)))
(define-thunk (! sixth lst)
  (do [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      (! car lst)))

;; Infinite loop
; (! Y (thunk (λ (loop) (! loop))))

(define-rec-thunk (! sum-loop acc)
  (case-λ
   [(#:bind) (ret acc)]
   [(x) (do [acc^ <- (! + x acc)]
            (! sum-loop acc^))]))

(define-thunk (! sum) (! sum-loop 0))
; (! sum)
; (! sum 1)
; (! sum 1 1)
; (! sum 5 6 7 8) ; 26

;; reverse is imported directly from racket/base via fiddle.rkt (much
;; faster than a user-space rev-loop).
; (! reverse (cons 3 (cons 4 (cons 5 null))))

;; dot-args : forall Y. (List ?v -> Y) -> Y
;; Hand the whole open segment (the values pushed since the last
;; delimiter, in source order) to k as a list. O(1), zero-copy.
(define-thunk (! dot-args k)
  (copat-rest [(xs) (! k xs)]))
(define grab-stack dot-args)
(define-thunk (! List) (! dot-args pop1))

; (! grab-stack pop1 0 1 2 3 4 5)  ; => '(0 1 2 3 4 5)

;; apply : U(X -> ... -> ?c) -> List X -> ?c
;; Push the elements of xs (source order) and force f. O(1) when the
;; open segment is empty; otherwise an append.
(define-thunk (! apply f xs)
  (^@ (! f) xs))

;; rev-apply : U(X -> ... -> ?c) -> List X -> ?c
;; Like apply but xs is given innermost-first (the order an accumulator
;; builds it in). Kept for the legacy sigil scans and exported.
(define-thunk (! rev-apply k xs)
  (do [sx <- (! reverse xs)]
      (^@ (! k) sx)))

;; (define-thunk (! even?)
;;   (letrec ([even? (thunk (λ (x)
;;                            (ifc (! zero? x)
;;                                 (ret #t)
;;                                 (do [x-1 <- (! - x 1)]
;;                                     (! odd? x-1)))))]
;;            [odd? (thunk (λ (x)
;;                            (ifc (! zero? x)
;;                                 (ret #f)
;;                                 (do [x-1 <- (! - x 1)]
;;                                     (! even? x-1)))))])
;;     (! even?)))
(define-thunk (! even? x)
  (do [xmod2 <- (! modulo x 2)]
      (! = xmod2 0)))
(define-thunk (! odd? x)
  (do [xmod2 <- (! modulo x 2)]
      (! = xmod2 1)))

(define-rec-thunk (! map-loop f xs acc)
  (ifc (! empty? xs)
       (! reverse acc)
       (do [x  <- (! first xs)]
           [tl <- (! rest  xs)]
         [y  <- (! f x)]
         (! map-loop f tl (cons y acc)))))
(define-rec-thunk (! map f xs) (! map-loop f xs '()))

(define-rec-thunk (! length-loop acc xs)
  (ifc (! null? xs)
       (ret acc)
       (do [acc <- (! + 1 acc)]
           [xs <- (! cdr xs)]
         (! length-loop acc xs))))

(define-thunk (! length) (! length-loop 0))
;; captures up to lit. match-k should take an abort-k argument and a list
(define-rec-thunk (! up-to-lit match-k abort-k lit seen)
  (copat-arg
   [(x)
    (ifc (! equal? x lit)
         (do [seen~ <- (! reverse seen)]
             ;; restore the sigil too (it was popped by copat-arg above)
             [abort-k <- (ret (thunk (! rev-apply abort-k (cons x seen))))]
             (! match-k abort-k seen~))
         (! up-to-lit match-k abort-k lit (cons x seen)))]
   [() (! rev-apply abort-k seen)]))

;; Multi-sigil upto: capture stack args in a single scan, stopping at
;; the first element in `sigils` (or at end-of-stack when `end?` is #t).
;; Invokes match-k with args-before-sigil and the found sigil (or the
;; keyword #:bind for end-of-stack). Uses Racket's `member` for the
;; per-element sigil check so the inner loop stays out of Fiddle's
;; recursive-thunk layer.
(define-rec-thunk (! up-to-multi match-k abort-k sigils end? seen)
  (copat-arg
   [(x)
    (do [in? <- (! member x sigils)]
        (if in?
            (do [seen~ <- (! reverse seen)]
                [abort-k <- (ret (thunk (! rev-apply abort-k (cons x seen))))]
                (! match-k abort-k seen~ x))
            (! up-to-multi match-k abort-k sigils end? (cons x seen))))]
   [()
    (if end?
        (do [seen~ <- (! reverse seen)]
            [abort-k <- (ret (thunk (! rev-apply abort-k seen)))]
            (! match-k abort-k seen~ '#:bind))
        (! rev-apply abort-k seen))]))

(define-thunk (! test-fail) (! abort 'failure))

(define-thunk (! test-equal! t1 t2)
  (do [x1 <- (! t1)] [x2 <- (! t2)]
    (ifc (! equal? x1 x2)
         (ret #f)
         (! error 'test-fail "expected ~v, got ~v" x2 x1))))

;; ---------------------------------------------------------------------
;; Copattern matching, compiled at expansion time.
;;
;; `copat` is a compiler: each clause's pattern list is turned directly
;; into nested typed primitives (copat-arg / copat-bind / copat-method /
;; ^% / ifc / do / let) with the backtracking continuation threaded
;; statically — the same thing `case-λ` and `λ` do in fiddle.rkt, one
;; level up. Nothing about patterns exists at runtime; the only runtime
;; helpers are the unknown-length scans `up-to-lit`, `up-to-method`,
;; `up-to-multi` and `dot-args`.
;;
;; Semantics are those of the old runtime matcher (`copat-match`):
;;   * clauses are tried in order; a failed clause restores everything
;;     it consumed (re-pushes popped args, re-invokes popped method
;;     frames, re-pushes what an `upto` scan consumed) and the next
;;     clause sees the original stack;
;;   * value sub-expressions in (= e), (% m ...), (upto xs e) are
;;     evaluated once at clause entry, in the scope OUTSIDE the clause's
;;     pattern variables (so [(x (= x)) ...] compares against the outer x);
;;   * (rest xs) grabs all remaining pushed args and must be last;
;;   * #:bind matches only the empty stack and must be last.
;;
;; Cost: one abort thunk per consumed stack element per attempted clause,
;; and none when nothing after that element can fail; one thunk per
;; attempted clause for the fall-through. Value patterns (cons/list/
;; literals) allocate nothing.

(begin-for-syntax
  ;; Bind a restoring abort thunk around the remainder only if the
  ;; remainder can fail; otherwise hand the remainder #f (never used).
  ;;   restore : syntax of a computation that undoes this step's consumption
  ;;   k       : (or/c id #f) -> syntax   compiles the remainder
  (define (with-abort fallible? restore k)
    (if fallible?
        (with-syntax ([(a) (generate-temporaries '(abort))])
          #`(let ([a (~ #,restore)]) #,(k #'a)))
        (k #f)))

  ;; ---- value patterns ---------------------------------------------
  ;; match : (v:id succ:stx fail:stx) -> stx
  ;;   `fail` never pops anything, so it is spliced at each failure
  ;;   point; it is always a single small call.
  ;; hoist : (listof (list tmp:id expr:stx)) evaluated at clause entry.
  (define-syntax-class vpat
    #:attributes (match hoist)
    (pattern x:id
      #:attr hoist '()
      #:attr match (λ (v succ fail) #`(let ([x #,v]) #,succ)))
    (pattern ((~literal =) e:expr)
      #:with (t) (generate-temporaries '(lit))
      #:attr hoist (list (list #'t #'e))
      #:attr match (λ (v succ fail) #`(ifc (! equal? t #,v) #,succ #,fail)))
    (pattern ((~literal quote) e)
      #:attr hoist '()
      #:attr match (λ (v succ fail) #`(ifc (! equal? (quote e) #,v) #,succ #,fail)))
    (pattern (~or e:boolean e:char e:number e:string)
      #:attr hoist '()
      #:attr match (λ (v succ fail) #`(ifc (! equal? e #,v) #,succ #,fail)))
    (pattern ((~literal cons) p:vpat q:vpat)
      #:with (va vd) (generate-temporaries '(car cdr))
      #:attr hoist (append (attribute p.hoist) (attribute q.hoist))
      #:attr match
      (λ (v succ fail)
        #`(ifc (! cons? #,v)
               (do [va <- (! car #,v)]
                   [vd <- (! cdr #,v)]
                 #,((attribute p.match) #'va
                    ((attribute q.match) #'vd succ fail)
                    fail))
               #,fail)))
    ;; (list p ...) is nested cons ending in '(), exactly as the old
    ;; simplify-list-pat did at runtime.
    (pattern ((~literal list) p ...)
      #:with d:vpat (foldr (λ (p acc) #`(cons #,p #,acc)) #''() (syntax->list #'(p ...)))
      #:attr hoist (attribute d.hoist)
      #:attr match (attribute d.match)))

  ;; ---- stack patterns ---------------------------------------------
  ;; step : (abort:id  k:((or/c id #f) -> stx)  fallible?:bool) -> stx
  ;;   `abort` restores everything consumed BEFORE this step; the step
  ;;   must extend it with its own restoration before handing an abort
  ;;   to `k`. `fallible?` says whether the remainder can fail at all.
  ;; hoist : as for vpat.
  (define-syntax-class pat
    #:attributes (step hoist)
    ;; (rest xs) — terminal (enforced by compile-copat); never fails.
    ;; xs := the whole open segment. O(1).
    (pattern ((~literal rest) xs:id)
      #:attr hoist '()
      #:attr step (λ (a k f?) #`(copat-rest [(xs) #,(k #f)])))

    ;; (upto xs (% m)) — xs := the open segment, provided the delimiter
    ;; beneath it is m. O(1), zero-copy. The delimiter is left in place
    ;; for the remainder / body (as the old matcher did). If the check
    ;; fails, or the remainder fails, re-pushing xs onto the (empty)
    ;; segment is an exact inverse.
    ;; (upto xs (% m (p ...))) is normalized to (upto xs (% m)) (% m) p ...
    ;; by normalize-pats below.
    (pattern ((~literal upto) xs:id ((~literal %) m:expr))
      #:with (t) (generate-temporaries '(meth))
      #:attr hoist (list (list #'t #'m))
      #:attr step
      (λ (a k f?)
        #`(copat-rest
           [(xs) (copat-at-method
                  [(% (t)) #,(with-abort f? #`(^@ (! #,a) xs) k)]
                  [() (^@ (! #,a) xs)])])))
    ;; (upto xs #:sigil s lit ... [#:bind]) — single scan for any of
    ;; several literal sigils; s binds the one found (or '#:bind).
    (pattern ((~literal upto) xs:id (~datum #:sigil) s:id lit:expr ...
                              (~optional (~and end-marker (~datum #:bind))))
      #:with end-flag (if (attribute end-marker) #'#t #'#f)
      #:with (t a1) (generate-temporaries '(sigils abort))
      #:attr hoist (list (list #'t #'(list lit ...)))
      #:attr step
      (λ (a k f?)
        #`(! up-to-multi (~ (λ (a1 xs s) #,(k #'a1))) #,a t end-flag '())))
    ;; (upto xs e) — grab args up to a literal sigil.
    (pattern ((~literal upto) xs:id e:expr)
      #:with (t a1) (generate-temporaries '(sigil abort))
      #:attr hoist (list (list #'t #'e))
      #:attr step
      (λ (a k f?)
        #`(! up-to-lit (~ (λ (a1 xs) #,(k #'a1))) #,a t '())))

    ;; (% m) — the delimiter m is on top of an empty segment: pop it; its
    ;; segment becomes the open one. The remainder's abort re-closes the
    ;; (by then restored) segment under m — an exact inverse of the pop.
    ;; (% m (p ...)) is normalized to (% m) p ... by normalize-pats: the
    ;; "arguments" of m are just the values beneath its delimiter.
    (pattern ((~literal %) m:expr)
      #:with (t) (generate-temporaries '(meth))
      #:attr hoist (list (list #'t #'m))
      #:attr step
      (λ (a k f?)
        #`(copat-method
           [(% (t)) #,(with-abort f? #`(! #,a % t) k)]
           [() (! #,a)])))

    ;; bare variable — the common case; bind directly.
    (pattern x:id
      #:attr hoist '()
      #:attr step
      (λ (a k f?)
        #`(copat-arg [(x) #,(with-abort f? #`(! #,a x) k)]
                     [() (! #,a)])))
    ;; any other value pattern at a stack position: pop, then match the
    ;; value; failure (and the remainder's abort) re-push the ORIGINAL.
    (pattern p:vpat
      #:with (v) (generate-temporaries '(arg))
      #:attr hoist (attribute p.hoist)
      #:attr step
      (λ (a k f?)
        #`(copat-arg
           [(v) #,((attribute p.match) #'v
                   (with-abort f? #`(! #,a v) k)
                   #`(! #,a v))]
           [() (! #,a)]))))

  (define (rest-pat? p)
    (syntax-parse p [((~literal rest) _:id) #t] [_ #f]))

  ;; Can anything in the remainder fail? Only an empty remainder without
  ;; #:bind, or a lone (rest xs), cannot.
  (define (tail-fallible? pats bind?)
    (cond [(null? pats) bind?]
          [(rest-pat? (car pats)) #f]
          [else #t]))

  (define (compile-copat steps pats bind? body abort)
    (cond
      [(null? steps)
       (if bind?
           #`(copat-bind [(#:bind) #,body] [() (! #,abort)])
           body)]
      [(rest-pat? (car pats))
       (unless (and (null? (cdr pats)) (not bind?))
         (raise-syntax-error 'copat
                             "(rest xs) must be the last pattern in a clause"
                             (car pats)))
       ((car steps) abort (λ (_) body) #f)]
      [else
       ((car steps) abort
                    (λ (abort^) (compile-copat (cdr steps) (cdr pats) bind? body abort^))
                    (tail-fallible? (cdr pats) bind?))]))

  ;; Wrap the compiled clause in a `let` of its hoisted value
  ;; sub-expressions so they are evaluated once, outside the pattern vars.
  (define (compile-clause steps hoists pats bind? body abort)
    (define hs (apply append hoists))
    (define compiled (compile-copat steps pats bind? body abort))
    (if (null? hs)
        compiled
        #`(let (#,@(for/list ([h (in-list hs)]) #`[#,(car h) #,(cadr h)]))
            #,compiled)))

  ;; Clause-level normalization, before the patterns are parsed:
  ;;   (% m (p ...))             ≡ (% m) p ...
  ;;   (upto xs (% m (p ...)))   ≡ (upto xs (% m)) (% m) p ...
  ;; A delimiter carries no arguments; "its arguments" are the segment
  ;; beneath it, matched as ordinary arg patterns (prefix semantics).
  (define (normalize-pats ps)
    (apply append
           (for/list ([p (in-list ps)])
             (syntax-parse p
               [((~literal %) m (q ...))
                (cons #`(% m) (syntax->list #'(q ...)))]
               [((~literal upto) xs:id ((~literal %) m (q ...)))
                (list* #`(upto xs (% m)) #`(% m) (syntax->list #'(q ...)))]
               [_ (list p)]))))

  (define-syntax-class copat
    #:attributes (compile src)
    (pattern (p0 ... #:bind)
      #:with (p:pat ...) (normalize-pats (syntax->list #'(p0 ...)))
      #:attr src (syntax->datum #'(p0 ... #:bind))
      #:attr compile
      (λ (body abort)
        (compile-clause (attribute p.step) (attribute p.hoist)
                        (syntax->list #'(p ...)) #t body abort)))
    (pattern (p0 ...)
      #:with (p:pat ...) (normalize-pats (syntax->list #'(p0 ...)))
      #:attr src (syntax->datum #'(p0 ...))
      #:attr compile
      (λ (body abort)
        (compile-clause (attribute p.step) (attribute p.hoist)
                        (syntax->list #'(p ...)) #f body abort)))))

;; (copat [(pat ...) body ...] ...)
;; Clause i's abort is a thunk of clause i+1; the last aborts to an
;; error that reports the remaining arguments and the source patterns.
(define-syntax (copat syn)
  (syntax-parse syn
    [(_ [cop:copat e ...] ...)
     (define default
       #`(! dot-args
            (~ (λ (args)
                 (! error 'copattern-match-error
                    "Failed to match the arguments ~v\n\tAgainst the copatterns: ~v"
                    args
                    (quote #,(datum->syntax syn (attribute cop.src))))))))
     (foldr (λ (compile body next)
              (with-syntax ([(a) (generate-temporaries '(abort))])
                #`(let ([a (~ #,next)]) #,(compile body #'a))))
            default
            (attribute cop.compile)
            (syntax->list #'((do e ...) ...)))]))
(define-syntax (pat syn)
  (syntax-parse syn
    [(_ v [p:pat k ...] ...)
     #`((copat [(p) k ...] ...) v)]))
(define-syntax (patc syn)
  (syntax-parse syn
    [(_ c [p:pat k ...] ...)
     #`(bind (v c) (pat v [p k ...] ...))]))

;; (case e [p e] ...)
;; Pattern language
;;   (cons p p)
;;   (= e)
;;   x
(define-syntax (pm syn)
  (syntax-parse syn
    [(_ e [p:pat k ...] ...)
     #`(do [x <- e] ((copat [(p) k ...] ...) x))]))

(define-syntax (do^ syn)
  (syntax-parse syn
    [(_ [x:id ... <- m] e es ...)
     #`(m (thunk (copat [(x ...) (do^ e es ...)])))]
    [(_ m) #`m]))

(define-thunk (! $) (copat [(f) (! f)] [() (! error 'foobar)]))
(define-thunk (! !!) (copat [(f) (! f)] [() (! error 'foobar)]))

; idiom is an implementation of "idiom brackets" ala applicative
; functors.  Expects the stack to consist of a sequence of UF thunks,
; the first of which is a function. Then idiom^ forces the thunks in
; sequence and finally applies the function to the arguments in the
; same order that they were on the stack originally. A simple
; implementation of call-by-value as a macro in cbpv is to translate
; subterms to thunks and translate ! to ! idiom.
(define-rec-thunk (! idiom^ f)
  (copat
   [(th)
    (do [x <- (! th)]
        (! idiom^ (thunk (! f x))))]
   [(#:bind)
    (! f)]))
(define-thunk (! idiom) (! idiom^ $))

(define-thunk (! Ret x) (ret x))

(define-thunk (! Thunk x) (ret (~ (ret x))))

;; Composition operators over STACK DELIMITERS.
;;
;;   (! <<v f a b % vo g c % v$)   ; call-by-value composition: f a b (g c)
;;   (! <<n k f a % no g c % n$)   ; call-by-name: stages passed as thunks
;;
;; Each stage's arguments are exactly the values between two delimiters,
;; so a stage is `(rest xs)` — O(1), no scan, no reverse. The chain ends
;; at the end delimiter, at end-of-stack (#:bind — the caller's remaining
;; arguments then land in the final stage), or at any foreign delimiter,
;; which is left in place for the final callee.
;; NB: typed definitions are type-checked eagerly in module pass 1, so
;; these must precede every definition that mentions them. v$ / n$ are
;; also the end markers of the CBV / CBN combinators further down.
(define! vo (! new-method 'cbv-o))
(define! v$ (! new-method 'cbv-end))
(define! no (! new-method 'cbn-o))
(define! n$ (! new-method 'cbn-end))

;; ---- legacy sigil-based implementations ---------------------------
;; Kept temporarily as a fallback while the corpus migrates from the
;; symbol sigils 'o / '$ to % vo / % v$. Reached only when a chain ends
;; (bind or foreign delimiter) and the final stage still contains a
;; sigil. To be deleted once the migration is complete.
(define-thunk (! legacy-sigils? xs)
  (! or (~ (! member 'o xs)) (~ (! member '$ xs))))

(define-rec-thunk (! <<v-impl/sigil k)
  (copat
   [(f (upto xs #:sigil s 'o '$ #:bind))
    (cond
      [(! equal? s 'o)
       (let ([k (thunk (λ (y)
                         (do [z <- (! apply f xs y)]
                             (! k z))))])
         (! <<v-impl/sigil k))]
      [#:else
       (do [z <- (! apply f xs)]
           (! k z))])]))

(define-rec-thunk (! <<n-impl/sigil)
  (copat
   [(k f (upto xs #:sigil s 'o '$ #:bind))
    (cond
      [(! equal? s 'o)
       (let ([k (thunk (copat [(y) (! k (thunk (! apply f xs y)))]))])
         (! <<n-impl/sigil k))]
      [#:else
       (! k (thunk (! apply f xs)))])]))

;; ---- delimiter-based implementations ------------------------------
(define-rec-thunk (! <<v-impl k)
  (copat
   [(f (rest xs))
    (copat-delim
     [(% d)
      (cond
        [(! eq? d vo)
         (! <<v-impl (~ (λ (y) (do [z <- (! apply f xs y)] (! k z)))))]
        [(! eq? d v$)
         (do [z <- (! apply f xs)] (! k z))]
        [#:else
         ;; foreign delimiter: re-install it, then terminate the chain
         (ifc (! legacy-sigils? xs)
              (! apply (~ (! <<v-impl/sigil k f)) xs % d)
              (do [z <- (! apply f xs % d)] (! k z)))])]
     [()
      ;; end of stack: the caller's remaining args are the last stage's
      (ifc (! legacy-sigils? xs)
           (! apply (~ (! <<v-impl/sigil k f)) xs)
           (do [z <- (! apply f xs)] (! k z)))])]
   [() (! error "<<v: expected a function")]))

(define-thunk (! <<v) (! <<v-impl Ret))

(define-rec-thunk (! <<n-impl)
  (copat
   [(k f (rest xs))
    (copat-delim
     [(% d)
      (cond
        [(! eq? d no)
         (! <<n-impl (~ (copat [(y) (! k (~ (! apply f xs y)))])))]
        [(! eq? d n$)
         (! k (~ (! apply f xs)))]
        [#:else
         (ifc (! legacy-sigils? xs)
              (! apply (~ (! <<n-impl/sigil k f)) xs % d)
              (! k (~ (! apply f xs)) % d))])]
     [()
      (ifc (! legacy-sigils? xs)
           (! apply (~ (! <<n-impl/sigil k f)) xs)
           (! k (~ (! apply f xs))))])]
   [() (! error "<<n: expected a continuation and a function")]))
(define-thunk (! <<n) (! <<n-impl $))

(define-thunk (! beep) (ret "beep"))
(define-thunk (! fc)
  (copat
   [(th) (! th)]))
;(! <<n fc 'o beep '$)


(define-rec-thunk (! foldl l step acc)
  (cond
    [(! empty? l) (ret acc)]
    [#:else
     (do [x <- (! car l)]
         [xs <- (! cdr l)]
       [acc <- (! step acc x)]
       (! foldl xs step acc))]))

(define-thunk (! foldl1 xs step)
  (cond [(! null? xs) (! error "tried to foldl1 with an empty list")]
        [else [x <- (! first xs)] [xs <- (! rest xs)]
              (! foldl xs step x)]))

(define-thunk (! foldl1^ step xs) (! foldl1 xs step))

;; (define-rec-thunk (! map f l)
;;   (! <<v reverse 'o
;;      foldl l
;;      (thunk
;;       (copat
;;        [(acc x)
;;         (do [y <- (! f x)]
;;             (ret (cons y acc)))]))
;;      '() '$))

(define-syntax (def/copat syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id p:pat ...) ms ...)
     #`(define-rec-thunk (! f p ...) (copat ms ...))]))
(define-syntax (def-thunk syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id x:id ... p:pat ... #:bind) es ...)
     #`(define-rec-thunk (! f x ...) (copat [(p ... #:bind) es ...]))]
    [(_ ((~literal !) f:id x:id ... p:pat ...) es ...)
     #`(define-rec-thunk (! f x ...) (copat [(p ...) es ...]))]))

(define-rec-thunk (! filter p xs)
  (cond
    [(! empty? xs) (ret '())]
    [#:else
     (do [x <- (! car xs)]
         [xs <- (! <<v filter p % vo cdr xs % v$)]
       (ifc (! p x)
            (ret (cons x xs))
            (ret xs)))]))

(def/copat (! debug)
  [(x #:bind) (! displayln x) (ret x)]
  [(x) (! displayln x) (! debug)])

(def-thunk (! @> x f) (! f x))
(def-thunk (! @>> xs f) (! apply f xs))
(def-thunk (! foldl^ step acc l) (! foldl l step acc))
(def-thunk (! foldr l step acc) (! <<v foldl^ (~ (! swap step)) acc % vo reverse l))
(def-thunk (! foldr^ step acc l) (! foldr l step acc))

;; Debugging primitives
(def/copat (! displayall)
  [(x) (! displayln x) (! displayall)]
  [() (ret #f)])

;; enough to monad?
#;
(define-syntax (mdo syn)
  (syntax-parse syn
    [(_ [x:id (~literal <-) m] e es ...)
     #`(bind (x m) (do e es ...))]
    [(_ [x:id (~literal =) m] e es ...)
     #`(let ([x m]) (do e es ...))]
    [(_ m) #`m]
    [(_ m e es ...)
     #`(bind (x m) (do e es ...))]))

;; ;; example:
;; ;;   (! CBV f o g $ inp x y)
;; ;;   =~
;; ;;   (do [gv <- (! g inp)] (! f gv x y))

;; ;; CBVo[X,Y] = { '$ : X -> Y, 'o : ∀ W. U(W -> F X) -> CBVo[W,Y] }
;; ;; CBV : ∀ X Y. U(X -> Y) -> CBVo[X,Y]
;; (def/copat (! CBV f)
;;   [((= '$) x #:bind) (! f x)]
;;   [((= 'o) g)
;;    (! CBV (~ (! .v f g)))])

;; ;; example:
;; ;;   (! CBN

;; ;; CBN>>[Y] = { '! : Y, '>> : ∀ Z. U(UY -> Z) -> CBN>>[Z] }
;; ;; CBN : ∀ Y. UY -> CBN>>[Y]
;; (def/copat (! CBN c)
;;   [((= '!)) (! c)]
;;   [((= '>>) f) (! CBN (~ (! f c)))])

(def-thunk (! negative? x)
  (! < x 0) )

(def-thunk (! apply/vector-loop k v ix)
  (cond [(! negative? ix) (! k)]
        [else
         [elt <- (! vector-ref v ix)]
         [next-ix <- (! - ix 1)]
         (! apply/vector-loop k v next-ix elt)]))

(def-thunk (! apply/vector k v)
  [last-ix <- (! <<v swap - 1 % vo vector-length v)]
  (! apply/vector-loop k v last-ix)
  )

(def-thunk (! list<-vector)
  (! apply/vector List))

(define! chest (! new-method 'chest))
(define! unit (! new-method 'unit))
(define! duo (! new-method 'duo))

((copat-method [(% (unit)) (ret 3)] [() (ret 4)]) % unit)

((copat [((% unit)) (ret 3)] [() (ret 4)]) % unit)
(def/copat (! ununit x) [((% unit ())) (ret x)])


;; Nominal combinators

(define! v> (! new-method 'cbv-compose))
;; v$ is defined with the <<v delimiters above.
(def-thunk (! CBV> t u)
  [x <- (! t)]
  (! u x))
(def/copat (! CBV t)
  [((% v> (u)))(! CBV (~ (! CBV> t u)))]
  [((% v$))     (! t)]
  [() (! error "CBV composition: expected either another thunk or an end of args method, but got:")])

(define! n> (! new-method 'cbn-compose))
;; n$ is defined with the <<n delimiters above.
(def/copat (! CBN t)
  [((% n> (u))) (! CBN (~ (! u t)))]
  [((% n$ ()))  (! t)]
  [() (! error "CBN composition: expected another composition % no or an end of args marker")])

(define! pair (! new-tag 'pair 2))
(define! mt   (! new-tag 'mt 0))
(def/copat (! nom<-list)
  [('()) (! Tag mt)]
  [((cons x xs)) (! idiom^ (~! Tag pair x) (~! nom<-list xs))])

(define! p (! Tag pair 0 1))
(define! the-mt (! Tag mt))


(def-thunk (! list<-nom x)
  (pat-tag x
   [(@ (mt dud)) (ret '())]
   [else 
    (pat-tag x
      [(@ (pair hd*tl))
       (do [hd <- (! first hd*tl)]
           [tl <- (! second hd*tl)]
         (! idiom^ (~! Cons hd) (~! list<-nom tl)))]
      [_ (! error )])]))
