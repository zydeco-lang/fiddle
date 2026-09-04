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
         <<v <<n oo idiom idiom^

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

;; stack-loop : forall X. (X -> ?c) -> X -> (U (?v -> X -> F X)) -> ?c
;; aka stack-foldl
(define-rec-thunk (! stack-loop k acc cons)
  (copat-arg
   [(x)
    (do [acc^ <- (! cons x acc)]
        (! stack-loop k acc^ cons))]
   [() (! k acc)]))

(define-thunk (! grab-rev-stack k)
  (! stack-loop k '() Cons))

; (! grab-rev-stack pop1 0 1 2 3 4 5)

(define-thunk (! grab-stack k)
  (! grab-rev-stack
     (thunk
      (λ (rev-stack)
        (do [stack <- (! reverse rev-stack)]
            (! k stack))))))

; (! grab-stack pop1 0 1 2 3 4 5)

; dot-args : forall Y. (List ?v -> Y) -> Y
(define dot-args grab-stack)
(define-thunk (! List) (! dot-args pop1))

; rev-apply : U(X -> ... -> ?c) -> List X -> ?c
(define-rec-thunk (! rev-apply k xs)
  (ifc (! null? xs)
       (! k)
       (do [hd <- (! car xs)]
           [tl <- (! cdr xs)]
         (! rev-apply k tl hd))))

(define-thunk (! apply f xs)
  (do [sx <- (! reverse xs)]
      (! rev-apply f sx)))

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
;; Copattern matching

;; A copattern is one of
;;   end-copat -- matches a kont (F stack)
;;   (list 'arg pat copat) -- matches an arg with pat, then the rest with copat
;;   'any-stack -- matches any stack, binds no variables
;;   'rest -- matches any stack, pops off all arguments until it finds a method/kont
;;   (list 'upto lit copat) -- grabs the stack upto lit as a list, then proceeds as copat
;;   (list 'method ty 'var copat) matches against the method ty, binds the method args to var and proceeds as copat
;;   (list 'method ty (pat ...) copat) matches against the method, matches the args of the method against pat, and proceeds as copat
;;   (list 'keyword kw pat copat) -- matches pat on the contents of the kw register, failing if the register is not set
(define! End (! new-tag 'end 0))
(define! end-copat (! Tag End))
(define-thunk (! end-copat? c)
  (pat-tag c
           [(@ (End _)) (ret #t)]
           [else (ret #f)]))
(define-thunk (! any-stack-copat?) (! equal? 'any-stack))
(define-thunk (! rest-copat?)      (! equal? 'rest))

;; arg-copat?, upto-copat?, kw-copat?, method-copat?,
;; lit-pat?, cons-pat?, list-pat?, method-pat?, method-only?,
;; upto-syn?, kw-syn?, method-syn? are struct predicates provided by
;; fiddle.rkt (see copat-structs.rkt). Field accessors likewise.
(define-thunk (! var-pat? pat) (! equal? pat 'var))

;; Parses one level of macro syntax for a copattern into a copattern
(define-rec-thunk (! view-copat syn)
  (cond
    [(! null? syn) (ret 'any-stack)]
    [(! cons? syn)
     (do [hd <- (! car syn)] [tl <- (! cdr syn)]
       (cond
         [(! or (~ (! end-copat? hd)) (~ (! equal? 'rest hd)))
          (ret hd)]
         [(! upto-syn? hd)
          [sigil <- (! upto-syn-sigil hd)]
          (! upto-copat sigil tl)]
         [(! upto-multi-syn? hd)
          [sigils <- (! upto-multi-syn-sigils hd)]
          [end?   <- (! upto-multi-syn-end? hd)]
          (! upto-multi-copat sigils end? tl)]
         [(! kw-syn? hd)
          [kw  <- (! kw-syn-kw hd)]
          [pat <- (! kw-syn-pat hd)]
          (! kw-copat kw pat tl)]
         [(! method-syn? hd)
          [m   <- (! method-syn-m hd)]
          [pat <- (! method-syn-pat hd)]
          (! method-copat m pat tl)]
         ;; anything else is an arg-copat with `hd` as the arg pattern.
         [#:else (! arg-copat hd tl)]))]))

;; captures up to lit. match-k should take an abort-k argument and a list
(define-rec-thunk (! up-to-lit match-k abort-k lit seen)
  (copat-arg
   [(x)
    (ifc (! equal? x lit)
         (do [seen~ <- (! reverse seen)]
             [abort-k <- (ret (thunk (! rev-apply abort-k seen)))]
             (! match-k abort-k seen~))
         (! up-to-lit match-k abort-k lit (cons x seen)))]
   [() (! rev-apply abort-k seen)]))

(define-rec-thunk (! up-to-method match-k abort-k method seen)
  (copat-method
   [(% (method xs)) ;; done, now return seen and xs to match-k
    (do [nees <- (! reverse seen)]
        (! match-k (~ (! apply (~ (! rev-apply abort-k seen % method)) xs)) nees xs))]
   [()
    (copat-arg
     [(x) ;; more arguments, push it onto seen and continue
      (! up-to-method match-k abort-k method (cons x seen))]
     [() ;; something else (other method, bind) so fail
      (! rev-apply abort-k seen)])]))

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

(define-rec-thunk (! simplify-list-pat pats)
  (ifc (! null? pats)
       (! lit-pat '())
       (do [hd <- (! car pats)]
           [tl <- (! cdr pats)]
         [tl-pat <- (! simplify-list-pat tl)]
         (! cons-pat hd tl-pat))))

(define-rec-thunk (! simplify-pat raw)
  (ifc (! list-pat? raw)
       (do [ps <- (! list-pat-ps raw)]
           (! simplify-list-pat ps))
       (ret raw)))
;; Attempt to match the stack against a copattern.
;;   exec match-k on success with args as determined by the copat
;;   exec abort-k on failure with the current stack
(define-rec-thunk (! copat-match match-k abort-k syn)
  (do [copat <- (! view-copat syn)]
      (pat-tag copat
       [(@ (End _))
        (copat-bind
         [(#:bind) (! match-k)]
         [() (! abort-k)])]
       [else
      (cond [(! any-stack-copat? copat) (! match-k)]
            [(! rest-copat? copat) (! dot-args match-k)]
            [(! upto-copat? copat)
             [sigil <- (! upto-copat-sigil copat)]
             [tl-copat <- (! upto-copat-rest copat)]
             (cond [(! lit-pat? sigil)
                    [lit <- (! lit-pat-v sigil)]
                    (! up-to-lit
                       (thunk
                        (λ (abort-k xs)
                          (! copat-match (~ (! match-k xs)) abort-k tl-copat)))
                       abort-k lit '())]
                   [(! method-only? sigil)
                    [m <- (! method-only-m sigil)]
                    (! up-to-method
                       (~ (λ (abort-k xs m-args)
                            (! apply (~ (! copat-match (~ (! match-k xs)) abort-k tl-copat % m)) m-args)))
                       abort-k m '())]
                   [(! method-pat? sigil)
                    [m   <- (! method-pat-m sigil)]
                    [pat <- (! method-pat-pat sigil)]
                    (! up-to-method
                       (~ (λ (abort-k xs m-args)
                            (! copat-match (~ (! match-k xs))
                                           (~ (λ (args) (! apply (~ (! abort-k % m)) args)))
                                           (cons pat tl-copat)
                                           m-args)))
                       abort-k m '())])
             ]
            [(! upto-multi-copat? copat)
             [sigils   <- (! upto-multi-copat-sigils copat)]
             [end?     <- (! upto-multi-copat-end? copat)]
             [tl-copat <- (! upto-multi-copat-rest copat)]
             (! up-to-multi
                (~ (λ (abort-k xs sigil)
                     (! copat-match (~ (! match-k xs sigil)) abort-k tl-copat)))
                abort-k sigils end? '())]
            [(! kw-copat? copat)
             [kw    <- (! kw-copat-kw copat)]
             [pat   <- (! kw-copat-pat copat)]
             [copat <- (! kw-copat-rest copat)]
             (kw-case-λ
              [(kw x)
               ; we need to restore the register if we abort
               (! copat-match match-k (~ (λ (x) (^: (! abort-k) kw x)))
                  (cons pat copat)
                  x)]
              [() (! abort-k)])]
            [(! arg-copat? copat)
             [raw-pat <- (! arg-copat-pat copat)]
             [pat     <- (! simplify-pat raw-pat)]
             [copat   <- (! arg-copat-rest copat)]
             (copat-arg
              [(x)
               (cond
                 [(! var-pat? pat)
                  (! copat-match (~ (! match-k x)) (~ (! abort-k x)) copat)]
                 [(! lit-pat? pat)
                  [lit <- (! lit-pat-v pat)]
                  (cond [(! equal? lit x)
                         (! copat-match match-k (~ (! abort-k x)) copat)]
                        [#:else (! abort-k x)])]
                 [(! cons-pat? pat)
                  [car-pat <- (! cons-pat-car pat)]
                  [cdr-pat <- (! cons-pat-cdr pat)]
                  (cond [(! cons? x)
                         [x-car <- (! car x)] [x-cdr <- (! cdr x)]
                         (! copat-match match-k (~ (λ (x y) (! abort-k (cons x y))))
                            (cons car-pat (cons cdr-pat copat))
                            x-car x-cdr)]
                        [#:else (! abort-k x)])]
                 )]
              [() (! abort-k)])]
            [(! method-copat? copat)
             [method <- (! method-copat-m copat)]
             [pat    <- (! method-copat-pat copat)]
             [copat  <- (! method-copat-rest copat)]
             (copat-method
              [(% (method xs))
               (! copat-match match-k (~ (λ (xs) (! apply (~ (! abort-k % method)) xs)))
                  (cons pat copat)
                  xs)]
              [()
               (! abort-k)])
             ]
            [else (! error "copattern matching syntax error, unrecognized copattern: " copat)])])))

(define-rec-thunk (! try-copatterns copat*ks abort-k)
  (ifc (! null? copat*ks)
       (! abort-k)
       (do [copat*kont <- (! first copat*ks)]
           [rest       <- (! rest copat*ks)]
         [copat <- (! first copat*kont)]
         [match-k <- (! second copat*kont)]
         (! copat-match match-k (~ (! try-copatterns rest abort-k)) copat))))

(define-thunk (! try-copatterns-default-error copat*ks)
  (! try-copatterns
     copat*ks
     (thunk
      (do
          (!
           dot-args
           (thunk
            (λ (args)
              (do [copats <- (! map first copat*ks)]
                  (! error 'copattern-match-error
                     "Failed to match the arguments ~v\n\tAgainst the copatterns: ~v"
                     args
                     copats)))))))))

(define-thunk (! test-fail) (! abort 'failure))

(define copat-ex0 '())
(define copat-ex1 '((lit hd) (lit tl) var))
(define copat-ex2 '(var))

(define-thunk (! test-equal! t1 t2)
  (do [x1 <- (! t1)] [x2 <- (! t2)]
    (ifc (! equal? x1 x2)
         (ret #f)
         (! error 'test-fail "expected ~v, got ~v" x2 x1))))

#;
(do (! test-equal! (~ (copat [() (ret 0)])) (~ (ret 0)))
    (! test-equal! (~ ((copat [((= 3)) (ret 0)] [() (! abort #f)]) 3))
       (~ (ret 0)))
  (! test-equal! (~ ((copat [((= 3)) (ret 0)] [() (! abort #f)])))
     (~ (ret #f)))
  (! test-equal! (~ ((copat [((= 3)) (ret 0)] [() (! abort #f)]) 4))
     (~ (ret #f)))
  (! test-equal! (~ ((copat [((upto xs 3)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
     (~ (ret '(0 1 2))))
  (! test-equal! (~ ((copat [((= 0) (upto xs 3)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
     (~ (ret '(1 2))))
  (! test-equal! (~ ((copat [((= 0) (upto xs 3)) (ret xs)] [() (! abort #f)]) 0 1 2))
     (~ (ret #f)))
  (! test-equal! (~ ((copat [((rest xs)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
     (~ (ret '(0 1 2 3))))
  (! test-equal! (~ ((copat [((= 0) (rest xs)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
     (~ (ret '(1 2 3))))
  (! test-equal! (~ ((copat [((cons x y)) (ret (cons x y))] [() (! abort #f)]) (list 0)))
     (~ (ret (list 0))))
  
  (! test-equal! (~ ((copat [(x (cons y z)) (ret (cons x (cons y z)))] [() (! abort #f)]) 0 (list 1)))
     (~ (ret (list 0 1))))

  (! test-equal! (~ ((copat [((cons (cons a b) c) (cons x (cons y z))) (ret (list a b c x y z))] [() (! abort #f)]) (cons (cons 0 1) 2) (cons 3 (cons 4 5))))
     (~ (ret (list 0 1 2 3 4 5))))
  ;; backtracking tests
  (! test-equal! (~ ((copat
                      [(x (= 'backtrack)) (ret #f)]
                      [((rest args)) (ret args)]
                      [() (! abort #f)])
                     0 'wrong))
     (~ (ret (list 0 'wrong))))
  (! test-equal! (~ ((copat
                      [((cons x y) (= 'backtrack)) (ret #f)]
                      [((rest args)) (ret args)]
                      [() (! abort #f)])
                     (cons 0 1) 'wrong))
     (~ (ret (list (cons 0 1) 'wrong))))
  (! test-equal! (~ ((copat
                      [((cons (cons a b) c) (cons x (cons (= 'backtrack) z))) (ret (list a b c x 'bktrk z))]
                      [((rest args)) (ret args)]
                      [() (! abort #f)])
                     (cons (cons 0 1) 2) (cons 3 (cons 4 5))))
     (~ (ret (list (cons (cons 0 1) 2) (cons 3 (cons 4 5))))))
  (! test-equal! (~ ((copat
                      [(#:bind) (ret #t)]
                      [() (! abort #f)])))
     (~ (ret #t)))
  (! test-equal! (~ ((copat
                      [(#:bind) (ret #t)]
                      [(x) (ret x)]) #f))
     (~ (ret #f)))
  ;; (should add/todo) upto tests, more rest tests
  (ret 'stdlib-tests-all-pass))

(begin-for-syntax
  ;; Each pattern (`.pattern` attribute) is now a Fiddle *computation*
  ;; returning a pat / copat-syn value. The `copat` class threads them
  ;; together via `do` so the enclosing list can hold plain values.
  (define-syntax-class meth-args-pat
    #:attributes (pattern all-vars)
    (pattern x:id
     #:attr pattern #`(ret 'var)
     #:attr all-vars #'(x))
    (pattern (p:pat ...)
     #:with (v ...) (generate-temporaries #'(p ...))
     #:attr pattern #`(do [v <- p.pattern] ... (! list-pat (list v ...)))
     #:attr all-vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))
    )

  (define-syntax-class pat
    #:attributes (pattern all-vars)
    (pattern
     x:id
     #:attr pattern #`(ret 'var)
     #:attr all-vars #'(x))

    (pattern
     ((~literal =) e:expr)
     #:attr pattern #`(! lit-pat e)
     #:attr all-vars #'())

    (pattern
     ((~literal upto) xs:id ((~literal %) v))
     #:attr pattern #`(do [s <- (! method-only v)] (! upto-syn s))
     #:attr all-vars #'(xs))
    (pattern
     ((~literal upto) xs:id ((~literal %) v m:meth-args-pat))
     #:attr pattern #`(do [mp <- m.pattern]
                          [s  <- (! method-pat v mp)]
                          (! upto-syn s))
     #:attr all-vars #`#,(cons #`xs
                               (syntax-e #`m.all-vars)))

    ;; multi-sigil upto:
    ;;   (upto xs #:sigil s lit ... [#:bind])
    ;; xs binds to args before the terminator; s binds to the found
    ;; sigil value (or the keyword #:bind for end-of-stack, if allowed).
    (pattern
     ((~literal upto) xs:id
                      (~datum #:sigil) s:id
                      lit:expr ...
                      (~optional (~and end-marker (~datum #:bind))))
     #:with end-flag (if (attribute end-marker) #'#t #'#f)
     #:attr pattern #`(! upto-multi-syn (list lit ...) end-flag)
     #:attr all-vars #'(xs s))

    (pattern
     ((~literal upto) xs:id e:expr)
     #:attr pattern #`(do [s <- (! lit-pat e)] (! upto-syn s))
     #:attr all-vars #'(xs))

    (pattern
     (k:keyword p:pat)
     #:attr pattern #`(do [pp <- p.pattern] (! kw-syn (quote k) pp))
     #:attr all-vars #`#,(syntax-e #`p.all-vars))
    (pattern
     ((~literal rest) xs:id)
     #:attr pattern #`(ret 'rest)
     #:attr all-vars #'(xs))
    (pattern
     ((~literal cons) car:pat cdr:pat)
     #:attr pattern #`(do [cp  <- car.pattern]
                          [cdp <- cdr.pattern]
                          (! cons-pat cp cdp))
     #:attr all-vars #`#,(append (syntax-e #`car.all-vars)
                                 (syntax-e #`cdr.all-vars)))
    (pattern
     ((~literal list) p:pat ...)
     #:with (v ...) (generate-temporaries #'(p ...))
     #:attr pattern #`(do [v <- p.pattern] ...
                          (! list-pat (list v ...)))
     #:attr all-vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))

    (pattern
     ((~literal quote) e)
     #:attr pattern #`(! lit-pat (quote e))
     #:attr all-vars #'())

    (pattern
     ((~literal @) e:expr x:id)
     #:attr pattern #`(ret (list 'tagged e 'var))
     #:attr all-vars #'(x))

    ;; tagged destructure

    (pattern
     ((~literal %) e:expr x:id)
     #:attr pattern #`(! method-syn e 'var)
     #:attr all-vars #'(x))
    (pattern
     ((~literal %) e:expr (p:pat ...))
     #:with (v ...) (generate-temporaries #'(p ...))
     #:attr pattern #`(do [v <- p.pattern] ...
                          [pp <- (! list-pat (list v ...))]
                          (! method-syn e pp))
     #:attr all-vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...))))     )

    (pattern
     (~or e:boolean e:char e:number e:string)
     #:attr pattern #`(! lit-pat e)
     #:attr all-vars #'())

)
  (define-syntax-class copat
    #:attributes (patterns vars)
    (pattern
     (p:pat ...)
     #:with (v ...) (generate-temporaries #'(p ...))
     #:attr patterns #`(do [v <- p.pattern] ...
                           (ret (list v ...)))
     #:attr vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))
    (pattern
     (p:pat ... #:bind)
     #:with (v ...) (generate-temporaries #'(p ...))
     #:attr patterns #`(do [v <- p.pattern] ...
                           (ret (list v ... end-copat)))
     #:attr vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))))

(define-syntax (copat syn)
  (syntax-parse syn
    [(_ [cop:copat e ...] ...)
     #:with (ps ...) (generate-temporaries #'(cop ...))
     ;; cop.patterns is a computation returning the pattern list; bind
     ;; each via `do` before assembling the outer list of (pat, kont) pairs.
     #`(do [ps <- cop.patterns] ...
           (! try-copatterns-default-error
              (list (list ps (thunk (λ cop.vars (do e ...)))) ...)))]))
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

;; Single-pass upto scan: match either 'o (compose) or '$ / end-of-stack
;; (done). `s` binds to the found sigil (or the keyword #:bind for end).
(define-rec-thunk (! <<v-impl k)
  (copat
   [(f (upto xs #:sigil s 'o '$ #:bind))
    (cond
      [(! equal? s 'o)
       (let ([k (thunk (λ (y)
                         (do [z <- (! apply f xs y)]
                             (! k z))))])
         (! <<v-impl k))]
      [#:else
       (do [z <- (! apply f xs)]
           (! k z))])]))

(define-thunk (! <<v) (! <<v-impl Ret))

(define-rec-thunk (! <<n-impl)
  (copat
   [(k f (upto xs #:sigil s 'o '$ #:bind))
    (cond
      [(! equal? s 'o)
       (let ([k (thunk (copat [(y) (! k (thunk (! apply f xs y)))]))])
         (! <<n-impl k))]
      [#:else
       (! k (thunk (! apply f xs)))])]))
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
         [xs <- (! <<v filter p 'o cdr xs '$)]
       (ifc (! p x)
            (ret (cons x xs))
            (ret xs)))]))

(def/copat (! debug)
  [(x #:bind) (! displayln x) (ret x)]
  [(x) (! displayln x) (! debug)])

(def/copat (! oo)
  [(f (upto xs '@)) [g <- (! apply f xs)] (! oo g)]
  [(f) (! f)])
(def-thunk (! @> x f) (! f x))
(def-thunk (! @>> xs f) (! apply f xs))
(def-thunk (! foldl^ step acc l) (! foldl l step acc))
(def-thunk (! foldr l step acc) (! <<v foldl^ (~ (! swap step)) acc 'o reverse l))
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
  [last-ix <- (! <<v swap - 1 'o vector-length v)]
  (! apply/vector-loop k v last-ix)
  )

(def-thunk (! list<-vector)
  (! apply/vector List))

(define! chest (! new-method 'chest 1))
(define! unit (! new-method 'unit 0))
(define! duo (! new-method 'duo 2))

((copat-method [(% (unit x)) (ret x)] [() (ret 3)]) % unit)

((copat [((% unit _)) (ret 3)] [() (ret 4)]) % unit)
(def/copat (! ununit x) [((% unit ())) (ret x)])


;; Nominal combinators

(define! v> (! new-method 'cbv-compose 1))
(define! v$ (! new-method 'cbv-end 0))
(def-thunk (! CBV> t u)
  [x <- (! t)]
  (! u x))
(def/copat (! CBV t)
  [((% v> (u)))(! CBV (~ (! CBV> t u)))]
  [((% v$ _))   (! t)]
  [() (! error "CBV composition: expected either another thunk or an end of args method, but got:")])

(define! n> (! new-method 'cbn-compose 1))
(define! n$ (! new-method 'cbn-end 0))
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
