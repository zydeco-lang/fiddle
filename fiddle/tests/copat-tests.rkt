#lang fiddle

;; Semantic tests for copattern matching. Run with `raco test` — the module
;; body executes and `test-equal!` raises 'test-fail on the first mismatch,
;; so a nonzero exit means failure. One top-level expression per test so
;; every test runs even if an earlier one fails to typecheck cleanly.
;;
;; These pin down the semantics of copattern matching (originally those of
;; the runtime interpreter, now the expansion-time compiler) and of the
;; segment-structured stack with delimiters.

(require fiddle/prelude)

(define! m  (! new-method 'm))
(define! m2 (! new-method 'm2))

;; ---------------------------------------------------------------------
;; Basics (revived from the old #; block in prelude.rkt)

(! test-equal! (~ ((copat [() (ret 0)]))) (~ (ret 0)))
(! test-equal! (~ ((copat [((= 3)) (ret 0)] [() (! abort #f)]) 3)) (~ (ret 0)))
(! test-equal! (~ ((copat [((= 3)) (ret 0)] [() (! abort #f)]))) (~ (ret #f)))
(! test-equal! (~ ((copat [((= 3)) (ret 0)] [() (! abort #f)]) 4)) (~ (ret #f)))

;; upto
(! test-equal! (~ ((copat [((upto xs 3)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
   (~ (ret '(0 1 2))))
(! test-equal! (~ ((copat [((= 0) (upto xs 3)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
   (~ (ret '(1 2))))
(! test-equal! (~ ((copat [((= 0) (upto xs 3)) (ret xs)] [() (! abort #f)]) 0 1 2))
   (~ (ret #f)))

;; rest
(! test-equal! (~ ((copat [((rest xs)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
   (~ (ret '(0 1 2 3))))
(! test-equal! (~ ((copat [((= 0) (rest xs)) (ret xs)] [() (! abort #f)]) 0 1 2 3))
   (~ (ret '(1 2 3))))

;; cons
(! test-equal! (~ ((copat [((cons x y)) (ret (cons x y))] [() (! abort #f)]) (list 0)))
   (~ (ret (list 0))))
(! test-equal! (~ ((copat [(x (cons y z)) (ret (cons x (cons y z)))] [() (! abort #f)])
                   0 (list 1)))
   (~ (ret (list 0 1))))
(! test-equal! (~ ((copat [((cons (cons a b) c) (cons x (cons y z)))
                           (ret (list a b c x y z))]
                          [() (! abort #f)])
                   (cons (cons 0 1) 2) (cons 3 (cons 4 5))))
   (~ (ret (list 0 1 2 3 4 5))))

;; ---------------------------------------------------------------------
;; Backtracking: a failed clause must restore everything it consumed

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
                    [((cons (cons a b) c) (cons x (cons (= 'backtrack) z)))
                     (ret (list a b c x 'bktrk z))]
                    [((rest args)) (ret args)]
                    [() (! abort #f)])
                   (cons (cons 0 1) 2) (cons 3 (cons 4 5))))
   (~ (ret (list (cons (cons 0 1) 2) (cons 3 (cons 4 5))))))

;; #:bind
(! test-equal! (~ ((copat [(#:bind) (ret #t)] [() (! abort #f)]))) (~ (ret #t)))
(! test-equal! (~ ((copat [(#:bind) (ret #t)] [(x) (ret x)]) #f)) (~ (ret #f)))
;; #:bind un-consume: first clause pops x then fails on #:bind; second sees both
(! test-equal! (~ ((copat [(x #:bind) (ret 'one)] [(x y #:bind) (ret 'two)]) 1 2))
   (~ (ret 'two)))

;; ---------------------------------------------------------------------
;; New tests

;; 1. any-stack clause leaves the stack for the body
(! test-equal! (~ ((copat [() (! List)]) 1 2)) (~ (ret (list 1 2))))

;; 2. self-quoting / quoted literals, including '()
(! test-equal! (~ ((copat [('sym #\c '()) (ret 'ok)] [((rest r)) (ret r)]) 'sym #\c '()))
   (~ (ret 'ok)))
(! test-equal! (~ ((copat [('sym #\c '()) (ret 'ok)] [((rest r)) (ret r)]) 'sym #\d '()))
   (~ (ret (list 'sym #\d '()))))

;; 3. (= v) with a variable
(! test-equal! (~ (let ([v 'k]) ((copat [((= v)) (ret 1)] [(x) (ret x)]) 'k)))
   (~ (ret 1)))
(! test-equal! (~ (let ([v 'k]) ((copat [((= v)) (ret 1)] [(x) (ret x)]) 'j)))
   (~ (ret 'j)))

;; 4. list: exact length, wrong length, empty
(! test-equal! (~ ((copat [((list a b)) (ret (list b a))] [((rest r)) (ret r)]) (list 1 2)))
   (~ (ret (list 2 1))))
(! test-equal! (~ ((copat [((list a b)) (ret (list b a))] [((rest r)) (ret r)]) (list 1 2 3)))
   (~ (ret (list (list 1 2 3)))))
(! test-equal! (~ ((copat [((list a b)) (ret (list b a))] [((rest r)) (ret r)]) '()))
   (~ (ret (list '()))))

;; 5. nested list inside cons
(! test-equal! (~ ((copat [((cons (list a) (cons b c))) (ret (list a b c))]
                          [((rest r)) (ret r)])
                   (cons (list 1) (cons 2 3))))
   (~ (ret (list 1 2 3))))

;; 6. upto literal not found → fallback sees everything
(! test-equal! (~ ((copat [((upto xs 'end)) (ret xs)] [((rest r)) (ret r)]) 1 2))
   (~ (ret (list 1 2))))

;; 7. upto sigil restore on backtrack (fixed in up-to-lit)
(! test-equal! (~ ((copat [((upto xs 'end) (= 'no)) (ret 'bad)] [((rest r)) (ret r)])
                   1 2 'end 'yes))
   (~ (ret (list 1 2 'end 'yes))))

;; 8. multi-sigil upto, with and without #:bind
(! test-equal! (~ ((copat [((upto xs #:sigil s 'a 'b) (rest r)) (ret (list xs s r))])
                   1 2 'b 3))
   (~ (ret (list (list 1 2) 'b (list 3)))))
(! test-equal! (~ ((copat [((upto xs #:sigil s 'a #:bind)) (ret (list xs s))]) 1 2))
   (~ (ret (list (list 1 2) '#:bind))))
(! test-equal! (~ ((copat [((upto xs #:sigil s 'a)) (ret 'bad)] [((rest r)) (ret r)]) 1 2))
   (~ (ret (list 1 2))))

;; 9. two method frames in sequence; (rest r) stops at a method frame
(! test-equal! (~ ((copat [((% m (n)) (% m2 (a b))) (ret (list n a b))]) % m 1 % m2 2 3))
   (~ (ret (list 1 2 3))))
(! test-equal! (~ ((copat [((rest r)) ((copat [((% m (y))) (ret (list r y))]))]) 1 2 % m 3))
   (~ (ret (list (list 1 2) 3))))

;; 10. method-frame restore on backtrack; (% m x) binds the arg list
(! test-equal! (~ ((copat [((% m (y)) (= 'no)) (ret 'bad)]
                          [((% m (y)) (rest r)) (ret (list y r))])
                   % m 3 'yes))
   (~ (ret (list 3 (list 'yes)))))
(! test-equal! (~ ((copat [((% m) (rest x)) (ret x)]) % m 3)) (~ (ret (list 3))))
;; method pattern fails on the args, falls through
(! test-equal! (~ ((copat [((% m ((= 'nope)))) (ret 'bad)] [((% m) (rest x)) (ret x)]) % m 3))
   (~ (ret (list 3))))

;; 11. upto (% m (y)) — grabs args up to the frame, matches the frame's args
(! test-equal! (~ ((copat [((upto xs (% m (y)))) (ret (list xs y))]) 1 2 % m 3))
   (~ (ret (list (list 1 2) 3))))

;; 12. upto (% m) leaves the frame in place for the remainder
(! test-equal! (~ ((copat [((upto xs (% m)) (% m (y))) (ret (list xs y))]) 1 2 % m 3))
   (~ (ret (list (list 1 2) 3))))

;; 14. duplicate _ pattern variables
(! test-equal! (~ ((copat [(_ _ x) (ret x)]) 1 2 3)) (~ (ret 3)))

;; 15. scoping: (= x) refers to the OUTER x, not the just-bound pattern var
(! test-equal! (~ (let ([x 'outer])
                    ((copat [(x (= x)) (ret 'same)] [((rest r)) (ret r)]) 'a 'outer)))
   (~ (ret 'same)))

;; pm / patc (value-pattern macros)
(! test-equal! (~ (pm (ret (cons 1 2)) [(cons a b) (ret (list a b))] [_ (ret 'no)]))
   (~ (ret (list 1 2))))
(! test-equal! (~ (pm (ret 'x) [(cons a b) (ret 'yes)] [_ (ret 'no)])) (~ (ret 'no)))
(! test-equal! (~ (patc (ret (list 1 2)) [(list a b) (ret (list b a))] [_ (ret 'no)]))
   (~ (ret (list 2 1))))

;; def/copat and def-thunk lower through copat
(def/copat (! twice)
  [((= 'a) x) (ret (list x x))]
  [(x) (ret x)])
(! test-equal! (~ (! twice 'a 1)) (~ (ret (list 1 1))))
(! test-equal! (~ (! twice 5)) (~ (ret 5)))
(def-thunk (! sum3 a b c #:bind) [s <- (! + a b)] (! + s c))
(! test-equal! (~ (! sum3 1 2 3)) (~ (ret 6)))

;; ---------------------------------------------------------------------
;; Segment-structured stack and delimiters

;; case-λ: the delimiter arm binds the delimiter itself (a ctype)
(! test-equal! (~ ((case-λ [(% d) (ret d)]) % m)) (~ (ret m)))
;; #:bind and "a delimiter on top" are distinct states
(! test-equal! (~ ((case-λ [(#:bind) (ret 'b)] [(% d) (ret 'd)]) % m)) (~ (ret 'd)))
(! test-equal! (~ ((case-λ [(#:bind) (ret 'b)] [(% d) (ret 'd)]))) (~ (ret 'b)))
;; all three arms, value on top
(! test-equal! (~ ((case-λ [(x) (ret x)] [(#:bind) (ret 'b)] [(% d) (ret 'd)]) 7)) (~ (ret 7)))

;; (upto xs (% m)) on an empty segment
(! test-equal! (~ ((copat [((upto xs (% m)) (% m) y) (ret (list xs y))]) % m 3))
   (~ (ret (list '() 3))))
;; wrong delimiter: falls through with the stack intact
(! test-equal! (~ ((copat [((upto xs (% m))) (ret 'bad)] [(r (% m2) z) (ret (list r z))]) 1 % m2 2))
   (~ (ret (list 1 2))))
;; nested delimiters, each exposing a one-element segment
(! test-equal! (~ ((copat [(a (% m) b (% m2) c #:bind) (ret (list a b c))]) 1 % m 2 % m2 3))
   (~ (ret (list 1 2 3))))
;; (rest xs) takes the open segment; the delimiter's own segment follows
(! test-equal! (~ ((copat [((rest xs)) ((copat [((% m) (rest ys)) (ret (list xs ys))]))]) 1 2 % m 3 4))
   (~ (ret (list (list 1 2) (list 3 4)))))
;; apply onto a NON-empty segment: pushed values go on top, source order
(! test-equal! (~ (! apply List (list 1 2) 3)) (~ (ret (list 1 2 3))))
;; prefix semantics: (% m (a)) binds a and leaves the rest of the segment
(! test-equal! (~ ((copat [((% m (a)) b #:bind) (ret (list a b))]) % m 1 2)) (~ (ret (list 1 2))))
;; backtracking that pops a delimiter, then fails on the exposed segment
(! test-equal! (~ ((copat [((% m) (= 'no)) (ret 'bad)] [((% m) x) (ret x)]) % m 3)) (~ (ret 3)))
;; a value on top of a delimiter: the value arm fires; the delimiter remains for the body
(! test-equal! (~ ((copat [(x) ((case-λ [(% d) (ret (list x d))]))]) 5 % m)) (~ (ret (list 5 m))))

;; ---------------------------------------------------------------------
;; Composition operators <<v / <<n over the delimiters vo/v$ and no/n$

;; compose-then-end: + 1 (* 2 3)
(! test-equal! (~ (! <<v + 1 % vo * 2 3 % v$)) (~ (ret 7)))
;; tail form: the chain ends at #:bind, the caller's args land in the last stage
(! test-equal! (~ (! (~ (! <<v + 1 % vo *)) 2 3)) (~ (ret 7)))
;; 0-stage: just a call
(! test-equal! (~ (! <<v equal? 'empty 'empty)) (~ (ret #t)))
;; a nested composition inside a stage: + 1 (* 2 (+ 1 2))
(! test-equal! (~ (! <<v + 1 % vo (~ (λ (x) (! <<v * 2 % vo + 1 x % v$))) 2 % v$)) (~ (ret 7)))
;; 11-stage chain, tail form
(! test-equal! (~ (! <<v + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 % vo + 1 0))
   (~ (ret 11)))
;; a foreign delimiter ends the chain: the final stage sees only its own
;; arguments, and the delimiter (with the segment beneath it) is left on
;; the stack for the continuation — for <<n that is the head thunk
(! test-equal! (~ (! <<n (~ (copat [(th (% m) c) (do [v <- (! th)] (ret (list v c)))]))
                     % no + 1 1 % m 3))
   (~ (ret (list 2 3))))
;; <<n: stages are passed as thunks, so an erroring stage that is never forced is harmless
(! test-equal! (~ (! <<n (~ (λ (th) (ret 'lazy))) % no error "never forced" % n$)) (~ (ret 'lazy)))
(! test-equal! (~ (! <<n (~ (λ (th) (! th))) % no + 1 2 % n$)) (~ (ret 3)))
;; <<n tail form
(! test-equal! (~ (! (~ (! <<n (~ (λ (th) (! th))) % no +)) 1 2)) (~ (ret 3)))

(! displayln 'copat-tests-all-pass)
