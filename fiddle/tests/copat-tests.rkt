#lang fiddle

;; Semantic tests for copattern matching. Run with `raco test` — the module
;; body executes and `test-equal!` raises 'test-fail on the first mismatch,
;; so a nonzero exit means failure. One top-level expression per test so
;; every test runs even if an earlier one fails to typecheck cleanly.
;;
;; These pin down the semantics of the runtime interpreter (copat-match) so
;; the expansion-time compiler can be checked against the same behavior.

(require fiddle/prelude)

(define! m  (! new-method 'm 1))
(define! m2 (! new-method 'm2 2))

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
(! test-equal! (~ ((copat [((% m x)) (ret x)]) % m 3)) (~ (ret (list 3))))
;; method pattern fails on the args, falls through
(! test-equal! (~ ((copat [((% m ((= 'nope)))) (ret 'bad)] [((% m x)) (ret x)]) % m 3))
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

(! displayln 'copat-tests-all-pass)
