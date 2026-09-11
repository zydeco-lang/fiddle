#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require "../../Parse.rkt")
(require "../../Stream.rkt")
(require (except-in fiddle/stdlib/CoList take))
(require fiddle/stdlib/Table)
(provide main-a main-b)

(define-thunk (! list-ref l n)
  (! stream-ref (thunk (! stream<-list l)) n))

(define-thunk (! monoid-cl-foldl)
  (copat
   [(m)
    (do [* <- (! first m)]
        [e <- (! second m)]
      (! cl-foldl^ * e))]))

;; A Monoid for a type A is a (list (A -> A -> F A) A)

(define-thunk (! parse-entry)
  (copat
   [(#\[ y y y y #\-
     mth1 mth2 #\-
     d1 d2 #\space
     h1 h2 #\:
     min1 min2 #\] #\space
     )
    (do [month <- (! parse-num (list mth1 mth2))]
        [day   <- (! parse-num (list d1 d2))]
      [hr <- (! parse-num (list h1 h2))]
      [min <- (! parse-num (list min1 min2))]
      [datetime <- (ret (list month day hr min))]
      (copat
       [(#\G u a r d spc #\# (upto digs #\space))
        (do [id <- (! parse-num digs)]
            (! abort (list 'datetime datetime id)))]
       [(#\f) (! abort (list 'datetime datetime 'falls-asleep))]
       [(#\w) (! abort (list 'datetime datetime 'wakes-up))]
       ))]))

(define entry-id third)
(define-thunk  (! guard?)
  (copat [(e) (! <<v number? % vo third e % v$)]))

(define-thunk (! rec<)
  (copat
   [(r1 r2)
    (do [dt1 <- (! second r1)] [dt2 <- (! second r2)]
      [m1 <- (! first dt1)] [m2 <- (! first dt2)]
      [d1 <- (! second dt1)] [d2 <- (! second dt2)]
      [h1 <- (! third dt1)] [h2 <- (! third dt2)]
      [min1 <- (! fourth dt1)] [min2 <- (! fourth dt2)]
      (cond
        [(! equal? m1 m2)
         (cond [(! equal? d1 d2)
                (cond [(! equal? h1 h2) (! < min1 min2)]
                      [#:else (! < h1 h2)])]
               [#:else (! < d1 d2)])]
        [#:else (! < m1 m2)]))]))

;; (! <<v apply parse-entry 'o string->list "[1518-09-11 00:02] Guard #863 begins shift" '$)
;; (! <<v apply parse-entry 'o string->list "[1518-09-11 23:59] Guard #863 begins shift" '$)
;; (! <<v apply parse-entry 'o string->list "[1518-11-23 00:14] falls asleep" '$)
;; (! <<v apply parse-entry 'o string->list "[1518-04-29 00:43] wakes up" '$)

(define-thunk (! group-entries)
  (letrec
      ([guards
        (thunk
         (copat
          [(acc es)
           (cond
             [(! empty? es) (ret acc)]
             [#:else
              (do [id <- (! <<v entry-id % vo car es % v$)]
                  (! <<v naps acc (list id) % vo cdr es % v$))])]))]
       [naps
        (thunk
         (copat
          [(acc g es)
           (cond [(! empty? es)
                  (do [g <- (! reverse g)] (ret (cons g acc)))]
                 [#:else
                  (do [hd <- (! car es)]
                      (cond [(! guard? hd)
                             (do [g <- (! reverse g)]
                                 (! guards (cons g acc) es))]
                            [#:else
                             (do [awakens <- (! second es)]
                                 [tl <- (! <<v cdr % vo cdr es % v$)]
                               (! naps acc (cons (list hd awakens) g) tl))]))])]))])
    (! guards '())))

(define-thunk (! fudge-nap asleep awake)
  (let ([get-minute (thunk (λ (x) (! <<v fourth % vo second x % v$)))])
    (! map get-minute (list asleep awake))))

;; get shit into the right format
(define-thunk (! fudge e)
  (do [id <- (! first e)]
      [fudged <- (! <<v map (thunk (! apply fudge-nap)) % vo cdr e % v$)]
    (ret (list id fudged))))

; U (CoList '(,Num ((,Num ,Num) ...))) -> Hash Num `((,Num ,Num) ...)
(define-thunk (! mk-entry-tbl)
  (copat
   [(es)
    (! cl-foldl
       es
       (thunk (λ (id->naps e)
                (do [key <- (! car e)]
                    [val <- (! second e)]
                  (! update id->naps key val (thunk (! append val))))))
       empty-table)]))

(define-thunk (! sleepier e1 e2)
  (do [t1 <- (! second e1)] [t2 <- (! second e2)]
    (cond [(! <= t1 t2) (ret e2)] [#:else (ret e1)])))

(define greatest-sleep (list -1 -inf.0))

(define-thunk (! total-sleep e)
  (do [hd <- (! first e)]
      [sum <- (! <<v apply + % vo map (thunk (! apply (thunk (! swap -)))) % vo rest e % v$)]
    (ret (list hd sum))))

(define-thunk (! inc-times)
  (copat
   [(time-array interval)
    (do [lo <- (! first interval)]
        [hi <- (! second interval)]
      (! <<n
         cl-foreach (thunk (λ (ix)
                             (do [x <- (! vector-ref time-array ix)]
                                 (! <<v vector-set! time-array ix % vo + 1 x % v$)))) % no
         range lo hi % n$))]))

(define-thunk (! debug-id x)
  (do [_ <- (! displayln x)]
      (ret x)))

(define-thunk (! best-minute naps)
  (do [times <- (! make-vector 60)]
      [_ <-
         (! <<n cl-foreach (thunk (! inc-times times)) % no colist<-list naps % n$)]
    [bst*time <- (! <<n
                    minimum-by (thunk (λ (x) (! <<v - % vo second x % v$))) greatest-sleep % no
                    cl-map (thunk (λ (i) (! <<v List i % vo vector-ref times i % v$)))% no
                    range 0 60 % n$)]
    (ret bst*time)))

(define-thunk (! main-a)
  (do [l <- (! slurp-lines!)]
      [l <- (! <<v map (thunk (! apply parse-entry)) % vo map string->list l % v$)]
    [es <- (! <<v group-entries % vo sort l rec< % v$)]
    [id->naps
     <- (! <<n mk-entry-tbl % no
           cl-map fudge % no colist<-list es % n$)]
    ;; (ret id->naps)
    [id*naps <- (ret (thunk (! <<v colist<-list % vo id->naps 'to-list % v$)))]
    [q <- (! <<n list<-colist % no cl-map total-sleep id*naps % n$)]
    [big-sleeper*sleep <- (! <<n
                             cl-foldl^ sleepier greatest-sleep % no
                             cl-map total-sleep id*naps % n$
                             )]
    [big-sleeper <- (! first big-sleeper*sleep)]
    [naps  <- (! id->naps 'get big-sleeper #f)]
    [best <- (! <<v first % vo best-minute naps % v$)]
    [chksum <- (! * best big-sleeper)]
    (! displayall (list 'part 'a ': 'id big-sleeper '* 'minute best '= chksum))))

(define-thunk (! main-b)
  (do [l <- (! slurp-lines!)]
      [l <- (! <<v map (thunk (! apply parse-entry)) % vo map string->list l % v$)]
    [es <- (! <<v group-entries % vo sort l rec< % v$)]
    [id->naps
     <- (! <<n mk-entry-tbl % no
           cl-map fudge % no colist<-list es % n$)]
    [id*naps <- (! id->naps 'to-list)]
    [id*best <- (! <<n
                   minimum-by (thunk (λ (x) (! <<v - % vo second % vo second x % v$))) (list -1 greatest-sleep) % no 
         cl-map (thunk (λ (x) (do [id <- (! first x)] [naps <- (! rest x)]
                                [best <- (! best-minute naps)]
                                (ret (list id best))))) % no
         colist<-list id*naps % n$)]
    [id <- (! first id*best)] [best <- (! second id*best)]
    [chksum <- (! <<v * id % vo first best % v$)]
    (! displayall (list 'part 'b ': 'id id '* 'minute best '= chksum))))


