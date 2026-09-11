#lang racket/base
;; Plain-Racket reference points for the Fiddle solution in solution.rkt
;; (part b, run with `racket racket-analogous.rkt`).
;;
;; Structure-for-structure port of solution.rkt:
;; lazy colist of turns, `operate` emitting a colist of score snapshots,
;; cl-last over it, a zipper circle as a message-dispatching closure with
;; list reversal on wraparound, scores as a closure over a mutable vector,
;; max via a fold with a monoid. Only the Fiddle stack machinery is gone.
(define NUM-PLAYERS 470)
(define NUM-MARBLES-B 7217000)

;; CoList A = (-> (or '(nil) (list 'cons A (CoList A))))
(define clv-nil '(nil))
(define (clv-nil? v) (equal? 'nil (car v)))
(define (clv-hd v) (cadr v))
(define (clv-tl v) (caddr v))
(define (cl-cons hd tl) (list 'cons hd tl))
(define (range-lo-hi lo hi)
  (λ () (if (<= hi lo) clv-nil (cl-cons lo (range-lo-hi (+ lo 1) hi)))))
(define (cl-last-loop x c)
  (let ([v (c)]) (if (clv-nil? v) x (cl-last-loop (clv-hd v) (clv-tl v)))))
(define (cl-last c)
  (let ([v (c)]) (if (clv-nil? v) (error "empty") (cl-last-loop (clv-hd v) (clv-tl v)))))
(define (cl-foldl l step acc)
  (let ([v (l)]) (if (clv-nil? v) acc (cl-foldl (clv-tl v) step (step acc (clv-hd v))))))
(define (cl-map f l)
  (λ () (let ([v (l)]) (if (clv-nil? v) clv-nil (cl-cons (f (clv-hd v)) (cl-map f (clv-tl v)))))))

(define (move-right l r n)
  (cond [(null? r) (move-right '() (reverse l) n)]
        [else (let ([x (car r)] [r (cdr r)])
                (if (zero? n) (list l x r) (move-right (cons x l) r (- n 1))))]))
(define (simple-circle l x r)
  (λ (msg . args)
    (case msg
      [(debug) (list l x r)]
      [(current-value) x]
      [(remove-current-value) (apply simple-circle (move-right l r 0))]
      [(add) (simple-circle l (car args) (cons x r))]
      [(move) (let ([n (car args)])
                (apply simple-circle
                       (if (< n 0)
                           (reverse (move-right r (cons x l) (- n)))
                           (move-right l (cons x r) n))))])))
(define (mutable-flexvec v)
  (λ (msg . args)
    (case msg
      [(update) (let ([ix (car args)] [up (cadr args)])
                  (vector-set! v ix (up (vector-ref v ix)))
                  (mutable-flexvec v))]
      [(to-colist) (cl-map (λ (i) (vector-ref v i)) (range-lo-hi 0 (vector-length v)))])))

(define (next-player p) (modulo (+ 1 p) NUM-PLAYERS))
(define (operate circle scores cur-player turns)
  (let ([v (turns)])
    (if (clv-nil? v)
        clv-nil
        (let ([marble (clv-hd v)] [turns (clv-tl v)])
          (if (equal? 0 (modulo marble 23))
              (let* ([circle (circle 'move -7)]
                     [marble2 (circle 'current-value)]
                     [circle (circle 'remove-current-value)]
                     [scores (scores 'update cur-player (λ (old) (+ marble marble2 old)))])
                (cl-cons scores (λ () (operate circle scores (next-player cur-player) turns))))
              (let* ([circle (circle 'move 2)]
                     [circle (circle 'add marble)])
                (operate circle scores (next-player cur-player) turns)))))))
(define (main n)
  (define circle (simple-circle '() 0 '()))
  (define scores (mutable-flexvec (make-vector NUM-PLAYERS 0)))
  (define final (cl-last (λ () (operate circle scores 0 (range-lo-hi 1 n)))))
  (cl-foldl (final 'to-colist) (λ (x y) (if (>= x y) x y)) -inf.0))
(displayln (main NUM-MARBLES-B))
