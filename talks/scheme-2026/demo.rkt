#lang fiddle

(require (except-in
          fiddle/prelude
          <<v
          Ret
          <<n))

;; There are two syntactic categories in Fiddle:
;; - values which are inert data and need no evaluation
;; - computations which are evaluated with access to the stack

;; Variables in Fiddle are always bound to values.
;; We have access to Racket's primitive data types.

(define num 0)
(define symbol 'symbol)
(define string "string")
(define nil '())
(define l (cons num (cons symbol '())))

;; In CBPV terminology, procedures are called *thunks*
;; To call a procedure you *force* the thunk.

;; Force is very basic so we write it simply as a bang (! t)

;; ((! displayln) "Hello, Scheme 2026!")

;; To create a thunk, we wrap a computation expression with a ~

(define hello
  (~ ((! displayln) "Hello, Scheme 2026!")))

;; Since it is so common to define thunks, we have a macro to give us a bit more of a schemey syntax
(def-thunk (! hello2)
  ((! displayln) "Hello, Scheme 2026!"))

;; Arguments are passed to thunks by pushing to the stack.
;; The core primitive for this is (^ c v)

(def-thunk (! hello3)
  (^ (! displayln) "Push me pop me"))

;; This all gets a bit loud so Fiddle supports more standard Scheme-like function call syntax which de-sugars to sequences of pushes.

;; The following desugars to the exact same computation as hello3:
(def-thunk (! hello-idiomatic)
  (! displayln "Push me pop me"))


;; This gives Fiddle a bit of a CBN feeling, where functions are "curried"
;; (! +)
;; (! + 2)
;; (! + 2 3)
;; (((! +) 2) 3)
;; (! + 2 3 4 5)


;; To sequence computations and get their results, we use bind, and to produce a result, we use ret
(def-thunk (! bind-example)
  (bind [m (! + 1 2)]
  (bind [n (! * 2 m)]
  (bind [_ (! displayln n)]
  (ret m)))))

;; A do macro makes this a lot easier
(def-thunk (! bind-example2)
  (do [m <- (! + 1 2)]
      [n <- (! * 2 m)]
      (! displayln n)
    (ret m)))

;; How do we define thunks that take arguments?
;; The primitive in Fiddle is a minimalistic version of case-λ

;; case-λ pattern matches on the _stack_ rather than a value
;; this is sometimes called "copattern matching"
(def-thunk (! case-λ-example)
  (case-λ
   ;; If there are *no arguments left* on the stack, then there is a continuation waiting for us to return
   [(#:bind) (ret 0)]
   ;; If there is *at least* one arg on the stack, pop it off and proceed
   [(x)      (ret 1)]
   ))

;; What happens when you run the following:
;; (! case-λ-example)
;; (! case-λ-example 1)
;; (! case-λ-example 1 2)

;; In Fiddle, the stack is dynamically typed, just like the values!

;; So we can get dynamic type errors if we try to interact with the stack in a way that it doesn't support

;; λ is defined as syntax sugar for failing when there is no argument.

;; What do you think the following does:
;; > (λ (x) (ret x))
;;

;; With this as the primitive we define a copattern matching macro

(def/copat (! copat-example x)
  [(y (cons z zs) #:bind) (! + x y z)]
  [((rest xs)) (ret xs)]
  )

;; Code Examples to show off what it's like

;; Supports CBN programming style very smoothly:
(def-thunk (! Y-combinator f)
  (! (~ (λ (x) (! f (~! x x))))
     (~ (λ (x) (! f (~! x x))))))

;; Variable arity functions can be implemented by "stack walking"
(def/copat (! abort x)
  [(#:bind) (ret x)]
  [(y) (! abort x)])

;; Some kooky vararg functions for call-by-value and call-by-name
;; composition/sequencing

(def-thunk (! Ret x) (ret x))
(def/copat (! <<v-impl k)
  [(f (upto xs 'o))
   (! <<v-impl (~ (λ (y) (do [z <- (! apply f xs y)] (! k z)))))]
  [(f (upto xs '$))
   (do [z <- (! apply f xs)] (! k z))]
  [(f)
   (! dot-args (~ (λ (args) (! k (~! apply f args)))))])
(def-thunk (! <<v) (! <<v-impl Ret))

(def-thunk (! !! f) (! f))

;; Conor McBride's "idiom brackets" as a Fiddle function
(def/copat (! idiom^ f)
  [(th)
    (do [x <- (! th)]
        (! idiom^ (thunk (! f x))))]
  [(#:bind)
    (! f)])
(define-thunk (! idiom) (! idiom^ $))

(def/copat (! foldl step acc)
  [((= '()) #:bind) (ret acc)]
  [((cons x xs))
   (do [acc <- (! step acc x)]
       (! foldl step acc xs))])

;; CBN style currying just works
(def-thunk (! foldr step acc)
  (! <<v foldl^ (~ (! swap step)) acc 'o reverse))

;; Can use copattern matching to implement OO-style "methods"
(def-thunk (! table<-hash h)
  (copat
   [((= 'has-key?) k #:bind) (! hash-has-key? h k)]
   [((= 'empty?) #:bind) (! hash-empty? h)]
   [((= 'set) k v #:bind)
    (do [h <- (! hash-set h k v)]
        (ret (thunk (! table<-hash h))))]
   [((= 'get) k v #:bind)
    (ifc (! hash-has-key? h k)
         (! hash-ref h k)
         (ret v))]
   [((= 'remove) k #:bind)
    [h <- (! hash-remove h k)] (ret (~ (! table<-hash h)))]
   [((= 'to-list) #:bind) (! hash->list h)]
   [((= 'to-hash) #:bind) (ret h)]))
