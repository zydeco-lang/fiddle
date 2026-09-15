#lang slideshow

(require pict
         net/sendurl
         racket/draw
         racket/runtime-path)

(define michigan-blue (make-color 0 39 76))
(define michigan-maize (make-color 255 203 5))
(define ink (make-color 25 38 48))
(define muted (make-color 91 107 115))
(define rule-gray (make-color 210 220 226))
(define pale-blue (make-color 235 241 245))
(define pale-maize (make-color 255 246 210))
(define white (make-color 255 255 255))

(define-runtime-path block-m-path "assets/block-m.png")
(define-runtime-path gtt-title-path "assets/gradual-type-theory-title.png")
(define-runtime-path page-106-path "assets/page-106.png")
(define-runtime-path levy-cbpv-paper-path "assets/levy-cbpv-paper.png")
(define-runtime-path stephen-chang-path "assets/stephen-chang.jpg")
(define-runtime-path ben-greenman-path "assets/ben-greenman.jpg")
(define-runtime-path northeastern-monogram-path
  "assets/northeastern-monogram.png")
(define-runtime-path yuchen-jiang-path "assets/yuchen-jiang.png")
(define-runtime-path relative-monads-title-path
  "assets/relative-monads-title.png")

(define-runtime-path levy-cbpv-types-path "assets/levy-cbpv-types.png")
(define-runtime-path levy-functions-pop-path
  "assets/levy-functions-pop-detail.png")
(define-runtime-path levy-cbv-translation-path
  "assets/levy-cbv-translation-detail.png")

(define (txt content size [style 'default] [color ink])
  (colorize (text content style size) color))

(define block-m
  (scale-to-fit (bitmap block-m-path) 210 235))

(define logo-panel
  (cc-superimpose
   (colorize (filled-rectangle 280 330) michigan-blue)
   block-m))

(define gtt-title
  (scale-to-fit (bitmap gtt-title-path) 690 135))

(define page-106
  (scale-to-fit (bitmap page-106-path) 760 430))

(define levy-cbpv-paper
  (scale-to-fit (bitmap levy-cbpv-paper-path) 445 340))

(define stephen-chang
  (scale-to-fit
   (inset/clip (bitmap stephen-chang-path) 0 0 0 -86)
   220 220))

(define ben-greenman
  (scale-to-fit
   (inset/clip (bitmap ben-greenman-path) -222 0 0 0)
   220 220))

(define northeastern-monogram
  (scale-to-fit (bitmap northeastern-monogram-path) 94 84))

(define yuchen-jiang
  (scale-to-fit (bitmap yuchen-jiang-path) 150 150))

(define relative-monads-title
  (scale-to-fit (bitmap relative-monads-title-path) 635 145))

(define (history-person portrait name)
  (vc-append
   12
   portrait
   (txt name 26 '(bold . default) michigan-blue)))

(define levy-cbpv-types
  (scale-to-fit (bitmap levy-cbpv-types-path) 840 150))

(define levy-functions-pop
  (scale-to-fit (bitmap levy-functions-pop-path) 840 300))

(define levy-cbv-translation
  (scale-to-fit (bitmap levy-cbv-translation-path) 840 126))

(define (source-note content)
  (txt content 12 '(italic . default) muted))

(define (cbpv-component notation description)
  (hc-append
   18
   (lt-superimpose
    (blank 70 32)
    (txt notation 25 '(bold . modern) michigan-blue))
   (txt description 24 'default)))

(define history-width 690)

(define history-heading
  (vl-append
   9
   (txt "Some Historical Context"
        38 '(bold . default) michigan-blue)
   (colorize (filled-rectangle 876 6) michigan-maize)))

(define history-opening
  (lt-superimpose
   (blank history-width 40)
   (txt "The year was 2018..." 31 'default)))

(define history-paper
  (lt-superimpose
   (blank history-width 190)
   (vl-append
    20
    (txt "We had just written the paper that was the core of my thesis"
         27 'default)
    gtt-title)))

(define history-proofs
  (lt-superimpose
   (blank history-width 42)
   (hc-append
    (txt "A paper with a " 27 'default)
    (txt "very" 27 '(italic . default))
    (txt " long extended version full of proofs" 27 'default))))

(define (recap-row label left-form right-form explanation)
  (hc-append
   22
   (lc-superimpose
    (blank 190 82)
    (txt label 20 '(bold . default) michigan-blue))
   (cc-superimpose
    (blank 385 82)
    (hc-append
     14
     (txt left-form 25 'modern)
     (txt "⇄" 25 '(bold . default) michigan-maize)
     (txt right-form 25 'modern)))
   (rc-superimpose
    (blank 235 82)
    (txt explanation 20 'default muted))))

(define (implementation-row label focal explanation
                            [focal-style 'default])
  (hc-append
   28
   (lc-superimpose
    (blank 260 92)
    (txt label 20 '(bold . default) michigan-blue))
   (lc-superimpose
    (blank 588 92)
    (vl-append
     7
     (txt focal 26 focal-style)
     (txt explanation 26 focal-style)))))

(define (code-count-row label count)
  (hc-append
   0
   (lc-superimpose
    (blank 600 78)
    (txt label 24 '(bold . default) muted))
   (rc-superimpose
    (blank 200 78)
    (txt count 36 '(bold . default) michigan-blue))))

(define (duality-row value-feature stack-feature)
  (hc-append
   0
   (cc-superimpose
    (blank 399 68)
    (txt value-feature 27 '(bold . default)))
   (colorize (filled-rectangle 2 68) rule-gray)
   (cc-superimpose
    (blank 399 68)
    (txt stack-feature 27 '(bold . default) michigan-blue))))

(define (repository-row logo name description url)
  (define url-pict (txt url 21 'modern michigan-blue))
  (clickback
   (lc-superimpose
    (blank 840 150)
    (hc-append
     30
     (cc-superimpose
      (blank 105 105)
      (txt logo 62 'default))
     (vl-append
      7
      (txt name 32 '(bold . default) michigan-blue)
      (txt description 22 'default muted)
      (vl-append
       3
       url-pict
       (colorize
        (filled-rectangle (pict-width url-pict) 2)
        michigan-maize)))))
   (lambda () (send-url url))))

(define (concept-column label focal detail-1 detail-2)
  (lc-superimpose
   (blank 390 170)
   (vl-append
    14
    (txt label 20 '(bold . default) muted)
    (txt focal 35 '(bold . default) michigan-blue)
    (vl-append
     4
     (txt detail-1 21 'default)
     (txt detail-2 21 'default)))))

(define (visual-heading content)
  (vl-append
   9
   (txt content 38 '(bold . default) michigan-blue)
   (colorize (filled-rectangle 876 6) michigan-maize)))

(define (diagram-tile content width height
                      #:background [background pale-blue]
                      #:size [size 28]
                      #:style [style '(bold . default)]
                      #:color [color michigan-blue])
  (cc-superimpose
   (colorize (filled-rounded-rectangle width height 12) background)
   (txt content size style color)))

(define scheme-cbpv-fiddle
  (hc-append
   20
   (diagram-tile "Scheme" 235 118 #:size 34)
   (txt "+" 44 '(bold . default) michigan-maize)
   (diagram-tile "CBPV" 220 118 #:size 34)
   (txt "=" 44 '(bold . default) michigan-maize)
   (diagram-tile "Fiddle" 210 118
                 #:background pale-maize
                 #:size 34
                 #:color ink)))

(define (ingredient token label [token-style 'modern])
  (vc-append
   12
   (diagram-tile token 190 95
                 #:size 31
                 #:style token-style)
   (txt label 20 '(bold . default) muted)))

(define (structure-example code label)
  (vc-append
   12
   (diagram-tile code 285 100
                 #:background pale-maize
                 #:size 29
                 #:style 'modern
                 #:color ink)
   (txt label 20 '(bold . default) muted)))

(define (application-line content)
  (lc-superimpose
   (blank 280 58)
   (txt content 29 'modern)))

(define (scheme-form code explanation)
  (hc-append
   36
   (lc-superimpose
    (blank 505 52)
    (txt code 25 'modern))
   (lc-superimpose
    (blank 335 52)
    (txt explanation 20 'default muted))))

(define (analogy-cell content width height
                      [size 27]
                      [style 'default]
                      [color ink])
  (cc-superimpose
   (blank width height)
   (txt content size style color)))

(slide
 #:layout 'center
 (hc-append
  58
  (vl-append
   13
   (txt "A Call-by-push-value" 46 '(bold . default) michigan-blue)
   (hc-append
    16
    (txt "Scheme" 58 '(bold . default) michigan-blue)
    (txt "🎻" 42 'default))
   (colorize (filled-rectangle 520 8) michigan-maize)
   (blank 1 18)
   (txt "Max S. New" 29 '(bold . default))
   (txt "University of Michigan" 23 'default muted)
   (blank 1 8)
   (txt "Scheme 2026" 24 '(bold . default) michigan-blue))
  logo-panel))

(slide
 #:layout 'top
 #:gap-size 0
 history-heading
 (blank 1 130)
 history-opening
 'next
 (blank 1 66)
 history-paper
 'next
 (blank 1 66)
 history-proofs)

(slide
 #:layout 'top
 #:gap-size 0
 (vc-append
  18
  history-heading
  page-106))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Call-by-push-value and Me")
  (blank 1 40)
  (hc-append
   52
   levy-cbpv-paper
   (vl-append
    7
    (txt "In our paper, we heavily"
         28 'default)
    (txt "used the CBPV calculus."
         28 'default)
    (blank 1 40)
    (txt "It simplified the semantics"
         28 'default)
    (txt "and proofs."
         28 'default)
    (blank 1 40)
    (txt "But what would a programming"
         28 '(bold . default) michigan-blue)
    (txt "language directly based on"
         28 '(bold . default) michigan-blue)
    (txt "CBPV look like?"
         28 '(bold . default) michigan-blue)))))

(slide
 #:layout 'top
 #:gap-size 0
 (vc-append
  0
  (visual-heading "A language-design experiment")
  (blank 1 78)
  scheme-cbpv-fiddle
  (blank 1 42)
  (txt "What if Scheme were based on CBPV?"
       31 '(italic . default))))

(slide
 #:layout 'top
 #:gap-size 0
 (vc-append
  0
  (visual-heading "Minimalist Philosophy")
  (blank 1 88)
  (hc-append
   48
   (txt "Scheme" 50 '(bold . default) michigan-blue)
   (txt "🤝" 54 'default)
   (txt "CBPV" 50 '(bold . default) michigan-blue))
  (blank 1 92)
  (vc-append
   7
   (txt "Use composition to build complex features"
        31 'default)
   (txt "from a small core."
        31 '(bold . default) michigan-blue))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "CBPV separates values from computations")
  (blank 1 60)
  levy-cbpv-types
  (blank 1 58)
  (vl-append
   10
   (txt "Value types classify data."
        29 '(bold . default) michigan-blue)
   (txt "Computation types classify the stacks that programs run against."
        26 '(bold . default) michigan-blue))
  (blank 1 34)
  (source-note
   "Source: Paul Blain Levy, Adjunction Semantics for Call-By-Push-Value (2004), slide 13.")))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Application pushes; lambda pops")
  (blank 1 32)
  levy-functions-pop
  (blank 1 32)
  (source-note
   "Source: Paul Blain Levy, Adjunction Semantics for Call-By-Push-Value (2004), slide 27.")))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "CBV Function Decomposition")
  (blank 1 42)
  levy-cbv-translation
  (blank 1 44))
 'next
 (lc-superimpose
  (blank 876 126)
  (vl-append
   15
   (cbpv-component "U" "Closure: suspended code.")
   (cbpv-component "A →" "Argument: a value supplied on the stack.")
   (cbpv-component "F B" "Return continuation: what consumes the result.")))
 (blank 1 35)
 (lc-superimpose
  (blank 876 16)
  (source-note
   "Source: Paul Blain Levy, A Tutorial on Call-by-Push-Value (2016), slide 37.")))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Scheme Minimalism")
  (blank 1 70)
  (txt "A tiny set of primitive ingredients:" 28 'default muted)
  (blank 1 30)
  (cc-superimpose
   (blank 876 70)
   (hc-append
    26
    (txt "atoms" 34 'modern)
    (txt "+" 34 '(bold . default) michigan-maize)
    (txt "cons cells" 34 'modern)
    (txt "+" 34 '(bold . default) michigan-maize)
    (txt "closures" 34 'modern)))
  (blank 1 88)
  (vl-append
   6
   (txt "More complex data (lists, trees) built up"
        30 '(bold . default) michigan-blue)
   (txt "from just a single binary data constructor"
        30 '(bold . default) michigan-blue))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Scheme Minimalism?")
  (blank 1 38)
  (txt "Closures come with a richer argument protocol:"
       28 'default)
  (blank 1 30)
  (vl-append
   14
   (scheme-form "(lambda (x y) ...)"
                "fixed arity")
   (scheme-form "(lambda (x y . rest) ...)"
                "collect extras as a list")
   (scheme-form "(case-lambda [(x) ...] [(x y) ...])"
                "dispatch on arity")
   (scheme-form "(apply f args)"
                "supply arguments from a list"))
  (blank 1 35)
  (vl-append
   6
   (txt "Compound, arbitrary size constructs are baked"
        29 '(bold . default) michigan-blue)
   (txt "into the core of procedures."
        29 '(bold . default) michigan-blue))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Fiddle: A CBPV Scheme")
  (blank 1 48)
  (txt "Treat the stack the way Scheme treats data."
       29 'default)
  (blank 1 36)
  (cc-superimpose
   (blank 876 216)
   (vc-append
    0
    (hc-append
     0
     (analogy-cell "" 150 48)
     (analogy-cell "BASE CASE" 230 48
                   18 '(bold . default) muted)
     (analogy-cell "EXTENSION" 200 48
                   18 '(bold . default) muted)
     (analogy-cell "INSPECTION" 260 48
                   18 '(bold . default) muted))
    (colorize (filled-rectangle 840 2) rule-gray)
    (hc-append
     0
     (analogy-cell "VALUES" 150 82
                   20 '(bold . default) michigan-blue)
     (analogy-cell "atomic data" 230 82 27 'modern)
     (analogy-cell "cons" 200 82 27 'modern)
     (analogy-cell "car/cdr/cons?/nil?" 260 82 20 'modern))
    (colorize (filled-rectangle 840 1) rule-gray)
    (hc-append
     0
     (analogy-cell "STACKS" 150 82
                   20 '(bold . default) michigan-blue)
     (analogy-cell "continuations" 230 82
                   27 'modern michigan-blue)
     (analogy-cell "argument" 200 82
                   27 'modern michigan-blue)
     (analogy-cell "case-lambda" 260 82
                   24 'modern michigan-blue))))
  (blank 1 52)
  (cc-superimpose
   (blank 876 50)
   (txt "pushing an argument is the cons of the stack."
        36 '(bold . default) michigan-blue))))

(slide
 #:layout 'center
 (inset
  (cc-superimpose
  (colorize (filled-rectangle 1024 768) michigan-blue)
  (vc-append
   20
   (txt "LIVE DEMO" 52 '(bold . default) white)
    (colorize (filled-rectangle 430 6) michigan-maize)
    (blank 1 22)
    (txt "#lang fiddle" 31 'modern white)
    (txt "🎻" 48 'default white)))
  (- margin)))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Minimalistic Untyped CBPV Core Language")
  (blank 1 38)
  (recap-row "CODE" "(~ e)" "(! v)" "thunk / force")
  (colorize (filled-rectangle 854 1) rule-gray)
  (recap-row "ARGUMENT" "(^ e v)" "(λ (x) e)" "push / pop")
  (colorize (filled-rectangle 854 1) rule-gray)
  (recap-row "RESULT" "(ret v)" "(bind [x e1] e2)"
             "return / bind")
  (blank 1 35)
  (vl-append
   6
   (txt "case-λ: minimalistic pattern match primitive for the stack "
        28 '(bold . default) michigan-blue))))

(slide
 #:layout 'top
 #:gap-size 0
 (lt-superimpose
  (blank 876 client-h)
  (vl-append
   0
   (visual-heading "Fiddle Implementation")
   (blank 1 45)
   (implementation-row "TURNSTILE"
                       "Implemented using the Turnstile library"
                       "checks values vs. computations"
                       '(bold . default))
   (colorize (filled-rectangle 856 1) rule-gray)
   (implementation-row "VIRTUALIZED STACK"
                       "global mutable list"
                       "threaded through the Racket program"
                       '(bold . default))
   (colorize (filled-rectangle 856 1) rule-gray)
   (implementation-row "INTEROP"
                       "Racket  ⇄  Fiddle"
                       "wrappers to mediate calling conventions"
                       '(bold . default)))
  (lb-superimpose
   (blank 876 client-h)
   (hc-append
    18
    (scale-to-fit stephen-chang 68 68)
    (vl-append
     5
     (txt "Thanks to my labmate Stephen Chang" 23 '(bold . default) michigan-blue)
     (txt "for helping me to use Turnstile." 20 '(italic . default) muted))))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Fiddle Implementation and Libraries")
  (blank 1 45)
  (code-count-row "CORE LANGUAGE" "457")
  (colorize (filled-rectangle 800 1) rule-gray)
  (code-count-row "RUNTIME + PRELUDE" "1,171")
  (colorize (filled-rectangle 800 1) rule-gray)
  (code-count-row "STANDARD LIBRARY" "955")
  (blank 1 18)
  (colorize (filled-rectangle 800 3) michigan-maize)
  (code-count-row "ADVENT OF CODE, 2018–19 · 29 DAYS" "≈4,100")
  (blank 1 18)
  (vl-append
   4
   (txt "Advent of Code solutions were quite slow compared to"
        20 '(bold . default) michigan-blue)
   (txt "direct Racket implementations."
        20 '(bold . default) michigan-blue))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Value constructions have stack duals")
  (blank 1 58)
  (hc-append
   0
   (cc-superimpose
    (blank 399 44)
    (txt "VALUE" 20 '(bold . default) muted))
   (colorize (filled-rectangle 2 44) rule-gray)
   (cc-superimpose
    (blank 399 44)
    (txt "STACK" 20 '(bold . default) muted)))
  (colorize (filled-rectangle 800 1) rule-gray)
  (duality-row "cons" "argument")
  (colorize (filled-rectangle 800 1) rule-gray)
  (duality-row "patterns" "copatterns"))
 'alts
 (list
  (list (blank 876 138))
  (list
   (lt-superimpose
    (blank 876 138)
    (vl-append
     0
     (colorize (filled-rectangle 800 1) rule-gray)
     (duality-row "atomic values" "atomic stacks?")
     (blank 800 69))))
  (list
   (lt-superimpose
    (blank 876 138)
    (vl-append
     0
     (colorize (filled-rectangle 800 1) rule-gray)
     (duality-row "atomic values" "atomic stacks?")
     (colorize (filled-rectangle 800 1) rule-gray)
     (duality-row "structs" "methods"))))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Nominal Stack Constructors: Methods")
  (blank 1 55)
  (hc-append
   40
   (concept-column "STRUCTS / DATA"
                   "named fields"
                   "opaque structs provide"
                   "data abstraction")
   (colorize (filled-rectangle 2 170) rule-gray)
   (concept-column "METHODS / STACK"
                   "named stack slots"
                   "opaque methods provide"
                   "stack abstraction?"))
  (blank 1 45)
  (vl-append
   6
   (txt "Can mechanisms like these help encode our sophisticated"
        25 '(bold . default) michigan-blue)
   (txt "Scheme marks and delimiters smoothly?"
        25 '(bold . default) michigan-blue))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Fiddle's Successor: Zydeco")
  (blank 1 35)
  (hc-append
   28
   (cc-superimpose
    (blank 245 170)
    (vl-append
     15
     (txt "FIDDLE 🎻" 22 '(bold . default) muted)
     (txt "untyped prototype" 29 '(bold . default) michigan-blue)
     (txt "Racket" 20 'default muted)))
   (txt "→" 42 '(bold . default) michigan-maize)
   (cc-superimpose
    (blank 315 170)
    (vl-append
     13
     (txt "ZYDECO 🪗" 22 '(bold . default) muted)
     (txt "typed CBPV" 32 '(bold . default) michigan-blue)
     (txt "System Fω polymorphism" 22 'default)))
   (vc-append
    7
    yuchen-jiang
    (txt "Yuchen Jiang" 18 '(bold . default) muted)))
  (txt "Built with Yuchen Jiang and Michigan undergraduates."
       21 'default muted)
  (blank 1 14)
  (colorize (filled-rectangle 840 1) rule-gray)
  (hc-append
   28
   (lc-superimpose
    (blank 165 145)
    (txt "THEORY" 20 '(bold . default) muted))
   (lc-superimpose
    (blank 650 145)
    relative-monads-title))
  (colorize (filled-rectangle 840 1) rule-gray)
  (hc-append
   28
   (lc-superimpose
    (blank 165 82)
    (txt "IMPLEMENTATION" 20 '(bold . default) muted))
   (lc-superimpose
    (blank 650 82)
    (vl-append
     6
     (txt "efficient CBPV intermediate representations"
          25 '(bold . default) michigan-blue)
     (txt "ongoing work" 19 'default muted))))))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (visual-heading "Takeaways from Fiddle")
  (blank 1 24)
  (hc-append
   30
   (lc-superimpose
    (blank 125 116)
    (txt "RACKET" 20 '(bold . default) muted))
   (lc-superimpose
    (blank 700 116)
    (vl-append
     10
     (txt "Racket and Turnstile made prototyping fast."
          32 '(bold . default) michigan-blue)
     (txt "Starting untyped made it easy to play with CBPV code."
          22 'default muted))))
  (colorize (filled-rectangle 855 1) rule-gray)
  (hc-append
   30
   (lc-superimpose
    (blank 125 116)
    (txt "CBPV" 20 '(bold . default) muted))
   (lc-superimpose
    (blank 700 116)
    (vl-append
     10
     (vl-append
      5
      (txt "CBPV brings stack manipulation"
           31 '(bold . default) michigan-blue)
      (txt "into a functional language."
           31 '(bold . default) michigan-blue))
     (txt "Methods and copatterns came from treating it like data."
          22 'default muted))))
  (colorize (filled-rectangle 855 1) rule-gray)
  (hc-append
   30
   (lc-superimpose
    (blank 125 116)
    (txt "SELF CARE" 20 '(bold . default) muted))
   (lc-superimpose
    (blank 700 116)
    (vl-append
     8
     (txt "Successful Procrastiworking"
          30 '(bold . default) michigan-blue)
     (vl-append
      3
      (txt "It helped me not burn out and play with side research ideas"
           20 'default muted)
      (txt "that I picked up again years later."
           20 'default muted)))))
  ))

(slide
 #:layout 'top
 #:gap-size 0
 (vl-append
  0
  (txt "Conclusion"
       38 '(bold . default) michigan-blue)
  (blank 1 9)
  (colorize (filled-rectangle 438 6) michigan-maize)
  (blank 1 24)
  (cc-superimpose
   (blank 840 100)
   (scale-to-fit scheme-cbpv-fiddle 700 100))
  (blank 1 24)
  (cc-superimpose
   (blank 840 40)
   (txt "Pushing an argument is the cons of the stack."
        29 '(bold . default) michigan-blue))
  (blank 1 22)
  (repository-row
   "🎻"
   "Fiddle"
   "A call-by-push-value Scheme"
   "https://github.com/zydeco-lang/fiddle")
  (colorize (filled-rectangle 840 1) rule-gray)
  (repository-row
   "🪗"
   "Zydeco"
   "A typed call-by-push-value language"
   "https://github.com/zydeco-lang/zydeco")))
