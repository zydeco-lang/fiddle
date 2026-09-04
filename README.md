# Fiddle 🎻

This repo contains an experimental, Scheme-like dynamically typed
variant of Paul Blain Levy's Call-by-push-value (CBPV), called
*Fiddle*. Fiddle is based on the "Scheme-like" model of gradual typing
in a CBPV setting we designed in [a paper on gradual typing][GTT] in a
CBPV-like metalanguage. Being based on CBPV, Fiddle allows for
call-by-value and call-by-name programming styles, resulting in a
language that feels a bit like a combination of Scheme, Haskell and
continuation-passing style.

The basic idea is that instead of being uni-typed, Fiddle is "dityped"
in that there are 2 kinds of expressions: inert values and effectful
computations. Values are S-expressions as in a typical Scheme: they
include symbols, boolean values, strings, ..., as well as immutable
cons pairs and "thunked" computations. Computations are effectful
programs that interact with the stack. The current stack is always
either an opaque continuation waiting for the computation to `ret` a
value to it, or is a value pushed onto a larger stack. The basic
primitive for interacting with the stack is `case-lambda` which
co-pattern matches on the stack to see if there are any arguments
left. This stack-manipulation provides a slightly lower level
interface for implementing variable-arity functions than typical
Schemes do.

The language is implemented as a "#lang" in Racket, using the
[turnstile][turnstile] library.

# Repository Structure

1. The actual library is in the `fiddle` directory, which can be
   installed using `raco`. It also contains a bare-bones stdlib.
2. More example code can be found in the `examples` directory.
3. The `cbpv` directory contains the beginnings of a typed version of
   the language.
4. The `hs` directory contains the beginnings of an interpreter
   written in Haskell.
5. The `notes` directory contains some notes on language design and
   comments on programming in this unfamiliar style.

# Performance

I've done some optimization but there is still a high amount of overhead from copattern matching and combinators that use copattern matching. This is hopefully mostly just interpretive overhead of the copattern matcher, and a smarter implementation should be a big improvement.

# The Name

In the Scheme tradition, Fiddle is named after a con: the [fiddle game](https://en.wikipedia.org/wiki/List_of_scams#Fiddle_game). In the fiddle game, two people who appear to be opposed are actually both working together to rip off the mark. In Fiddle, values and stacks are superficially quite different but are really playing quite similar roles.

[turnstile]: https://docs.racket-lang.org/turnstile/index.html
[GTT]: https://arxiv.org/abs/1811.02440
