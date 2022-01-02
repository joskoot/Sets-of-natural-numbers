#lang scribble/manual

@(require
  scribble/core
  scribble/eval   
  racket
  "natset.rkt"
  "scribble-utensils.rkt"
  (for-label "natset.rkt" racket)
  (for-template "natset.rkt" racket)
  (for-syntax racket)) 

@title[#:version ""]{Finite sets of natural numbers@(lb)and their infinite complements.}
@author{Jacob J. A. Koot}
@(defmodule Sets-of-natural-numbers/natset #:packages ())
@;@(defmodule "natset.rkt" #:packages ())

@section{Representation}

Let @bold{N} be the set of all natural numbers, 0 included.
A finite set of natural numbers can be represented by one single natural number, say n.
Let b@↓{k} be bit k of the binary positional representation of n,
counting the bits starting from 0 and in increasing order of significance up to and including the
most significant non-zero bit.
n can be regarded as a representation of the set S@↓{n}
of all natural numbers k for which bit b@↓{k} is 1.
0 represents S@↓{0}, which is empty.
The two's complement of @(minus)(n+1) represents the complement of S@↓{n}.
S@↓{@(minus)(n+1)} = @bold{N}\S@↓{n}, which is infinite.
The set of all subsets of @bold{N} is uncountably infinite,
whereas the set @bold{Z} of all integer numbers is countably infinite.
Hence, the representation does not include all subsets of @bold{N},
even when disregarding the finiteness of memory.
For example, it does not include the set of all odd natural numbers,
nor the set of all prime numbers.

@ignore{@note{I intend to make a module that allows the representation of
sets like that of the set of all odd natural numbers.
This can be done with a representation in terms of predicates.
However, such a representation necessarily is much less efficient.
In principle such a representation can include all subsets of @bold{N},
but in practice it cannot not do so if we limit the description of the predicates
to a finite number of characters.}}

@inset{@Tabular[
(("n" "binary" "represents")
 ("0" @natset->string[0 #:min-width 6] "the empty set {}.")
 ("1" @natset->string[1 #:min-width 6] "set {0}")
 ("2" @natset->string[2 #:min-width 6] "set {1}.")
 ("3" @natset->string[3 #:min-width 6] "set {0, 1}.")
 (@(list @(minus)"1") @natset->string[-1 #:min-width 6]
   @(list @bold{N} ", the set of all natural numbers."))
 (@(list @(minus)"2") @natset->string[-2 #:min-width 6]
   @(list @bold{N} "\\{0}, id est, the set of all natural numbers except 0." ))
 (@(list @(minus)"3") @natset->string[-3 #:min-width 6]
   @(list @bold{N} "\\{1}, id est, the set of all natural numbers except 1." ))
 (@(list @(minus)"4") @natset->string[-4 #:min-width 6]
   @nb{@(list @bold{N} "\\{0, 1},  id est, the set of all natural numbers except 0 and 1.")}))
#:sep (hspace 3)
#:row-properties '((top-border bottom-border) ()()()()()()() 'bottom-border)
#:column-properties '(right 'center 'left)]}

In the following a natural number taken as representation of a set of natural numbers
will be called a “natset”.

@section{Constants and procedures}

@defproc[#:kind "predicate" (natset? (obj any/c)) boolean?]{
Same as @nbr[natural?] in the sense of @nbr[eq?].}

@defthing[empty-natset natset? #:value 0]{
Reprents the empty set (of natural numbers).}

@defthing[total-natset natset? #:value -1]{
Represents the set of all natural numbers.}

@defproc[(make-natset
(arg (or/c natural?
           (and/c (list/c natural? natural?) (< (car arg) (cadr arg))))) ...)
natset?]{
An @nbr[arg] consisting of a single natural number is included in the set to be returned.
A range @nbr[(from to)] includes the numbers @nbr[from] up to but not including @nbr[to].
Overlapping arguments do no harm. Eamples:

@Interaction[
(natset->string (make-natset)         #:min-width 12)
(natset->string (make-natset 0)       #:min-width 12)
(natset->string (make-natset 1)       #:min-width 12)
(natset->string (make-natset 0 1)     #:min-width 12)
(natset->string (make-natset '(0 10)) #:min-width 12)
(code:comment "Repeated arguments are no problem")
(natset->string (make-natset 5 6 5 7) #:min-width 12)
(code:comment "Overlapping arguments are no problem")
(=
 (make-natset '(0 10) '(5 20))
 (make-natset '(0 20))
 (sub1 (expt 2 20)))]}

@defproc[(natset-union (natset natset?) ...) natset?]{
Returns the natset representing the union of its arguments.
Without arguments the @nbr[empty-natset] (= @nbr[0]) is returned.
In fact @nbr[natset-union] is the same as @nbr[bitwise-ior].

@Interaction[
(define a (make-natset '(5 10)))
(define b (make-natset '(8 15)))
(printf "~a~n~a~n~a"
 (natset->string a #:min-width 20)
 (natset->string b #:min-width 20)
 (natset->string (natset-union a b) #:min-width 20))]}

@defproc[(natset-intersection (natset natset?) ...) natset?]{
Returns the natset representing the intersection of the arguments.
Without arguments the @nbr[empty-natset] (= @nbr[0]) is returned.
In fact @nbr[natset->union] is the same as @nbr[bitwise-and].


@Interaction[
(define a (make-natset '(5 10)))
(define b (make-natset '(8 15)))
(printf "~a~n~a~n~a"
 (natset->string a #:min-width 20)
 (natset->string b #:min-width 20)
 (natset->string (natset-intersection a b) #:min-width 20))]}

@defproc[(natset-complement (natset natset?)) natset?]{
Returns the natural number representing the complement of @nbr[natset],
id est, the set of all natural numbers, but without those in @nbr[natset].
(Same as @nbr[(- (add1 natset))])

@Interaction[
(define a (make-natset '(5 10)))
(define b (natset-complement a))
(printf "~a~n~a"
 (natset->string a #:min-width 20)
 (natset->string b #:min-width 20))]

Union of a natset with its complement always returns the whole natset @bold{N}:

@Interaction[
(for/and ((k (in-range 1 10)))
 (define natset (make-natset (list 0 k)))
 (= (natset-union natset (natset-complement natset)) -1))]}


@defproc[(natset-subtract (natset natset?) (to-be-removed natset?) ...) natset?]{
Returns the natural number representing the @nbr[natset] from which
all elements of the @nbr[to-be-removed] natsets are removed.
Called without any @nbr[to-be-removed] natset, @nbr[natset] is returned
without removing any element. Elements in a @nbr[to-be-removed] natset
not present in the @nbr[natset] do no harm. They are ignored.

@Interaction[
(for/list ((k (in-range 5 11)) (l (in-naturals 7)) (m (in-range 2 20 3)))
 (natset->string
  (natset-subtract
   (make-natset '(0 20))
   (make-natset m)
   (make-natset (list k l)))))
(code:comment #,(list "Elements present in "
                      @nbr[to-be-removed]
                      " but not present in "
                      @nbr[natset]
                      " are ignored:"))
(natset->string (natset-subtract (make-natset 1 2 3) (make-natset 2 5 7)))]}

@defproc[(natset-member? (natset natset?) (k natural?)) boolean?]{
@nbr[#t] if @nbr[k] is an element of @nbr[natset], else @nbr[#f].

@Interaction[
(define natset (make-natset '(2 5) '(8 11)))
(for/list ((k (in-range 0 20)) #:when (natset-member? natset k)) k)]}

@defproc[(natset->string (natset natset?) (#:min-width min-width natural? 0)) string?]{
Converts the @nbr[natset] to a string of bits 0 and 1, at least @nbr[min-width] of them.
Count the bits from right to left and starting from 0.
Bit k being 0 means that k is not in the @nbr[natset].
Bit k being 1 means that k is in the @nbr[natset].
Interpret the leftmost bit as an infinite sequence of this bit.

@Interaction[
(code:line (natset->string -1) (code:comment "Read this as an infinite sequence of ones."))
(code:line (natset->string 0) (code:comment "Read this as an infinite sequence of zeros."))
(define set.3.to.5.included (make-natset '(3 6)))
(code:line (natset->string set.3.to.5.included) (code:comment "Contains no elements greater than 5."))
(code:line (natset->string (natset-complement set.3.to.5.included))
           (code:comment "Contains all numbers greater than 5."))]}



1111111111



