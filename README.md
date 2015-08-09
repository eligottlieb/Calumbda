# Calumbda
An implementation of [C.S. Calude's "Anytime Algorithms for Non-Ending
Computations"](https://researchspace.auckland.ac.nz/handle/2292/23906) for the untyped lambda calculus.

##Build, install, run

### Prerequisites
Your local machine will need to have [Haskell](https://www.haskell.org/) and
[Cabal](https://www.haskell.org/cabal/), for instance as part of the [Haskell
Platform](https://www.haskell.org/platform/).  You can also install
Haskell and Cabal via your operating system's package manager.

### Calumbda itself

First, clone Calumbda:

    $ git clone https://github.com/eligottlieb/Calumbda.git

Then, just use Cabal to configure and build it.

    $ cabal configure
    $ cabal build

Finally, make sure that your Cabal installation directory points where you want
it to, and install Calumbda:

    $ cabal install

Should you have your `$PATH` variable configured to include the Cabal
installation directory, you should now be able to run Calumbda:

    $ Calumbda
    >> _

## Examples

For a first example, we obtain 95% confidence that the Y fixed-point combinator,
applied to itself, indeed loops forever.

    >> :confidence 0.95 (lambda f. (lambda x. f (x x)) (lambda x. f (x x))) (lambda f. (lambda x. f (x x)) (lambda x. f (x x)))
    Nontermination 0.95

We then examine the same computational term, but try to find out how much
confidence we can have about its termination behavior in only 3000 evaluation
steps.

    >> :steps 3000 (lambda f. (lambda x. f (x x)) (lambda x. f (x x))) (lambda f. (lambda x. f (x x)) (lambda x. f (x x)))
    Nontermination 0.9817425814164945

We then try something a bit more prosaic: simple addition, but with too few
steps to fully evaluate the term.  As can be seen, in this few steps we can
barely obtain 30% confidence that the computation never halts!

    >> :steps 2 (3 + 3)
    Nontermination 0.29289321881345254

What if we add enough steps to the computation to fully evaluate it?  We find
that, when terms *do* normalize, we observe them normalizing with 100% probability.

    >> :steps 3 (3 + 3)
    Normalized 6
    >> :confidence 0.95 (3 + 3)
    Normalized 6

## Command and term languages

The command language consists of three commands:

* `:steps n Term` runs Term for `n` steps, and then displays the Calude result.
* `:confidence p Term` calculates how many steps are needed to obtain `p` confidence (that is, a probability of `p*100`%) that Term does not terminate, runs it that long, and then displays the Calude result.
* `:quit` quits the Calumbda interpreter.

##Supported language

The supported language is an untyped lambda calculus with integer constants and
addition added in.  Adding arithmetic operations meant that the language was no
longer actually unityped, and so dynamic type-checking had to be added.

In the following grammar, *n* ranges over integer constants, *id* over
alphanumberical identifiers, and *p* over probabilities within [0, 1):

    Command ::= ":steps" n Term | ":confidence" p Term
    Term ::= n | lambda id. Term | Term Term | Term + Term | (Term)

The grammar `Parser.y` can be compiled with the
[Happy parser generator](https://www.haskell.org/happy/) via `happy -ag Parser.y`.
