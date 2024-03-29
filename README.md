# Mélitte

This is *Mélitte*, a toy implementation of Martin-Löf Type Theory (MLTT) written
in the OCaml language.

## Compiling

To compile Mélitte, you need a working OCaml development environment and some
libraries. The easiest and cleanest way to install them is to create a local
OPAM switch. The repository provides a script to do so.

```shell
$ ./create-local-switch.sh
$ dune build
```

(The script also installs Tuareg and Merlin for developer convenience.)

Some example programs to exercise the type-checker can be found in the
[test](test/) directory. Run them using `dune test`.

## Inspirations

Mélitte is strongly inspired from existing tutorial implementation of dependent
type theory. Here are the ones I looked at:

- Andràs Kovàcs' [elaboration
  zoo](https://github.com/AndrasKovacs/elaboration-zoo/).

- Daniel Gratzer's [Normalization-by-Evaluation for
  MLTT](https://github.com/jozefg/nbe-for-mltt).

- Jon Sterling's [DreamTT](https://github.com/jonsterling/dreamtt).

## Future Additions

### Foundations

- inductive types (W-types, a universe of descriptions à la McBride-Dagand?)
- universe of definitionally-irrelevant propositions

### Engineering

- explicit telescopes as arguments of pi and sigma

### Usability

- built-in non-dependent arrow and product types for better printing
- basic module system
- minimalistic Emacs mode with basic interaction facilities

### Refinement

- unification facilities for metavariables and implicit arguments
