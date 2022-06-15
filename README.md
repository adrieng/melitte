# Mélitte

This is *Mélitte*, a toy implementation of Martin-Löf Type Theory (MLTT) written
in the OCaml language.

## Compiling

You need a working OCaml development environment with Dune and OPAM. To install
dependencies and compile, simply run the following commands.

```shell
$ ./install-deps.sh
$ dune build
```

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

## TODO

- Fix shift/reduce conflict
