#!/usr/bin/env sh

set -e

opam switch create --deps-only --with-doc --with-test -y .
eval $(opam env)
opam install -y tuareg merlin
