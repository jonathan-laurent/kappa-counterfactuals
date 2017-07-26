Kappa resimulation algorithm
---

Here is an implementation of the resimulation algorithm for Kappa.
It generates a random counterfactual trace given a reference trace and
an intervention.

### How to install

In order to build this repository, you need to install the latest development
version of KaSim: `opam pin --dev add KaSim`. Then, just type `make`.

### How to use

The resimulator is mostly intended to be used as an OCaml library whose interface
can be found at `src/resimulation.mli`. However, a restricted subset of features
is currently available through the `resimulate` executable that can be used as 
follows:
```
resimulate TRACE_FILE -b RULE
```
resimulates the trace contained in the json file `TRACE_FILE`, blocking every instance
of rule `RULE`.