QCSTM: A Simple State-Machine Framework for OCaml Based on QCheck 
=================================================================

This library implements a simple, typed state machine framework for
property-based testing of imperative code. Tests are described by (a
generator of) symbolic commands and two command interpreters over an
abstract model and the system under test.

The library requires a recent installation of both OCaml and the [QCheck](https://github.com/c-cube/qcheck) framework.

State-machine frameworks for other languages include:
 - [Quviq QuickCheck](http://www.quviq.com/downloads/) for Erlang
 - [Proper](https://proper-testing.github.io/) for Erlang  
 - [Triq](http://krestenkrab.github.io/triq/) for Erlang  
 - [ScalaCheck](https://www.scalacheck.org/) for Scala  
 - [Hedgehog](https://github.com/hedgehogqa) for Haskell and R  
 - [quickcheck-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine) for Haskell
 - [fast-check](https://github.com/dubzzz/fast-check) for JavaScript/TypeScript  
 - [Lua-QuickCheck](https://github.com/luc-tielen/lua-quickcheck) for Lua  
 - [RapidCheck](https://github.com/emil-e/rapidcheck) for C++  
 - ...

QCSTM takes inspiration from the commercial Erlang state machine
framework from Quviq and from ScalaCheck's state machine framework.

The library is formulated as an OCaml functor. As its argument, the
functor expects a module specifying 3 types:

- `cmd`: the type of commands
- `state`: the type of model's state
- `sut`: the type of the system under test

In addition the user has to provide:

- `arb_cmd`: a generator of commands. It accepts a state parameter to enable state-dependent command generation.
- `init_state` and `next_state`: specifies the initial state and the (single-step) state transition function
                                 of the model.
- `run_cmd`: interprets a command over the system under test and returns a Boolean, indicating whether the
             execution went well, and whether any returned value agrees with the model's result.
- `init_sut` and `cleanup`: specificies how to initialize and clean up after the system under test.
- `precond`: specifies preconditions for each command. This is useful, e.g., to prevent the shrinker from
             breaking invariants when minimizing counterexamples.

In return, the framework provides a generator of `cmd` lists (incl. a shrinker)
as well as an agreement test between the model and system under test.


Installation
------------

With `opam` this should be as simple as `opam install qcstm`.

You can also install from source assuming you have `ocamlbuild`,
`ocamlfind` and a not-too-ancient `qcheck` installed, by issuing:
```
  make
  make install
```

To uninstall with `opam` just run `opam remove qcstm`.
To uninstall from a source installation run `make uninstall`
from the souce directory.


An example
----------

Consider the following example (available in examples/counter.ml) that
tests an `int ref` against a model consisting of a single `int`:

```ocaml
  open QCheck
  
  module CConf =
  struct
    type cmd =
      | Incr
      | Decr
      | Set of int
      | Deref [@@deriving show { with_path = false }]
    type state = int
    type sut = int ref
  
    let arb_cmd _ =
      let int_gen = Gen.oneof [Gen.int; Gen.small_int] in
      QCheck.make ~print:show_cmd
        (Gen.oneof [Gen.return Incr;
                    Gen.return Decr;
                    Gen.map (fun i -> Set i) int_gen;
                    Gen.return Deref])
  
    let init_state  = 0
    let init_sut () = ref 0
    let cleanup _   = ()
  
    let next_state c s = match c with
      | Incr  -> s+1
      | Decr  -> s-1
      | Set i -> if i<>1213 then i else s (* an artificial fault *)
      | Deref -> s
  
    let run_cmd c s r = match c with
      | Incr  -> (incr r; true)
      | Decr  -> (decr r; true)
      | Set i -> (r := i; true)
      | Deref -> !r = s
        
    let precond _ _ = true
  end
  module CT = QCSTM.Make(CConf)
  ;;
  QCheck_runner.run_tests ~verbose:true [CT.agree_test ~count:10_000 ~name:"ref-model agreement"]
```

Here we provide a type of four different kinds of commands as well as
a generator of these. `init_state` and `init_sut` specifies the
initial states of both the model and the system under test.

`next_state` and `run_cmd` interpret the four different commands over
the model and the system under test, respectively. Since we can only
observe references through a dereferencing operation, this is the only
operation comparing the outputs from the two.

To test whether the testsuite works as expected, we inject a bug in
the model that ignores setting the reference when the argument is
1213.

Finally we can compile the state machine model and run the
tests. Depending on the underlying random number generator, this may
or may not catch the model's bug in a given run:


```
  $ make counter
  ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/counter.cma examples/counter.native
  Finished, 8 targets (3 cached) in 00:00:00.
  $ ./counter.native 
  random seed: 272260055
  generated error  fail  pass / total     time test name
  [✓] 10000     0     0 10000 / 10000     1.0s ref-model agreement
  ================================================================================
  success (ran 1 tests)
  $ ./counter.native 
  random seed: 36511368
  generated error  fail  pass / total     time test name
  [✗]  2032     0     1  2031 / 10000     1.2s ref-model agreement
  
  --- Failure --------------------------------------------------------------------
  
  Test ref-model agreement failed (14 shrink steps):
  
  [(Set 1213); Deref]
  ================================================================================
  failure (1 tests failed, 0 tests errored, ran 1 tests)
```


A number of additional examples are provided in the `examples` directory.
