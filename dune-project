(lang dune 2.4)

(name qcstm)

(generate_opam_files true)

(license BSD-2-clause)
(maintainers "Jan Midtgaard <mail@janmidtgaard.dk>")
(authors "Jan Midtgaard <mail@janmidtgaard.dk>" )
(source (github jmid/qcstm))
(documentation "https://github.com/jmid/qcstm")

(package
 (name qcstm)
 (depends
  (ocaml  (>= 4.05.0))
  (qcheck (>= 0.8))
  (ppx_deriving   (and (>= 5.2) :with-test))
  (ctypes         (and (>= 0.19) :with-test))
  (ctypes-foreign (and (>= 0.18) :with-test)))
  (synopsis "A simple state-machine framework for OCaml based on QCheck")
  (description "
This library implements a simple, typed state machine framework for
property-based testing of imperative code. Tests are described by
(a generator of) symbolic commands and two command interpreters over
an abstract model and the system under test.

The library requires a recent installation of the QCheck framework.
")
  (tags ("state machine"
   "test"
   "property"
   "quickcheck")))