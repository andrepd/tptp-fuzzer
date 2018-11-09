TPTP-fuzzer
===========

Generates random problems in TPTP-cnf format. Suitable for testing theorem provers.

Currently generates cnf problems only.

Building
--------

Dependencies: batteries, ocamlbuild (install with opam)

Run `make`

Help
----

TPTP-fuzzer currently has three modes:

* `print`: Print a random problem to stdout.
* `compare`: Generate a random problem, feed it to several solvers, and compare the results. Solvers are specified via the syntax `<name>:<path>`, where `name` is the name of a solver (see below) and `path` is the path to the binary. This enables you to test whether several solvers give the same results; use-cases include comparing your solver vs a reference one, or two versions of the same solver. 
	- Supported solvers: `iprover`, `vampire`/`vprover`. 
* `hammer`: As `compare`, but will run indefintely and report only when discrepancies arise. 
* `save`: Saves a test set in a directory.
* `load`: As `compare`, but with a test set previously saved with `save`.

All of these modes can be run *incrementally*, meaning rather than using all ![$n$][eq1] clauses of the problem at once, the program will run in sequence for clauseset ![$\{C_1\}, \{C_1,C_2\}, \dots, \{C_1,\dots,C_n\}$][eq2]. 

For more help use the `-h` or `--help` flag on the main binary or its subcommands.

Bugs
----

Report bugs on github or via email to <andrepd@protonmail.com>.



[eq1]: http://quicklatex.com/cache3/8f/ql_5040ad562e1c3048bfe6e8666d73768f_l3.png
[eq2]: http://quicklatex.com/cache3/1a/ql_2a9fb1a2fdd1e3ec82d434075064281a_l3.png
