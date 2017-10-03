===============
Getting started
===============

This page describes how to install `bistro`, write a small pipeline
and execute it.

Installation
============

``bistro`` can be used under Linux and MacOSX (never tried with
Windows). It can easily be installed using ``opam``, the OCaml package
manager. You can install ``opam`` following these `instructions
<https://opam.ocaml.org/doc/Install.html>`__. Typically, under
Debian/Ubuntu just

.. code:: sh
    $ apt update && apt install opam

(as root or using ``sudo``). Once this is done, initialize a fresh
``opam`` repository and install ``bistro``:

.. code:: sh
    $ opam init --comp=4.04.2

Take good care to follow the instructions given by ``opam`` after this
command ends. Now you're ready to install `bistro`, and ``utop`` which
is a nice interactive interpreter for OCaml:

.. code:: sh
    $ opam install bistro utop

If you're new to OCaml, you might want to install ``ocaml-top``, which
is a simple editor supporting syntax highlighting, automatic
indentation and incremental compilation for OCaml:

.. code:: sh
    $ opam install ocaml-top

You can also find similar support for other general-purpose editors
like ``emacs``, ``vi`` or ``atom``.
