============
OCaml primer
============

Writing a workflow with ``bistro`` requires to learn a tiny subset of
the `OCaml <http://ocaml.org>`_ language. This page aims at quickly
presenting this subset, which should be sufficient to write basic
pipelines. For the interested reader, I recommend the following
`easy
introduction <https://www.goodreads.com/book/show/18212242-ocaml-from-the-very-beginning>`_
to the language and functional programming in general.


OCaml is a *functional language*, which means in brief that variables
cannot be modified. The immediate consequence of this is that ``for``
or ``while`` loops are then pretty useless and are replaced by
(possibly recursive) function calls. An OCaml program is a sequence of
expressions (like ``1 + 1``) or definitions introduced by the keyword
``let``. For instance, the program

.. code-block:: ocaml

   let a = 1 + 1

defines a variable named ``a``, which has value ``2``. This name can be
reused in subsequent definitions, like in:

.. code-block:: ocaml

   let a = 1 + 1
   let b = 2 * a

A name cannot be used if it was not defined previously. If a name is
used twice, the two definition coexist but only the last one is
visible from the subsequent definitions. Hence, in the following
program:

.. code-block:: ocaml

   let a = 1 + 1
   let b = 2 * a
   let a = 1
   let c = a

the variable ``c`` has value ``1``.

Getting started with the OCaml interpreter
==========================================

While OCaml programs can be compiled into executables, it is also very
convenient to enter programs interactively using an *interpreter*
(similar to what exists for python or R). The OCaml language has a
very nice interpreter called [``utop``](https://github.com/diml/utop)
than can easily installed using ``opam``. In a shell just type:

.. code-block:: sh

   opam install utop

and then you can call ``utop`` on the command line. An interpreter like ``utop`` reads expressions or definitions, evaluates them and prints the result. Expressions or definitions sent to ``utop`` should be ended with ``;;`` (this is not needed in OCaml programs). For instance, let's enter a simple sentence ``let a = 1;;``. ``utop`` answers as follows:

.. code-block:: ocaml

        OCaml version 4.02.3

   # let a = 1;;
   val a : int = 1

The interpreter answers that we just defined a variable named ``a``,
of type ``int`` (the basic type for integers`` and equal to
``1``. Let's enter other definitions to meet new basic data types,
like strings:

.. code-block:: ocaml

   # let s = "bistro";;
   val s : string = "bistro"

booleans:

.. code-block:: ocaml

   # let b = true;;
   val b : bool = true

or floating-point numbers:

.. code-block:: ocaml

   # let x = 3.14159;;
   val x : float = 3.14159

To quit the interpreter, just press ``Ctrl+D``

Functions
=========

In OCaml, functions can be defined with the ``fun`` keyword. For
instance, the expression ``fun x -> x + 1`` denotes the function that
given some integer returns the next integer. We can of course give the
function a name like for any other value:

.. code-block:: ocaml

   # let f = fun x -> x + 1;;
   val f : int -> int = <fun>

Note that the interpreter "guessed" the type of ``f``, as a function
that takes an integer and returns an integer. This function can then
be called using the following syntax:

.. code-block:: ocaml

   # f 41;;
   - : int = 42

In OCaml, the arguments of a function are just separated by spaces. In
general we use a simpler (but equivalent) notation to define
functions:

.. code-block:: ocaml

   # let f x = x + 1;;
   val f : int -> int = <fun>
