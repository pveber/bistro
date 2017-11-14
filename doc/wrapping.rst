=====================
How to wrap new tools
=====================

The library ``bistro.bioinfo`` offers a handful of functions to call
various tools in computational biology, but of course many are
missing. The purpose of this chapter is to demonstrate the few steps
required to make a new tool available in ``bistro`` (a.k.a. wrapping).

A (very) basic example
======================

As a starting example, let's see how we'd proceed with a very silly
example, wrapping the ``touch`` command. To do so, we will use the
``Bistro.EDSL`` module which provides many convenient functions to
create new ``workflow`` values. Here's what it looks like:

.. code-block:: ocaml

   open Bistro.EDSL

   let touch =
     workflow ~descr:"touch" [
       cmd "touch" [ dest ] ;
     ]

Let's describe what we wrote:
  - the first line (open statement) makes all the many handy functions
    from ``Bistro.EDSL`` visible in the current scope; the functions
    we describe below come from this module
  - we define ``touch`` by calling the ``workflow`` function from
    ``Bistro.EDSL``
  - this function takes an argument ``descr`` which can be used to give
    a name to the workflow. This argument is optional and is only used
    for display purpose, but it helps ``bistro`` to display readable
    information when logging
  - the second and last argument of ``workflow`` is a list of commands
    that will be executed when the workflow is run
  - a command can be built with the ``cmd`` function from
    ``Bistro.EDSL``, which takes a string providing the name of the
    executable to run and a list of arguments
  - arguments are of type ``Bistro.Template.t``, which can be seen as
    a representation of text with some special tokens inside, that can
    be replaced by some value when we try to execute the command 
  - the single argument to our command (``dest``) is an example of these
    special tokens, and represents a path where ``bistro`` expects to
    find the result file or directory of the workflow

Basically defining a workflow amounts to providing a list of commands
that are expected to produce a result at the location represented by
the token ``dest``. The value ``touch`` we have defined has type ``'a
workflow``, and represents a recipe (right, a very simple one) to
produce a result file. This type is too general and we'd have to
restrict it to prevent run-time error, but we'll see that later. Let's
now see how we make make a pipeline on some parameter.

Parameterizing workflows
========================

Our ``touch`` workflow is a very normal OCaml value. It's a
datastructure that describes a recipe to produce a file. Let's write
another one which is very similar:

.. code-block:: ocaml

   let echo_hello =
     workflow ~descr:"echo_hello" [
       cmd "echo" ~stdout:dest [ string "hello" ] ;
     ]

There are a few newcomers here:
  - there is an argument ``stdout`` to the ``cmd`` function, which
    adds to the command what's necessary to redirect its standard
    output to a file. Here we redirect to ``dest``
  - we see that we can form arguments from simple strings with the
    ``string`` function. There are other such argument constructors,
    like ``int``, ``float`` and other more sophisticated ones

With this wrapper, we've encoded the following command line:

.. code-block:: bash

   $ echo "hello" > $DEST

So far so good. But do we really have to write a new wrapper each time
we want to change a small detail in the workflow? Of course not,
instead we can simply write our a function that produces our workflow:

.. code-block:: ocaml

   let echo msg =
     workflow ~descr:"echo" [
       cmd "echo" ~stdout:dest [ string msg ] ;
     ]

Our workflow is now a lot more generic, since it can be used to
produce files with any content. Well saying workflow here is slightly
incorrect, because the value ``echo`` has type ``string -> 'a
workflow``. It's a function that produces workflows, but since it will
be so common, I'll just call them workflows. To put it another way,
instead of writing a single script, we now have a function that can
produce a particular kind of script given a string.

Depending on others
===================

Most of the time, a computational step in a workflow will take as an
input the results obtained from some other. This can be expressed
thanks to the function ``dep``. Let's see right away how it can be
used to wrap the program ``sort``:

.. code-block:: ocaml

   let sort text_file =
     workflow ~descr:"sort" [
       cmd "sort" ~stdout:dest [ dep text_file ] ;
     ]

The value ``sort`` thus defined is again a function, but this time its
argument is a workflow. If you ask OCaml, it will say that ``sort``
has type ``'a workflow -> 'b workflow``. That is, given a first
workflow, this function is able to build a new one. This new workflow
will call ``sort`` redirecting the standard output to the expected
destination and giving it ``text_file`` as an argument.
