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
the token ``dest``.

