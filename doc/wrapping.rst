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
``Bistro.Shell_dsl`` module which provides many convenient functions to
create new ``workflow`` values. Here's what it looks like:

.. code-block:: ocaml

   open Bistro.Shell_dsl

   let touch =
     Workflow.shell ~descr:"touch" [
       cmd "touch" [ dest ] ;
     ]

Let's describe what we wrote:
  - the first line (open statement) makes all the many handy functions
    from ``Bistro.Shell_dsl`` visible in the current scope; many
    functions we describe below come from this module
  - we define ``touch`` by calling a function from ``Bistro.Workflow``
    named ``shell``. As the name suggests, workflow steps it defines
    are built calling a command line on a shell.
  - this function takes an argument ``descr`` which can be used to give
    a name to the workflow. This argument is optional and is only used
    for display purpose, but it helps ``bistro`` to display readable
    information when logging
  - the second and last argument of ``Workflow.shell`` is a list of
    commands that will be executed when the workflow is run
  - a command can be built with the ``cmd`` function from
    ``Bistro.Shell_dsl``, which takes a string providing the name of
    the executable to run and a list of arguments
  - arguments are of type ``Bistro.Shell_dsl.template``, which can be
    seen as a representation of text with some special tokens inside,
    that can be replaced by some value when we try to execute the
    command
  - the single argument to our command (``dest``) is an example of these
    special tokens, and represents a path where ``bistro`` expects to
    find the result file or directory of the workflow

Basically defining a workflow amounts to providing a list of commands
that are expected to produce a result at the location represented by
the token ``dest``. **Note that a workflow that doesn't use ``dest``
is necessarily incorrect** since it has no means to produce its output
at the expected location. The value ``touch`` we have defined has type
``'a path workflow``, and represents a recipe (right, a very simple
one) to produce a result file. This type is too general and we'd have
to restrict it to prevent run-time error, but we'll see that
later. Let's now see how we make make a pipeline on some parameter.

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
instead we can simply write a function that produces our workflow:

.. code-block:: ocaml

   let echo msg =
     workflow ~descr:"echo" [
       cmd "echo" ~stdout:dest [ string msg ] ;
     ]

Our workflow is now a lot more generic, since it can be used to
produce files with any content. Well saying workflow here is slightly
incorrect, because the value ``echo`` has type ``string -> 'a path
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
has type ``'a path workflow -> 'b path workflow``. That is, given a
first workflow, this function is able to build a new one. This new
workflow will call ``sort`` redirecting the standard output to the
expected destination and giving it ``text_file`` as an argument. More
precisely, ``bistro`` will inject the location it decided for the
output of workflow ``text_file`` in the command invocating
``sort``. By combining the use of ``dep`` and ``dest``, you can write
entire collections of interdependent scripts without ever caring about
where the generated files are stored.

Utility functions to describe a command's arguments
===================================================

The functions ``string`` and ``dep`` are enough to describe virtually
any command-line argument to a program. In addition, the module
``Bistro.Shell_dsl`` provides a few more utility functions that help
writing concise and readable wrappers. The following code illustrates
the use of a few of them on a simplified wrapper for the ``bowtie``
command:

.. code-block:: ocaml

   let bowtie ?v index fq1 fq2 =
     workflow ~descr:"bowtie" [
       cmd "bowtie" [
         string "-S" ;
         opt "-1" dep fq1 ;
         opt "-2" dep fq2 ;
         option (opt "-v" int) v ;
         seq ~sep:"" [ dep index ; string "/index" ] ;
         dest ;
       ]
     ]

Let us examine each parameter to this command from top to bottom:
  - the first argument is a simple ``-S`` switch, we encode it
    directly with the ``string`` function
  - the second and third arguments are paths to input files introduces
    with a switch; here writing ``[ ... ; opt "-1" dep fq1 ; ... ]``
    is equivalent to writing ``[ ... ; string "-1" ; dep fq1 ;
    ... ]`` but is shorter and more readable
  - the fourth argument is optional; notice that the variable ``v`` is
    an optional argument to the ``bowtie`` function, so it is of type
    ``'a option``; the ``option`` function from ``Bistro.Shell_dsl``
    will add nothing to the command line if ``v`` is ``None`` or else
    apply its first argument to the value if holds. In that case, the
    applied function adds an integer argument introduced by a ``-v``
    switch
  - the fifth argument features a constructor called ``seq`` that can
    be used to concatenate a list of other chunks interspersed with a
    string (here the empty string); here we use it to describe a
    subdirectory of a workflow result
  - the last argument is simply the destination where to build the
    result.

Typing workflows
================

We have seen that the ``Workflow.shell`` function from
``Bistro.Shell_dsl`` can be used to make new workflows that call
external programs. This function has of course no means to know what
the format of the result file or directory will be. For this reason,
it outputs a value of type ``'a path workflow``, which means a result
whose format is compatible with any other. This is obviously wrong in
the general case, and could lead to run-time errors by feeding a tool
with inputs of an unsupported format. In order to prevent such
run-time errors, we can provide more precise types to our functions
producing workflows, when we have more information. Let's see that on
an example. FASTA files have the property that when you concatenate
several of them, the result is still a FASTA file (this is false in
general case of course). We are now going to write a workflow that
concatenates several FASTA files, and make sure its typing reflects
this property.

Both ``Bistro`` and ``Bistro_bioinfo`` define a few type definitions
for annotating workflows. In particular we'll use
``Bistro_bioinfo.fasta`` for our example. Here's how it looks:

.. code-block:: ocaml

   open Bistro
   open Bistro.Shell_dsl
   open Bistro_bioinfo

   let fasta_concat (x : fasta pworkflow) (y : fasta pworkflow) : fasta pworkflow =
     workflow ~descr:"fasta-concat" [
       cmd "cat" ~stdout:dest [ dep x ; dep y ] ;
     ]

Note the ``'a pworkflow`` type which is used here, and which is
synonym for ``'a path workflow``. Alternatively, you can define your
workflow in a ``.ml`` file:

.. code-block:: ocaml

   open Bistro.Shell_dsl

   let fasta_concat x y =
     workflow ~descr:"fasta-concat" [
       cmd "cat" ~stdout:dest [ dep x ; dep y ] ;
     ]

and constraint its type in the corresponding ``.mli`` file:

.. code-block:: ocaml

   open Bistro
   open Bistro_bioinfo

   val fasta_concat : fasta pworkflow -> fasta pworkflow -> fasta pworkflow
