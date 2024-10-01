===============
Getting started
===============

This page describes how to install ``bistro``, write a small pipeline
and execute it.

Installation
============

``bistro`` can be used under Linux and MacOSX (never tried with
Windows). It can easily be installed using ``opam``, the OCaml package
manager. You can install ``opam`` following these `instructions
<https://opam.ocaml.org/doc/Install.html>`__. Typically, under
Debian/Ubuntu just

.. code-block:: bash

   $ apt update && apt install opam

(as root or using ``sudo``). Once this is done, initialize a fresh
``opam`` repository and install ``bistro``:

.. code:: sh
   
   $ opam init --comp=4.07.1

**Take good care** to follow the instructions given by ``opam`` after this
command ends. Typically you will need to add this line:

.. code:: sh
   
   $ . ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

to your ``.bashrc`` and execute

.. code:: sh
   
   $ eval `opam config env`

for OPAM to be configured in your current console.

Now you're ready to install `bistro`, and ``utop`` which
is a nice interactive interpreter for OCaml:

.. code:: sh

   $ opam install bistro bistro-bio utop

If you're new to OCaml, you might want to install ``ocaml-top``, which
is a simple editor supporting syntax highlighting, automatic
indentation and incremental compilation for OCaml:

.. code:: sh

   $ opam install ocaml-top

You can also find similar support for other general-purpose editors
like ``emacs``, ``vi`` or ``atom``.

A simple example
================

Using your favorite editor, create a file named ``pipeline.ml`` and
paste the following program:

.. code-block:: ocaml

   #require "bistro.bioinfo bistro.utils"

   open Bistro
   open Bistro_bioinfo
   open Bistro_utils

   let sample = Sra.fetch_srr "SRR217304"           (* Fetch a sample from the SRA database *)
   let sample_fq = Sra_toolkit.fastq_dump sample    (* Convert it to FASTQ format *)
   let genome = Ucsc_gb.genome_sequence `sacCer2    (* Fetch a reference genome *)
   let bowtie2_index = Bowtie2.bowtie2_build genome (* Build a Bowtie2 index from it *)
   let sample_sam =                                 (* Map the reads on the reference genome *)
     Bowtie2.bowtie2 bowtie2_index (SE_or_PE.Single_end [ sample_fq ])
   let sample_peaks =                               (* Call peaks on mapped reads *)
     Macs2.(callpeak sam [ sample_sam ])

   let repo = Repo.[
     [ "peaks" ] %> sample_peaks
   ]

   (** Actually run the pipeline *)
   let () = Repo.build_main ~outdir:"res" ~np:2 ~mem:(`GB 4) repo

Running a pipeline
==================

A typical bioinformatics workflow will use various tools that should
be installed on the system. Maintaining installations of many tools on
a single system is particularly time-consuming and might become
extremely tricky (e.g. to have several versions of the same tool, or
tools that have incompatible dependencies on very basic pieces of the
system, like the C compiler). To avoid this problem, ``bistro`` can
use so-called *containers* like `Docker <https://www.docker.com/>`__
or `Singularity <https://www.sylabs.io/>` to run each tool of the
workflow in an isolated environment containing a proper installation
of the tool. In practice, you don't have to install anything: for each
step of a workflow ``bistro`` will invoke a container specifying which
environment it needs. This is a tremendous time-saver in practice to
deploy a pipeline on a new machine.

To get there you have to install ``docker`` or ``singularity``. Follow
instructions on `this page
<https://docs.docker.com/engine/installation/#supported-platforms>`__
for ``docker̀` and `this one
<https://www.sylabs.io/guides/3.0/user-guide/quick_start.html#quick-installation-steps>`__
for ``singularity``. Summarized instructions are also available `there
<installing-docker.html>`_ for ``docker̀`. Note that ``bistro`` can be
used without containers, but in that case, you must make each program
used in the pipeline available on your system.

Assuming ``docker`` is installed on your machine, you can simply run
your pipeline by:

  .. code:: bash

     $ utop pipeline.ml

At the end you should obtain a ``res`` directory where you will find
the output files of the pipeline.

In the remainder of this section we'll look at the code in more
details, but first we'll need to learn a bit of the OCaml language.
