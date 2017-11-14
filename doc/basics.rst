==============================
Basics: how to write pipelines
==============================

Now that we have a working installation of ``bistro`` and ``docker``,
let us get back to our original goal, namely to write pipelines of
scientific computations. In the following we'll use the ``utop``
interpreter to run an OCaml script. If you write your code in a file
named ``pipeline.ml``, you can run it by typing

.. code-block:: bash

   $ utop pipeline.ml

which will create a ``_bistro`` directory used to store the results of
the pipeline. We'll get back to that later, let's now start with an
overview of the library.

What's in bistro
================

``bistro`` essentially brings three main components:
  - a data structure to represent a workflow, understood as a
    collection of interdependent shell scripts
  - an execution engine that can run a workflow, featuring parallel
    build, resume-on-failure and logging
  - a library of pre-defined workflows to easily run applications from
    the field of computational biology

Those three components are provided as three libraries, respectively
named ``bistro``, ``bistro.engine`` and ``bistro.bioinfo``. A fourth
library named ``bistro.utils`` provides more convenient functions to
run workflows and log execution.

One key feature of ``bistro`` is that workflows are described without
ever caring for file names. The result of each computational step is
automatically named and stored in a cache.

For a typical application, one will first describe the expected
workflow either using already defined wrappers or by defining new
ones. Once this is done, we define the outputs we want from the
workflow, and how they should be layed out in an output directory
(called an output repo). And finally we send this description to a
build engine that will actually run the workflow.


A tiny QC pipeline
==================

Let's write the above mentionned three parts on a simple example to
perform quality check (QC) on a high-throughput sequencing
sample. First, we need to load the library and open the appropriate
modules:

.. code-block:: ocaml

   #require "bistro.bioinfo bistro.utils"

   open Bistro.EDSL
   open Bistro_bioinfo.Std
   open Bistro_utils

This will make the functions from the three components available. Then
we can start writing our pipeline, with the following
steps:

  1. download a sample from the `SRA <https://www.ncbi.nlm.nih.gov/sra>`_ database,
  2. convert it to FASTQ format,
  3. run `FastQC <https://www.bioinformatics.babraham.ac.uk/projects/fastqc/>`_
     on this data. Using the functions from the ``bistro.bioinfo`` library.

This is how it goes:

.. code-block:: ocaml

   let sample = Sra.fetch_srr "SRR217304"
   let sample_fq = Sra_toolkit.fastq_dump sample
   let qc = FastQC.run sample_fq

Now we need to specify which output we are interested in, using the
``Repo`` module:

.. code-block:: ocaml

   let repo = Repo.[
       ["qc"] %> qc ;
     ]

Here we specify that in our result directory, we want the output of
``FastQC`` to be named ``qc``. The two other steps will not appear in
the result directory, as we are not really interested in seeing them.

Finally, we can run the workflow using a function from the ``Repo``
module:

.. code-block:: ocaml

   let () = Repo.build ~outdir:"res" repo

This will execute the workflow and place the result file we asked in
it. You're now ready to actually run the pipeline: save the file and
invoke

.. code-block:: bash

   $ utop pipeline.ml

