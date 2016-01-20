# bistro: build and run distributed scientific workflows

`bistro` is an [OCaml](http://ocaml.org) library to build and run
computations represented by a collection of interdependent scripts, as
is often found in applied research (especially computational
biology). 

**Features**:
- build complex, composable and reusable workflows from basic
  components declaratively
- simple and lightweight wrapping of new components
- resume-on-failure: if something fails, fix it and the workflow will
  be run from where it stopped
- parallel workflow execution (locally or over a PBS cluster)
- development-friendly: when a script is modified, only the necessary
  part of the workflow is re-run
- automatic naming of generated files
- static typing: detect file format error at compile time!

The library provides a datatype to represent scripts (including
metadata and dependencies), an engine to run workflows and a small
standard library for now mainly geared towards computational biology.

Questions, suggestions or contributions are welcome, please file an
[issue](https://github.com/pveber/bistro/issues) as needed.

