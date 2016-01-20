# bistro: build and run distributed workflows

`bistro` is an [OCaml](http://ocaml.org) library to build and run
computations represented by a collection of interdependent scripts, as
is often found in applied research (especially computational
biology). 

**Features**:
- build complex and composable workflows declaratively
- simple and lightweight wrapping of new components
- resume-on-failure: if something fails, fix it and the workflow will
  restart from where it stopped
- parallel workflow execution (locally or over a PBS cluster)
- development-friendly: when a script is modified, bistro
  automatically finds out what needs to be recomputed
- automatic naming of generated files
- static typing: detect file format errors at compile time!

The library provides a datatype to represent scripts (including
metadata and dependencies), an engine to run workflows and a
standard library providing components for popular tools (although
mostly related to computational biology and unix for now).

Questions, suggestions or contributions are welcome, please file an
[issue](https://github.com/pveber/bistro/issues) as needed.

## Installation

I recommend installing `bistro` using
[opam](http://opam.ocaml.org/) (see
[installation instructions](http://opam.ocaml.org/doc/Install.html)). You
need a recent (at least 4.02.0) installation of OCaml. Once this is
done, simply type

```
opam install bistro
```

to install the library.

