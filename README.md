# yaspcompiler

`yaspcompiler` - Yet Another Scheme-Programmed Compiler

`yaspcompiler` is a compiler for Fortress - a statically typed imperative programming language for [dwarf-vm][dwarf-vm] - stack virtual machine.

[dwarf-vm]: https://github.com/sayon/dwarf-vm

# Usage

Use your favourite Scheme interpreter (`guile` works best) to load file `compile.ss`. Then you can use the `compile-dwarf` procedure:

`compile-dwarf output-file input-file...`

to compile and link specified input files into one dwarf-vm bytecode output file.