moloch
======

This project aims at developing a bootstrapping Scheme to C compiler.

The approach is to start with C and build upwards slowly, first adding closures then TCO. We aim to use extreme-programming in particular test-driver development to make everything as modular and robust as possible.


To do
=====

## C language stuff


* Finish defining the sexp interface to the C language
* Set up a testing infrastructure that will be used throughout
* Thoroughly test the C formatter
* Define scheme data types and implement the runtime in it
