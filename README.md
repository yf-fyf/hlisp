----

**Notice:** This public repository is currently intended to obtain feedback from others
and is \*not\* intended to give a precise explanation of the author's work. 
All the theoretical and practical results of HLisp 
will be published as a research paper in some appropriate place.

----

# HLisp

HLisp is a toy (Scheme-like) Lisp implementation written in C.
This implementation has been created as a part of the author's conceptual study on
the notion of "homoiconicity"(cf. \[Davies 2009\]); and thus it was named as "HLisp" (not due to the Heisei era of Japan :p).

According to Alan Kay \[Kay 1969\], a language is called _homoiconic_ if its internal and external representations are essentially the same.
This notion of "(syntactic) homoiconicity" is often referred
as an essential feature of Lisp,
since Lisp programs are written as S-expressions and they can be manipulated within the Lisp; namely, programs themselves can be controlled as data by other programs in the same language.

HLisp is, similar to other homoiconic languages, designed as a homoiconic Lisp implementation with a macro mechanism, and is implemented as an extension of SECD machine with what the author calls _macro closures_:

* SECD Machine: A virtual machine for functional programming languages (cf. \[Henderson 1980\]\[Hiroi 2009\]);
* Macro closure: A first-class macro mechanism,
the only one novel (but possibly reinvented) feature of HLisp,
which is a kind of function closure for metaprogramming.
For more details, please refer an explanation in `init.lisp`.

The design criteria for HLisp are summarized as follows:

* The language should be self-extensible
* The number of primitives should be few, as much as possible
* The execution efficiency is out of scope, since HLisp is just a conceptual implementation

Thanks to the notion of macro closure, the following features
can be obtained as user-defined programs:

* conditional expression
* let binding
* recursive function definition
* quasi-quotation

This means that a lot of features of HLisp 
can be defined by HLisp itself (or so to speak, by microHLisp).

## To-do list

* Meta issue
  * Add examples
  * Write a documentation
  * Write a paper on the theory and implementation of HLisp
  * Discover untyped/typed lambda-calculi that give a theoretical basis of HLisp
  * Design a type system for HLisp
* Implementation issue
  * Implement an optimization of tail-recursive function calls,
    along the line of TR-SECD machine in \[Ramsdell 1999\]
  * Refactor the codes of parser and garbage collector
  * Add primitive data types (string, floating point numbers, vector, etc.)

## Note
The implementation of HLisp is highly inspired by "minischeme" of Atsushi Moriwaki \[Moriwaki, since 1989\], which is an elegant implementation of a subset of Scheme.

## Reference

1. Toby Davies. Homoiconicity, Lazyness and First-Class Macros. Manuscript, 2009.
\[ [link](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.453.7185) \]
2. Alan C. Kay. The Reactive Engine. Ph.D. dissertation, University of Utah, 1969.
3. John D. Ramsdell. The Tail-Recursive SECD Machine.
  In Journal of Automated Reasoning, Volume 23, Issue 1, pp.43--62, 1999. \[ [link](https://doi.org/10.1023/A:1006151910336) \]
4. Peter Henderson. Functional Programming: Application and Implementation.
  Prentice-Hall International, 1980.
5. Makoto Hiroi. お気楽 Scheme プログラミング入門 (written in Japanese), M.Hiroi's Home Page, 2009. 
\[ [link](http://www.nct9.ne.jp/m_hiroi/func/abcscm33.html) \] (last visited Apr. 18, 2019)
6. Atsushi Moriwaki (with Chris Pressey and Akira Kida). minischeme.
\[ [src](http://tinyscheme.sourceforge.net/minischeme.tar.gz) \] (last visited Apr. 18, 2019)
