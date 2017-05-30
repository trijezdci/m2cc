# m2cc

Modula-2 Core Compiler

M2CC is a self hosting Modula-2 compiler.

The compiler implements a subset of the revised language by B.Kowarsch and R.Sutcliffe "Modula-2 Revision 2010" (M2R10). It is written in the extended Modula-2 dialect of the M2C/M2J/M2Sharp compiler suite and may therefore be bootstrapped using any of these compilers. 

In translator mode, M2CC translates Modula-2 source to C99 source files. In compiler mode, M2CC compiles Modula-2 source via C99 source files to object code or executables using the host system's C99 compiler. An LLVM back-end will be added later.

For more details please visit the project wiki at the URL: https://github.com/trijezdci/m2cc/wiki
