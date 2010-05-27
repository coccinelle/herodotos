// !x&y combines boolean negation with bitwise and
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/notand.html
// Options: -macro_file_builtins /var/storage/smatch/notand.h

@r disable unlikely,likely @
expression E;
constant C;
position p;
@@

(
  !E & !C
|
  !@p E & C
)

@script:python@
p << r.p;
@@

cocci.print_main("",p)
