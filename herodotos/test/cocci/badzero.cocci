// A pointer should not be compared to NULL
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/badzero.html
// Options:

@r disable is_zero,isnt_zero @
expression *E;
position p;
@@

(
  E@p == 0
|
  E@p != 0
|
  0 == E@p
|
  0 != E@p
)

@script:python@
p << r.p;
@@

cocci.print_main("",p)

