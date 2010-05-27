// A variable is only initialized to a constant and is never used otherwise
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/unused.html
// Options:

@e@
identifier i;
position p;
type T;
@@

extern T i@p;

@v@
type T;
identifier i;
constant C;
position p1 != e.p;
position p2;
@@

T i@p1;
  <+... when != i
i@p2 = C;
  ...+>

@script:python@
p1 << v.p1;
p2 << v.p2;
@@

cocci.print_main("",p1)
cocci.print_secs("",p2)
