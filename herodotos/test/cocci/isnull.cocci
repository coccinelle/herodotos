// Dereference of an expression that has been checked to be NULL
//
// Confidence: Moderate
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/isnull.html
// Options:

@r exists@
expression E, E1;
identifier f;
statement S1,S2,S3;
position p;
iterator iter;
@@

if (E == NULL)
{
  ... when != if (E == NULL && ...) S1 else S2
      when != if (E == NULL || ...) S1 else S2
      when != iter(E,...) S1
      when != E = E1
  E@p->f
  ... when any
  return ...;
}
else S3

@script:python@
p << r.p;
@@

cocci.print_main("",p)
