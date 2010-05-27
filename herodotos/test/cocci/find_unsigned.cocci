// A variable that is declared as unsigned should not be tested to be less than
// zero.
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/find_unsigned.html
// Options: -all_includes

@u@ type T; unsigned T i; position p; @@

 i@p < 0

@script:python@
p << u.p;
@@

cocci.print_main("",p)

