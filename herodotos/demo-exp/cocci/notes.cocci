
@r@
identifier F;
position p1;
@@

 F@p1(...)

@script:python@
f << r.F;
p1 << r.p1;
@@

coccilib.org.print_todo(p1[0],f)