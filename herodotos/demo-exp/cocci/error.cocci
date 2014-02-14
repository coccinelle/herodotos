
@r@
expression E;
position p1,p2;
@@

 error@p1(E@p2)

@script:python@
e << r.E;
p1 << r.p1;
p2 << r.p2;
@@

coccilib.org.print_todo(p1[0],"I found an error")
coccilib.org.print_link(p2[0],e, "ovl-face2")