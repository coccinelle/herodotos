@initialize:python@
@@

from coccilib.org import print_todo

@r@
expression E;
position p;
@@

 error(E@p)

@script:python@
e << r.E;
p << r.p;
@@

msg = "I found an error with %s" % (e)
print_todo(p[0], msg)
