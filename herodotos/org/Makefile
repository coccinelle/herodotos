TARGET = org

LEXER_SOURCES = org_lexer.mll
PARSER_SOURCES = org_parser.mly
SOURCES = org_helper.ml $(PARSER_SOURCES:.mly=.ml) $(LEXER_SOURCES:.mll=.ml) \
	  org.ml bugs.ml orgfilter.ml
EXTLIBS=parmap volt
P4=volt
TOPLIB=commons diff

# Automatic computation
OCAMLFIND=ocamlfind

-include ../Makefile.config

GENERATED= $(LEXER_SOURCES:.mll=.ml) \
	   $(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli)
LIBDIR=$(shell $(OCAMLFIND) query $(EXTLIBS))
P4DIR=$(shell $(OCAMLFIND) query $(P4))
P4FLAG=-pp 'camlp4o $(P4DIR)/bolt_pp.cmo'
INCLUDES =$(TOPLIB:%=-I ../%) $(LIBDIR:%=-I %)
OBJS = $(SOURCES:.ml=.cmo)
LIB=$(TARGET).cma
OPTOBJS = $(OBJS:.cmo=.cmx)
OPTLIB=$(LIB:.cma=.cmxa)

MENHIR=$(MENHIR_PATH)/menhirLib.cmo
MENHIRO=$(MENHIR_PATH)/menhirLib.cmx

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes $(FLAGS)
OCAMLC =$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(INCLUDES) $(P4FLAG)
OCAMLOPT =$(OCAMLFIND) ocamlopt $(OPTFLAGS) $(INCLUDES) $(P4FLAG)
OCAMLLEX = ocamllex$(OPTBIN)
OCAMLYACC= menhir $(OCAMLYACCOPT)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES) $(P4FLAG)


all: $(LIB)
all.opt: $(OPTLIB)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: depend clean

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(LEXER_SOURCES:.mll=.ml) :	$(LEXER_SOURCES)
	$(OCAMLLEX) $(LEXER_SOURCES)

$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	$(OCAMLYACC) $(PARSER_SOURCES)

$(LIB): $(GENERATED) $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(GENERATED) $(OPTOBJS)
	$(OCAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

clean::
	rm -f $(TARGET)
	rm -f $(GENERATED)
	rm -f *.cm[aiox] *.cmxa *.a *.o *.annot *.conflicts
	rm -f *~ .*~

distclean: clean
	rm -f .depend

.depend depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
