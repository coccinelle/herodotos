MAIN=herodotos howto-herodotos
TEX=$(MAIN:%=%.tex)
PDF=$(MAIN:%=%.pdf)
HTML=$(MAIN:%=%.html)
WEB=~npalix/Documents/Under\ Git/Website/herodotos/docs

all: pdf html

install: all
	hacha -o index.html herodotos.html
	mv *.html *.css *.pdf *.gif $(WEB)

clean:
	rm -f *.log *.aux *.out *.toc *~
	rm -f *.html *.haux *.htoc

distclean: clean
	rm -f $(PDF)

##############################################

macros: macros.tex macros_grammar.tex macros_common.tex
herodotos.tex: macros foreword.tex \
	install.tex howto.tex syntax.tex examples.tex
howto-herodotos.tex: macros howto.tex

##############################################

.SUFFIXES: .tex .pdf .html

pdf: $(PDF)
html: $(HTML)

.tex.pdf:
	pdflatex $<
	pdflatex $<

.tex.html:
# For Gentoo: /usr/lib64/hevea
# For Ubuntu family: /usr/share/hevea
# For default installation
	@if [ -f /usr/lib64/hevea/xxdate.exe ]; then \
	hevea -exec /usr/lib64/hevea/xxdate.exe  $< ; \
	hevea -exec /usr/lib64/hevea/xxdate.exe  $< ; \
	elif [ -f /usr/share/hevea/xxdate.exe ]; then \
	hevea -exec /usr/share/hevea/xxdate.exe $< ; \
	hevea -exec /usr/share/hevea/xxdate.exe $< ; \
	elif [ -f /usr/local/lib/hevea/xxdate.exe ]; then \
	hevea -exec /usr/local/lib/hevea/xxdate.exe $< ; \
	hevea -exec /usr/local/lib/hevea/xxdate.exe $< ; \
	else \
	echo -e "\n\n *** Hevea: Unable to find xxdate.exe ***\n\n"; \
	fi
#	hacha -o index-$@ $@
