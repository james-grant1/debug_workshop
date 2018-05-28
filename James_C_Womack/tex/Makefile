.PHONY : all read

TEX_FILES := $(wildcard *.tex)
PDF_FILES := $(patsubst %.tex,%.pdf,${TEX_FILES})
CLEAN_EXT := .aux .dvi .log .nav .out .pdf .ps .snm .toc

all: ${PDF_FILES}

%.pdf : %.tex 
	latex $*
# Only add BibTeX if you use the BibTeX referencing system:
#	bibtex ${filename}
#	latex ${filename}
	dvips $*.dvi
	ps2pdf $*.ps

read:
	./read.sh $(filename) &

clean:
	for FILE in ${TEX_FILES}; do \
		 BN=$$(basename $${FILE} .tex) ;\
		 for EXT in ${CLEAN_EXT}; do \
		   rm -vf $${BN}$${EXT}; \
		 done; \
	done

