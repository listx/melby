all: architecture-overall.svg
%.pdf: %.tex
	lualatex --halt-on-error $<
%.svg: %.pdf
	dvisvgm --no-fonts --pdf $<

.PHONY: all
