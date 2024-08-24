all: architecture-overall.svg
%.pdf : %.tex
	lualatex --halt-on-error $<
%.svg : %.pdf
	pdf2svg $< $@.uncropped.svg
	inkscape \
		--export-plain-svg \
		--export-margin=5 \
		--export-filename=$@ \
		--export-area-drawing \
		$@.uncropped.svg

.PHONY: all
