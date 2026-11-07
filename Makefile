PROJ_ROOT := $(shell git rev-parse --show-toplevel)
PROCS := $(shell nproc)

org_files := index.org developer-manual.org user-manual.org image.org plan.org
obj_files := $(org_files:%.org=%.lobj)
archive_file := melby.larc

all: tangle weave lint

fresh-repl:
	make -C $(PROJ_ROOT) -j$(PROCS) tangle
	make -C $(PROJ_ROOT)/daemon repl

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files)

$(archive_file): $(obj_files)
	lilac archive $^ --out-file $@

do-tangle: tangle-sources tangle-generated
.PHONY: do-tangle

tangle:
	make -C $(PROJ_ROOT) -j$(PROCS) do-tangle
.PHONY: tangle

tangle-generated:
	# Generate generated source code.
	$(MAKE) -C daemon all
.PHONY: tangle-generated

# Generate source code. This also generates the toplevel Makefile (this file)
# and image/Makefile (overwriting them if necessary) to bootstrap the whole
# literate-programming pipeline. Note that these LP-related Makefiles are
# different than the ones used to compile the tangled source code.
tangle-sources: $(archive_file)
	lilac tangle $< --out-dir .
.PHONY: tangle-sources

weave: build-html

build-images: tangle-sources
	$(MAKE) -C image

build-html: $(archive_file) build-images
	rm -rf site/image
	lilac weave $< --out-dir site --write-css --write-js
	mkdir -p site/image
	cp image/*.svg site/image

lint: $(archive_file)
	lilac --project-conf Lilac.toml lint $<

.PHONY: all weave build-images build-html lint

# Enter development environment.
shell:
	nix-shell --pure
