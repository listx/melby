PROJ_ROOT := $(shell git rev-parse --show-toplevel)
LILAC_ROOT := $(PROJ_ROOT)/deps/elisp/lilac
PROCS := $(shell nproc)
define run_emacs
	LILAC_ROOT=$(LILAC_ROOT) PROJ_ROOT=$(PROJ_ROOT) emacs $(2) \
		--quick --batch --kill \
		--load $(LILAC_ROOT)/lilac.el \
		--load $(PROJ_ROOT)/melby.el \
		--eval="$(1)"
endef

all: tangle weave

all-build-tools: tangle-build-tools weave

fresh-repl:
	make -C $(PROJ_ROOT) -j$(PROCS) tangle
	make -C $(PROJ_ROOT)/daemon repl

tangle-build-tools: build-tools tangle-sources tangle-generated
	# Duplicate some files. This way we avoid symlinking, because symlinks can
	# break when we section off parts of the codebase into separate sandboxes
	# for building things, such as when building Haskell packages.
	cp -f LICENSE client/LICENSE
	cp -f melby_client.proto client/lib/MelbyClient/melby_client.proto
	cp -f melby_client.proto client-rust/proto/melby_client.proto
	cp -f melby_renderer.proto daemon/lib/melbyd/renderer/lib/MelbyRenderer/melby_renderer.proto
.PHONY: tangle-build-tools

# Same as tangle-build-tools, but does not rebuild build tools (ptu).
do-tangle: tangle-sources tangle-generated
	cp -f LICENSE client/LICENSE
	cp -f melby_client.proto client/lib/MelbyClient/melby_client.proto
	cp -f melby_client.proto client-rust/proto/melby_client.proto
	cp -f melby_renderer.proto daemon/lib/melbyd/renderer/lib/MelbyRenderer/melby_renderer.proto
.PHONY: do-tangle

# Currently we don't have any optimizations for tangling, but we still set
# MELBY_LP_QUICK=1 anyway to align with what we do for weave-quick.
tangle:
	MELBY_LP_QUICK=1 make -C $(PROJ_ROOT) -j$(PROCS) do-tangle
.PHONY: tangle

tangle-profile:
	$(call run_emacs,(melby-tangle-profile),developer-manual.org)
.PHONY: tangle-profile

tangle-profile-inspect:
	emacs -Q --load $(LILAC_ROOT)/lilac.el --eval="(profiler-find-profile \"emacs-profile-tangle.txt\"))"
.PHONY: tangle-profile-inspect

tangle-generated:
	# Generate generated source code.
	$(MAKE) -C daemon all
.PHONY: tangle-generated

# Generate source code.
tangle-sources: developer-manual-org \
		user-manual-org \
		image-org

# Sadly, orgmode does not support including files for tangling. This means we
# have to tangle each org file separately, even though they all come together
# into main.org.
image-org: developer-manual.org
	$(call run_emacs,(org-babel-tangle),image.org)
# The developer manual generates the toplevel Makefile (this file) and
# image/Makefile (overwriting them if necessary) to bootstrap the whole
# literate-programming pipeline. Note that these LP-related Makefiles are
# different than the ones used to compile the tangled source code.
developer-manual-org:
	$(call run_emacs,(org-babel-tangle),developer-manual.org)
user-manual-org: developer-manual-org
	$(call run_emacs,(org-babel-tangle),user-manual.org)

.PHONY: tangle-sources
.PHONY: developer-manual-org
.PHONY: user-manual-org
.PHONY: image-org

weave: build-html build-images

weave-quick:
	MELBY_LP_QUICK=1 make -C ${PROJ_ROOT} -j$(PROCS) build-html build-images
.PHONY: weave-quick

weave-profile:
	$(call run_emacs,(lilac-publish-profile),main.org)
.PHONY: weave-profile

weave-profile-inspect:
	emacs -Q --load $(LILAC_ROOT)/lilac.el \
		--eval="(profiler-find-profile \"emacs-profile-weave.txt\"))"
.PHONY: weave-profile-inspect

build-images:
	$(call run_emacs,(org-babel-execute-buffer),image.org)
	$(MAKE) -C image -B

build-html: main.html image.html developer-manual.html user-manual.html

main.html: developer-manual.html image.html main.org
	#$(call run_emacs,(lilac-gen-css-and-exit),main.org)
	$(call run_emacs,(lilac-publish),main.org)

developer-manual.html: developer-manual.org
	$(call run_emacs,(lilac-publish),developer-manual.org)

user-manual.html: user-manual.org developer-manual.html
	$(call run_emacs,(lilac-publish),user-manual.org)

image.html: image.org
	$(call run_emacs,(lilac-publish),image.org)

.PHONY: all all-build-tools weave

# Enter development environment.
shell:
	nix-shell --pure
