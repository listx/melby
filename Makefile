# Copyright 2024 Linus Arver
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
	# Prepend copyright headers.
	#
	# FIXME: Use something like "The Melby Authors" or something like that? See
	# https://opensource.google/documentation/reference/releasing/authors
	#
	# FIXME: Before tangling, run this utility to check what copyright years
	# were used for a file (and write this information to disk somewhere). Then
	# after tangling (when all such copyrights are stripped), when we run
	# "add-legal", check if the file we're prepending the copyright to has an
	# entry in the file written to disk. If so, use the year in that entry.
	# Otherwise, use the current year. This is so that we only use the year when
	# the file is first "published" (came into existence).
	./ptu add-legal $(shell git rev-parse --show-toplevel) "Linus Arver"
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
	./ptu add-legal $(shell git rev-parse --show-toplevel) "Linus Arver"
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
	$(call run_emacs,(melby-tangle-profile),build-literate.org)
.PHONY: tangle-profile

tangle-profile-inspect:
	emacs -Q --load $(LILAC_ROOT)/lilac.el --eval="(profiler-find-profile \"emacs-profile-tangle.txt\"))"
.PHONY: tangle-profile-inspect

tangle-generated:
	# Generate generated source code.
	$(MAKE) -C daemon all
.PHONY: tangle-generated

build-literate-org:
	# Generate the toplevel Makefile (this file) and image/Makefile (overwriting
	# them if necessary). In a way this bootstraps the whole
	# literate-programming pipeline. Note that these files are different than
	# the ones used to compile the tangled source code.
	$(call run_emacs,(org-babel-tangle),build-literate.org)

# Generate source code.
tangle-sources: client-org \
		client-rust-org \
		daemon-org \
		controller-org \
		model-org \
		view-org \
		user-manual-org \
		image-org \
		build-packaging-org

# Sadly, orgmode does not support including files for tangling. This means we
# have to tangle each org file separately, even though they all come together
# into main.org.
build-packaging-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),build-packaging.org)
image-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),image.org)
client-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),client.org)
client-rust-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),client-rust.org)
daemon-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),daemon.org)
controller-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),controller.org)
model-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),model.org)
view-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),view.org)
user-manual-org: build-literate-org
	$(call run_emacs,(org-babel-tangle),user-manual.org)

.PHONY: build-literate-org
.PHONY: build-packaging-org
.PHONY: tangle-sources
.PHONY: client-org
.PHONY: client-rust-org
.PHONY: daemon-org
.PHONY: controller-org
.PHONY: model-org
.PHONY: view-org
.PHONY: user-manual-org
.PHONY: image-org

build-tools: ptu

# FIXME: NOTE: This stack invocation is for environments without Nix. But also,
# it's useful during development because invoking stack directly like this
# results in populating the build cache. In our (FIXME: link to build-melby-ptu
# rule), when we invoke that we don't get any caching.
UNAME := $(shell uname)
C_INCLUDE_PATH := ""
ifeq ($(UNAME), Darwin)
	# The `xcrun...` stuff here is from
	# https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266.
	C_INCLUDE_PATH := C_INCLUDE_PATH=$(shell xcrun --show-sdk-path)/usr/include/ffi
endif
ptu: ptu.cabal ptu.hs stack.yaml lib/PostTangleUtil/GitVersion.hs
	$(C_INCLUDE_PATH) \
	PROJECT_GIT_ROOT=$(PWD) stack build \
		--copy-bins \
		--local-bin-path $(PWD) \
		--no-nix-pure \
		--extra-lib-dirs=$(ZLIB_SO_DIR:-L%=%) \
		--extra-include-dirs=$(ZLIB_H_DIR:-I%=%)
.PHONY: ptu

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

developer-manual.html: developer-manual.org daemon.org model.org view.org controller.org build-literate.org build-packaging.org
	$(call run_emacs,(lilac-publish),developer-manual.org)

user-manual.html: user-manual.org developer-manual.html
	$(call run_emacs,(lilac-publish),user-manual.org)

image.html: image.org
	$(call run_emacs,(lilac-publish),image.org)

.PHONY: all all-build-tools weave

# Enter development environment.
shell:
	nix-shell --pure
