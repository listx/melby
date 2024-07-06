ZLIB_SO_DIR=$(shell pkg-config --libs-only-L zlib)
ZLIB_H_DIR=$(shell pkg-config --cflags-only-I zlib)

all: melbyr

melbyr:
	MELBY_PROJECT_ROOT=$(PWD) stack build \
		--copy-bins \
		--local-bin-path $(PWD) \
		--no-nix-pure \
		--extra-lib-dirs=$(ZLIB_SO_DIR:-L%=%) \
		--extra-include-dirs=$(ZLIB_H_DIR:-I%=%)
.PHONY: melbyr

ghci:
	stack exec -- ghci
.PHONY: ghci

# This avoids building the main executable on top of the base library and test
# executable. For various reasons, Stack always builds the main executable
# binary. See https://github.com/commercialhaskell/stack/issues/5647#issuecomment-1106796448.
test:
	stack build melby-renderer:test
.PHONY: test
