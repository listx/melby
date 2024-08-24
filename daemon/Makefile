# See https://stackoverflow.com/a/73509979/437583 for getting the directory of
# this Makefile.
ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
# FIXME: Merge these with the env vars already exported in nix-shell.
export MELBYR_PATH := $(ROOT_DIR)/lib/melbyd/renderer/melbyr
export LUA_PATH := $(ROOT_DIR)/test/sample/?.lua
export MELBY_DIR := $(ROOT_DIR)/test/sample

# Platform. E.g., "Darwin", "Linux".
PLATFORM := $(shell uname)

all:
	mix deps.get
	# Run the command below to install protoc-gen-elixir.
	# mix escript.install hex protobuf
	# FIXME: replace with a make eval to remove the repetition
	protoc --elixir_out=plugins=grpc:lib/generated --proto_path=.. ../melby.proto
	protoc --elixir_out=plugins=grpc:lib/generated --proto_path=.. ../melby_client.proto
	protoc --elixir_out=plugins=grpc:lib/generated --proto_path=.. ../melby_daemon.proto
	protoc --elixir_out=plugins=grpc:lib/generated --proto_path=.. ../melby_renderer.proto
check-deps:
	mix hex.outdated
update-mix:
	mix deps.update --all
# We have to run this after running 'mix deps.update --all' in order to
# sync the dependencies used for building the release version with the
# dependencies used during development.
update-mix.nix: update-mix
	mix2nix > mix.nix
repl: fix-rustler
	iex -S mix
fix-rustler:
	mkdir -p _build/test/lib/melbyd/priv/native
	mkdir -p _build/dev/lib/melbyd/priv/native
	mkdir -p _build/prod/lib/melbyd/priv/native
ifeq ('${PLATFORM}','Darwin')
	ln -sf $(PWD)/lib/melbyd/nifs/target/debug/deps/libmelbyd_nifs.dylib \
		$(PWD)/_build/test/lib/melbyd/priv/native/libmelbyd_nifs.so || true
	ln -sf $(PWD)/lib/melbyd/nifs/target/debug/deps/libmelbyd_nifs.dylib \
		$(PWD)/_build/dev/lib/melbyd/priv/native/libmelbyd_nifs.so || true
	ln -sf $(PWD)/lib/melbyd/nifs/target/debug/deps/libmelbyd_nifs.dylib \
		$(PWD)/_build/prod/lib/melbyd/priv/native/libmelbyd_nifs.so || true
else
	ln -sf $(PWD)/lib/melbyd/nifs/target/debug/deps/libmelbyd_nifs.so  \
		$(PWD)/_build/test/lib/melbyd/priv/native/libmelbyd_nifs.so || true
	ln -sf $(PWD)/lib/melbyd/nifs/target/debug/deps/libmelbyd_nifs.so \
		$(PWD)/_build/dev/lib/melbyd/priv/native/libmelbyd_nifs.so || true
	ln -sf $(PWD)/lib/melbyd/nifs/target/debug/deps/libmelbyd_nifs.so \
		$(PWD)/_build/prod/lib/melbyd/priv/native/libmelbyd_nifs.so || true
endif
PHONY: fix-rustler
compile-prod: fix-rustler
	MIX_ENV=prod mix compile
compile-dev: fix-rustler
	MIX_ENV=dev mix compile
run-dev: compile-dev
	MIX_ENV=dev elixir --sname melbyd-dev -S mix run --no-compile --no-halt
run-prod: compile-prod
	MIX_ENV=prod elixir --sname melbyd-prod -S mix run --no-compile --no-halt
test: fix-rustler
	mix test
.PHONY: test
