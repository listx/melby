define run_build
	nix-build build.nix --option sandbox relaxed --show-trace \
		-A $(1) --out-link $(1)
endef

build:
	nix-build build.nix --option sandbox relaxed --show-trace

build-melby-client:
	$(call run_build,melby-client)

build-melby-client-rust:
	$(call run_build,melby-client-rust)

build-melby-daemon:
	$(call run_build,melby-daemon)

build-melby-nifs:
	$(call run_build,melby-nifs)

build-melby-renderer:
	$(call run_build,melby-renderer)

shell:
	nix-shell --pure

# Update nix/ folder to pin dependencies to nixos-23.11.
update-nix:
	niv update nixpkgs --branch nixos-23.11

.PHONY: build
.PHONY: build-melby-client
.PHONY: build-melby-client-rust
.PHONY: build-melby-renderer
.PHONY: build-melby-daemon
.PHONY: build-melby-nifs
.PHONY: shell
.PHONY: update-nix
