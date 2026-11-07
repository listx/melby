
define run_build
	nix-build build.nix --option sandbox relaxed --show-trace \
		-A $(1) --out-link $(1)
endef

build:
	nix-build build.nix --option sandbox relaxed --show-trace

build-melby-client:
	$(call run_build,melby-client)

build-melby-daemon:
	$(call run_build,melby-daemon)

build-melby-nifs:
	$(call run_build,melby-nifs)

shell:
	nix-shell --pure

# Update nix/ folder to pin dependencies to nixos-26.05.
update-nix:
	niv update nixpkgs --branch nixos-26.05

.PHONY: build
.PHONY: build-melby-client
.PHONY: build-melby-daemon
.PHONY: build-melby-nifs
.PHONY: shell
.PHONY: update-nix
