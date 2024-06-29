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

define run_build
	nix-build build.nix --option sandbox relaxed --show-trace \
		-A $(1) --out-link $(1)
endef

build:
	nix-build build.nix --option sandbox relaxed --show-trace

build-melby-ptu:
	$(call run_build,melby-ptu)

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
.PHONY: build-melby-ptu
.PHONY: build-melby-client
.PHONY: build-melby-client-rust
.PHONY: build-melby-renderer
.PHONY: build-melby-daemon
.PHONY: build-melby-nifs
.PHONY: shell
.PHONY: update-nix
