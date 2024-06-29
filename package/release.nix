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

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  melby = import ./build.nix;
in
  pkgs.symlinkJoin {
    name = "melby";
    paths = with melby; [
      melby-client
      melby-client-rust
      melby-renderer
      melby-daemon
    ];
    # If users have already installed individual packages already, make the ones
    # we bundle here act as a fallback. Default priority is 5, and so by setting
    # it to 10, makes Nix treat the files of this derivation as a fallback if
    # there is a conflict.
    meta.priority = 10;
  }
