;; Copyright 2024 Linus Arver
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Built-in packages (distributed with Emacs).
(require 'tex-mode)
(require 'elisp-mode)

(defun melby-load (path)
    (add-to-list 'load-path (concat
                             (getenv "PROJ_ROOT")
                             (concat "/deps/elisp/"
                                     path))))

;; Third-party packages (checked in as Git submodules)
(melby-load "dash.el")
(require 'dash) ; required by magit

(melby-load "compat.el")
(require 'compat) ; required by magit

(melby-load "haskell-mode")
(require 'haskell-mode)
(require 'haskell-cabal)

(melby-load "magit/lisp")
(require 'magit-section) ; required by nix-mode

(melby-load "nix-mode")
(require 'nix-mode)

(melby-load "rust-mode")
(require 'rust-mode)

(melby-load "emacs-elixir")
(require 'elixir-mode)

(melby-load "lua-mode")
(require 'lua-mode)

(melby-load "protobuf/editors")
(require 'protobuf-mode)

(melby-load "yaml-mode")
(require 'yaml-mode)
