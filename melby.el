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

(setq org-latex-compiler "lualatex")
