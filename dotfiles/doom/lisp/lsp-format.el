;;; lisp/lsp-format.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-lens-enable t
        lsp-inlay-hints-enable t
        lsp-idle-delay 0.25
        lsp-log-io nil))

(setq +format-with-lsp t)

(setq +format-on-save-enabled-modes
      '(lua-mode nix-mode python-mode json-mode yaml-mode sh-mode typescript-mode css-mode mhtml-mode web-mode))

(setq treesit-font-lock-level 4)
