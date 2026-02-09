;;; lisp/lsp-format.el -*- lexical-binding: t; -*-

;; -----------------------------
;; LSP – UI & Performance
;; -----------------------------
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-lens-enable t
        lsp-inlay-hints-enable t
        lsp-idle-delay 0.25
        lsp-log-io nil

        ;; IMPORTANT:
        ;; We use external formatters, not LSP formatting
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil))

;; -----------------------------
;; Doom format system
;; -----------------------------
;; Use Doom's formatter framework, not LSP formatting
(setq +format-with-lsp nil)

;; Format on save for selected modes
(setq +format-on-save-enabled-modes
      '(nix-mode
        python-mode
        typescript-mode
        js-mode
        js2-mode
        rjsx-mode
        web-mode
        css-mode
        json-mode
        yaml-mode
        sh-mode))

;; -----------------------------
;; Formatters
;; -----------------------------

(after! format
  ;; -------- Nix --------
  (set-formatter! 'alejandra
    '("alejandra" "--quiet")
    :modes '(nix-mode))

  ;; -------- JavaScript / TypeScript --------
  (set-formatter! 'prettier
    '("prettier" "--stdin-filepath" filepath)
    :modes '(typescript-mode
             js-mode
             js2-mode
             rjsx-mode
             web-mode
             css-mode))

  ;; -------- Python --------
  (set-formatter! 'black
    '("black" "-q" "-")
    :modes '(python-mode)))

;; -----------------------------
;; Tree-sitter
;; -----------------------------
(setq treesit-font-lock-level 4)
