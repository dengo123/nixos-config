;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ── User / UI ─────────────────────────────────────────────────────────────────
(setq user-full-name "Deniz"
      user-mail-address "deniz060198@hotmail.com")

(setq doom-theme 'doom-one
      display-line-numbers-type 'relative)   ; 't oder 'relative

;; Fonts (pass bei Bedarf an)
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      ;; optional, aber hilfreich für spezielle Icons:
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

;; ── Org ──────────────────────────────────────────────────────────────────────
(setq org-directory "~/org/"
      org-agenda-files (list (expand-file-name "agenda.org" org-directory))
      org-ellipsis " ▾ ")

;; ── Projekte / Workspaces ────────────────────────────────────────────────────
;; Suchpfade für Projectile / project.el (je nachdem, was du nutzt)
(setq projectile-project-search-path '("~/code" "~/projects" "~/nixos-config"))
(setq +workspaces-on-switch-project-behavior 'workspaces
      +workspaces-auto-save t)

;; ── Consult-Dir ──────────────────────────────────────────────────────────────
(use-package! consult-dir
  :commands (consult-dir)
  :init
  (map! :leader
        :desc "Jump to dir (consult-dir)"
        "f d" #'consult-dir))

;; ── Treemacs / Dired Convenience ─────────────────────────────────────────────
;; Toggle-Key für Treemacs (falls aktiv)
(map! :leader :desc "Toggle treemacs" "t t" #'+treemacs/toggle)
;; Dired: hjkl-Navigation (optional, falls du das magst)
(with-eval-after-load 'dired
  (map! :map dired-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file))

;; ── LSP / Tree-sitter / Format ───────────────────────────────────────────────
;; Du hast :tools (lsp) + :editor (format +onsave) + :tools tree-sitter
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-lens-enable t
        lsp-inlay-hints-enable t
        lsp-idle-delay 0.25
        lsp-log-io nil))

;; On-save-Formatter (Doom builtin). Wähle, ob per LSP oder externe Tools.
(setq +format-with-lsp t) ; starte simpel: LSP erledigt das Format
;; Begrenze, wo auto-format wirklich laufen soll:
(setq +format-on-save-enabled-modes
      '(lua-mode nix-mode python-mode json-mode yaml-mode sh-mode))

;; Tree-sitter: bevorzugte Major-Modes (abhängig von Doom-Version)
(setq treesit-font-lock-level 4)

;; ── Direnv (für Nix- / devShells) ────────────────────────────────────────────
;; Aktiv: :tools direnv (Modul)
;; Keine Extra-Config nötig, aber falls du Probleme mit env hast:
;; (setq direnv-always-show-summary t)

;; ── Terminals ────────────────────────────────────────────────────────────────
;; vterm Quick-Access
(map! :leader :desc "vterm" "o t" #'vterm)
;; vterm soll sich wie ein normales Terminal verhalten, ohne Evil-Normal-Mode
(with-eval-after-load 'vterm
  ;; alle neuen vterm-Buffer starten im Emacs-State
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; falls du schon in einem vterm bist und es ändern willst:
  (add-hook 'vterm-mode-hook #'evil-emacs-state))

;; ── Completion QoL (corfu + orderless, vertico) ──────────────────────────────
;; Orderless Feinheiten
(with-eval-after-load 'orderless
  ;; mache „space = AND“ passender:
  (setq orderless-component-separator #'orderless-escapable-split-on-space))

;; ── Git / Magit ──────────────────────────────────────────────────────────────
(map! :leader :desc "Magit status" "g g" #'magit-status)

;; ── Spell (du hast :checkers (spell +flyspell) aktiv) ────────────────────────
;; Wähle deine Wörterbuchsprache/n:
;; (setq ispell-dictionary "de_DE") ; oder "en_US"
;; (add-hook 'text-mode-hook #'flyspell-mode)
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; ── Eigene Helfer laden (für projekt-spezifische Minor-Modes etc.) ──────────
;; Lege deine .el-Dateien in $DOOMDIR/lisp/ und aktiviere sie per .dir-locals.el
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

;; Beispiel: Awesome-Auto-Reload wird per .dir-locals.el im Awesome-Projekt aktiviert:
;; ~/.config/awesome/.dir-locals.el:
;; ((lua-mode . ((eval . (progn (require 'awesome-dev)
;;                              (awesome-dev-mode 1))))))

;; ── Kleine Leader-Shortcuts, die du sicher nutzt ─────────────────────────────
(map! :leader
      :desc "Project file"  "p f" #'project-find-file
      :desc "Ripgrep"       "s s" #'consult-ripgrep
      :desc "Switch buffer" "b b" #'switch-to-buffer)

;; ── Optional: Sprache-spezifisches Feintuning ────────────────────────────────
;; Python: black/ruff on-save lieber via LSP? (+format-with-lsp t oben reicht oft)
;; Lua: stylua via LSP-Server, oder extern falls du später Apheleia nutzt.
;; Nix: nixd (oder nil) – du hast (nix +lsp) aktiv.
