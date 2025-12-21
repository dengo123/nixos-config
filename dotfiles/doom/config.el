;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ── User / UI ─────────────────────────────────────────────────────────────────
(setq user-full-name "Deniz"
      user-mail-address "deniz060198@hotmail.com")

(setq doom-theme 'doom-one
      display-line-numbers-type 'relative)   ; 't oder 'relative

;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      ;; optional, aber hilfreich für spezielle Icons:
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

;; ── Org ──────────────────────────────────────────────────────────────────────
(after! org
  ;; Startzustand
  (setq org-directory "~/org"
        org-startup-folded 'content   ; nur Überschriften sichtbar (Text eingeklappt)
        org-startup-indented t        ; visuelles Einrücken nach Überschrift-Ebene
        org-ellipsis " ▾ "            ; hübscher Fold-Indikator
        org-hide-emphasis-markers t   ; *bold* zeigt nur bold, nicht die Sternchen
        org-pretty-entities t)        ; z.B. \alpha schöner anzeigen

  ;; besserer Lesefluss
  (add-hook 'org-mode-hook #'visual-line-mode))

;; ── Projekte / Workspaces ────────────────────────────────────────────────────
(setq projectile-project-search-path '("~/code" "~/projects" "~/nixos-config"))

;; Projektwechsel => eigener Workspace, Workspaces speichern
(setq +workspaces-on-switch-project-behavior t
      +workspaces-auto-save t)

;; Workspace-Name dauerhaft in der Modeline (inkl. "main"/Default)
(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t))

(after! persp-mode
  ;; emacsclient -c soll keinen neuen Workspace machen, sondern immer "main"
  (setq persp-emacsclient-init-frame-behaviour-override "main")

  ;; "main" neutral halten: beim Wechsel in main -> ~ als cwd
  (defun my/workspace-main-set-home-dir ()
    (when (and (fboundp '+workspace-current-name)
               (string= (+workspace-current-name) "main"))
      (setq default-directory (expand-file-name "~"))))
  (add-hook 'persp-switch-hook #'my/workspace-main-set-home-dir)

  ;; --- main-default-directory beim switch-project nicht "verschmutzen" ---
  (defvar my/main-workspace-dir-cache nil)

  (defun my/workspace--buffers ()
    "Return buffers belonging to current workspace if possible, else all buffers."
    (cond ((fboundp '+workspace-buffer-list) (+workspace-buffer-list))
          ((fboundp '+workspace-list-buffers) (+workspace-list-buffers))
          (t (buffer-list))))

  (defun my/cache-main-workspace-dirs ()
    (when (and (fboundp '+workspace-current-name)
               (string= (+workspace-current-name) "main"))
      (setq my/main-workspace-dir-cache
            (mapcar (lambda (b)
                      (cons b (buffer-local-value 'default-directory b)))
                    (my/workspace--buffers)))))

  (defun my/restore-main-workspace-dirs ()
    (when my/main-workspace-dir-cache
      (dolist (pair my/main-workspace-dir-cache)
        (when (buffer-live-p (car pair))
          (with-current-buffer (car pair)
            (setq default-directory (cdr pair)))))
      (setq my/main-workspace-dir-cache nil)))

  (defun my/preserve-main-dirs-around-switch-project (orig-fn &rest args)
    (my/cache-main-workspace-dirs)
    (prog1 (apply orig-fn args)
      ;; nach dem Projektwechsel main wieder herstellen
      (my/restore-main-workspace-dirs)))

  ;; Doom/Projectile entrypoints abfangen
  (when (fboundp '+projectile/switch-project)
    (advice-add '+projectile/switch-project :around #'my/preserve-main-dirs-around-switch-project))
  (when (fboundp 'projectile-switch-project)
    (advice-add 'projectile-switch-project :around #'my/preserve-main-dirs-around-switch-project)))

;; ── Projectile ───────────────────────────────────────────────────────────────
(after! projectile
  (setq projectile-indexing-method 'hybrid)

  (defun my/projectile-buffers-same-root (buffers)
    "Keep only buffers whose *own* Projectile root equals the current Projectile root."
    (let* ((cur-root (when (projectile-project-p)
                       (file-truename (projectile-project-root)))))
      (seq-filter
       (lambda (buf)
         (with-current-buffer buf
           (let* ((f (buffer-file-name))
                  (dir (or (and f (file-name-directory f))
                           default-directory))
                  (buf-root (ignore-errors
                              (let ((default-directory dir))
                                (projectile-project-root)))))
             (and cur-root buf-root
                  (string= (file-truename buf-root) cur-root)))))
       (projectile-buffers-with-file-or-process buffers))))

  (setq projectile-buffers-filter-function #'my/projectile-buffers-same-root)

  (defun my/projectile-kill-buffer ()
    "Kill one buffer belonging to the current Projectile project."
    (interactive)
    (let* ((bufs (projectile-project-buffers))
           (names (mapcar #'buffer-name bufs))
           (choice (completing-read "Kill project buffer: " names nil t)))
      (when-let ((b (get-buffer choice)))
        (kill-buffer b)
        (message "Killed: %s" choice))))

  (map! :leader
        :desc "Kill project buffer" "p K" #'my/projectile-kill-buffer))

;; ── Tabs ─────────────────────────────────────────────────────────────────────
;; (map! :leader
;;       :prefix ("TAB" . "tabs")
;;       :desc "New tab"     "n" #'tab-bar-new-tab
;;       :desc "Close tab"   "c" #'tab-bar-close-tab
;;       :desc "Next tab"    "]" #'tab-bar-switch-to-next-tab
;;       :desc "Prev tab"    "[" #'tab-bar-switch-to-prev-tab
;;       :desc "Rename tab"  "r" #'tab-bar-rename-tab
;;       :desc "Switch tab"  "s" #'tab-bar-switch-to-tab)

;; ── consult-fd ───────────────────────────────────────────────────────────────
(after! consult
  (map! :leader :desc "Find file (consult-fd)" "f z" #'consult-fd))

;; ── Treemacs / Dired Convenience ─────────────────────────────────────────────
;; Toggle-Key für Treemacs
(map! :leader :desc "Toggle treemacs" "t t" #'+treemacs/toggle)
;; Dired: hjkl-Navigation
(with-eval-after-load 'dired
  (map! :map dired-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file))

;; ── LSP / Tree-sitter / Format ───────────────────────────────────────────────
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-lens-enable t
        lsp-inlay-hints-enable t
        lsp-idle-delay 0.25
        lsp-log-io nil))

;; On-save-Formatter (Doom builtin). Wähle, ob per LSP oder externe Tools.
(setq +format-with-lsp t)
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
(with-eval-after-load 'vterm
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook #'evil-emacs-state))

;; ── Completion QoL (corfu + orderless, vertico) ──────────────────────────────
;; Orderless Feinheiten
(with-eval-after-load 'orderless
  (setq orderless-component-separator #'orderless-escapable-split-on-space))

;; ── Git / Magit ──────────────────────────────────────────────────────────────
(map! :leader :desc "Magit status" "g g" #'magit-status)

;; ── Spell ────────────────────────────────────────────────────────────────────
(use-package! jinx
  :hook ((text-mode org-mode markdown-mode) . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; ── Eigene Helfer laden (für projekt-spezifische Minor-Modes etc.) ──────────
;; Lege deine .el-Dateien in $DOOMDIR/lisp/ und aktiviere sie per .dir-locals.el
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))

;; Beispiel: Awesome-Auto-Reload wird per .dir-locals.el im Awesome-Projekt aktiviert:
;; ~/.config/awesome/.dir-locals.el:
;; ((lua-mode . ((eval . (progn (require 'awesome-dev)
;;                              (awesome-dev-mode 1))))))

;; ── Optional: Sprache-spezifisches Feintuning ────────────────────────────────
;; Python: black/ruff on-save lieber via LSP? (+format-with-lsp t oben reicht oft)
;; Lua: stylua via LSP-Server, oder extern falls du später Apheleia nutzt.
;; Nix: nixd (oder nil) – du hast (nix +lsp) aktiv.
