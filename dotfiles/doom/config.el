;;; config.el -*- lexical-binding: t; -*-

;; Allgemeines
(setq user-full-name "Deniz"
      user-mail-address "deniz@example.com")

;; Fonts & Theme
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Inter" :size 13))
(setq doom-theme 'doom-one) ; später gern dein “Luna Blue”-Theme nachziehen

;; UI
(setq display-line-numbers-type 'relative) ; 't oder 'relative oder nil
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)

;; Dateien, Backup, Autosave (halt es schlank)
(setq make-backup-files nil
      auto-save-default t
      create-lockfiles nil)

;; Projekt-Root & Suche
(after! projectile
  (setq projectile-project-search-path '("~/code" "~/projects")
        projectile-indexing-method 'alien
        projectile-enable-caching t))

;; LSP
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.3
        lsp-log-io nil))

;; Format-on-save für unterstützte Sprachen
(add-hook 'before-save-hook #'+format/buffer)

;; Org (leichtgewichtig vorbelegt)
(setq org-directory (expand-file-name "~/org/")
      org-agenda-files (list (expand-file-name "agenda.org" org-directory))
      org-ellipsis " ▾ ")

;; Keybindings (Beispiele)
(map! :leader
      :desc "Project find file" "p f" #'projectile-find-file
      :desc "Toggle treemacs"  "t t" #'+treemacs/toggle)

;; Custom file nicht im Store überschreiben
(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file) (load custom-file nil t))

