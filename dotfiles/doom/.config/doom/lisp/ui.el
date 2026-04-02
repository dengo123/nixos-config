;;; lisp/ui.el -*- lexical-binding: t; -*-

;; Theme / UI
(setq doom-theme 'doom-one
      display-line-numbers-type 'relative)

;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-symbol-font nil)

(after! doom-themes
  (set-fontset-font t '(#xe0a0 . #xe0d7)
                    (font-spec :family "JetBrainsMono Nerd Font"))
  (set-fontset-font t '(#xf000 . #xf8ff)
                    (font-spec :family "JetBrainsMono Nerd Font")))
