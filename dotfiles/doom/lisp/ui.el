;;; lisp/ui.el -*- lexical-binding: t; -*-

;; Theme / UI
(setq doom-theme 'doom-one
      display-line-numbers-type 'relative)

;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

;; Buffer names
(require 'uniquify)

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")
