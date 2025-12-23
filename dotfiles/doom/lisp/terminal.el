;;; lisp/terminal.el -*- lexical-binding: t; -*-

(map! :leader :desc "vterm" "o t" #'vterm)

(with-eval-after-load 'vterm
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook #'evil-emacs-state))
