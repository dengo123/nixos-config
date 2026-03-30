;;; lisp/spell.el -*- lexical-binding: t; -*-

(use-package! jinx
  :hook ((text-mode org-mode markdown-mode) . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
