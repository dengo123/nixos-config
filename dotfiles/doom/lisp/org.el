;;; lisp/org.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/org"
        org-startup-folded 'content
        org-startup-indented t
        org-ellipsis " ▾ "
        org-hide-emphasis-markers t
        org-pretty-entities t)
  (add-hook 'org-mode-hook #'visual-line-mode))
