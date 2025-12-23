;;; lisp/navigation.el -*- lexical-binding: t; -*-

(after! consult
  (map! :leader :desc "Find file (consult-fd)" "f z" #'consult-fd))

(map! :leader :desc "Toggle treemacs" "t t" #'+treemacs/toggle)

(with-eval-after-load 'dired
  (map! :map dired-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file))
