;;; lisp/navigation.el -*- lexical-binding: t; -*-

(map! :leader :desc "Find file (consult-fd)" "f z" #'consult-fd)

(map! :leader :desc "Toggle treemacs" "t t" #'+treemacs/toggle)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "l") #'dired-find-file))
