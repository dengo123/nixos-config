;;; lisp/workspaces.el -*- lexical-binding: t; -*-

;; Projekte/Workspaces Basics
(setq projectile-project-search-path '("~/code" "~/projects" "~/nixos-config"))

(setq +workspaces-on-switch-project-behavior t
      +workspaces-auto-save t)

(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t))

(after! persp-mode
  ;; emacsclient -c Frames starten immer in "main"
  (setq persp-emacsclient-init-frame-behaviour-override "main")

  ;; --- main clean policy: was darf in main bleiben? ---
  (defconst my/main-keep-buffer-names
    '("*doom*" "*scratch*" "*Messages*")
    "Buffers that are always allowed in main.")

  (defun my/main-allowed-buffer-p (buf)
    "Return non-nil if BUF is allowed to stay in the 'main' workspace."
    (with-current-buffer buf
      (or (member (buffer-name buf) my/main-keep-buffer-names)
          ;; Terminals ausdrücklich erlauben
          (derived-mode-p 'vterm-mode 'eshell-mode 'term-mode 'shell-mode)
          ;; nicht-Datei buffers (help/transient/etc.) sind ok
          (null buffer-file-name))))

  (defun my/main-persp ()
    "Return the persp object for workspace 'main' if possible."
    (cond ((fboundp '+workspace-get) (+workspace-get "main"))
          ((fboundp 'persp-get-by-name) (persp-get-by-name "main"))
          (t nil)))

  (defun my/prune-main-buffers ()
    "Remove non-allowed buffers from 'main' (do not kill)."
    (when (and (fboundp 'persp-remove-buffer)
               (fboundp 'persp-buffers))
      (when-let ((p (my/main-persp)))
        (dolist (buf (persp-buffers p))
          (when (and (buffer-live-p buf)
                     (not (my/main-allowed-buffer-p buf)))
            (persp-remove-buffer buf p)))))))

(after! projectile
  ;; Wenn du aus main heraus `projectile-switch-project` machst,
  ;; landen gern Projekt-Buffers in main -> danach main aufräumen.
  (defun my/after-projectile-switch-project-clean-main (&rest _)
    (run-at-time 0 nil #'my/prune-main-buffers))

  (advice-add 'projectile-switch-project
              :after #'my/after-projectile-switch-project-clean-main))
