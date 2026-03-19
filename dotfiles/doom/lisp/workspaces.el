;;; lisp/workspaces.el -*- lexical-binding: t; -*-

;; Projekte/Workspaces Basics
(setq projectile-project-search-path '("~/org" "~/projects" "~/nixos-config"))

(setq +workspaces-on-switch-project-behavior t
      +workspaces-auto-save t)

(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-display-default-persp-name t
        doom-modeline-persp-icon t))

(after! persp-mode
  ;; emacsclient -c Frames starten immer in "main"
  (setq persp-emacsclient-init-frame-behaviour-override "main")

  ;; ------------------------------
  ;; Global Allowlist
  ;; ------------------------------

  (defconst my/workspace-global-buffers
    '("*doom*" "*scratch*" "*Messages*" "*Warnings*")
    "Buffers allowed in all workspaces.")

  ;; ------------------------------
  ;; Helpers
  ;; ------------------------------

  (defun my/ws-safe (s)
    "Make S safe to use in buffer names."
    (replace-regexp-in-string
     "[^[:alnum:]._+-]" "_" (or s "main")))

  (defun my/workspace-vterm-popup-name (ws)
    "Return popup vterm buffer name for workspace WS."
    (format "*vterm-popup:%s*" (my/ws-safe ws)))

  (defun my/workspace-vterm-name (ws)
    "Return full vterm buffer name for workspace WS."
    (format "*vterm:%s*" (my/ws-safe ws)))

  ;; ------------------------------
  ;; Project root per workspace speichern
  ;; ------------------------------

  (defun my/workspace-set-project-root ()
    "Attach projectile root to current workspace."
    (when-let ((root (projectile-project-root)))
      (persp-set-parameter 'project-root root (get-current-persp))))

  (add-hook 'projectile-after-switch-project-hook
            #'my/workspace-set-project-root)

  ;; ------------------------------
  ;; Allow Policy (HARD ISOLATION)
  ;; ------------------------------

  (defun my/workspace-allowed-buffer-p (buf persp)
    "Return non-nil if BUF is allowed inside PERSP."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((name (buffer-name buf))
               (ws   (safe-persp-name persp))
               (root (persp-parameter 'project-root persp)))
          (cond
           ;; Always allowed
           ((member name my/workspace-global-buffers) t)

           ;; vterm popup: only the one for this workspace
           ((string-prefix-p "*vterm-popup:" name)
            (string= name (my/workspace-vterm-popup-name ws)))

           ;; full vterm: only the one for this workspace
           ((string-prefix-p "*vterm:" name)
            (or (string= name (my/workspace-vterm-name ws))
                (string-prefix-p
                 (format "*vterm:%s:" (my/ws-safe ws))
                 name)))

           ;; generic unnamed vterms are never allowed
           ((derived-mode-p 'vterm-mode)
            nil)

           ;; main: only non-file buffers
           ((string= ws "main")
            (null buffer-file-name))

           ;; project workspace: file must be inside project root
           (root
            (and buffer-file-name
                 (string-prefix-p (file-truename root)
                                  (file-truename buffer-file-name))))

           ;; fallback: non-file buffers only
           (t
            (null buffer-file-name)))))))

  (defun my/workspace-prune-current ()
    "Remove non-allowed buffers from current workspace."
    (let ((persp (get-current-persp)))
      (dolist (buf (persp-buffers persp))
        (unless (my/workspace-allowed-buffer-p buf persp)
          (persp-remove-buffer buf persp)))))

  (add-hook 'persp-activated-hook #'my/workspace-prune-current))
