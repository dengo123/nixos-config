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
  ;; Helper: project root per workspace speichern
  ;; ------------------------------

  (defun my/workspace-set-project-root ()
    "Attach projectile root to current workspace."
    (when-let ((root (projectile-project-root)))
      (persp-set-parameter 'project-root root (get-current-persp))))

  (add-hook 'projectile-after-switch-project-hook
            #'my/workspace-set-project-root)

;; ───────────────────────────────────────────────────────────────────────────
;; Allow Policy (HARD ISOLATION) + prune on workspace switch
;; ───────────────────────────────────────────────────────────────────────────

(after! persp-mode
  (defconst my/workspace-global-buffers
    '("*doom*" "*scratch*" "*Messages*" "*Warnings*")
    "Buffers allowed in all workspaces.")

  (defun my/workspace-set-project-root ()
    "Attach projectile root to current workspace."
    (when-let ((root (projectile-project-root)))
      (persp-set-parameter 'project-root root (get-current-persp))))

  (add-hook 'projectile-after-switch-project-hook
            #'my/workspace-set-project-root)

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

           ;; vterm popups: only the one for this workspace
           ((string-prefix-p "*vterm-popup:" name)
            (string= name (my/vterm-popup-buffer-for ws)))

           ;; vterm "here": only the one for this workspace
           ((string-prefix-p "*vterm:" name)
            (string= name (my/vterm-buffer-for ws)))

           ;; block generic vterm buffers (*vterm*, *vterm<2>*, ...)
           ((derived-mode-p 'vterm-mode)
            nil)

           ;; main: allow only non-file buffers
           ((string= ws "main")
            (null buffer-file-name))

           ;; project workspace: files must live under the workspace root
           (root
            (and buffer-file-name
                 (string-prefix-p (file-truename root)
                                  (file-truename buffer-file-name))))

           ;; fallback: non-file buffers only
           (t
            (null buffer-file-name)))))))

  (defun my/workspace-prune-current ()
    "Remove non-allowed buffers from current workspace (do not kill)."
    (let ((persp (get-current-persp)))
      (dolist (buf (persp-buffers persp))
        (unless (my/workspace-allowed-buffer-p buf persp)
          (persp-remove-buffer buf persp)))))

  (add-hook 'persp-activated-hook #'my/workspace-prune-current))
