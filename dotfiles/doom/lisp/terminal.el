;;; lisp/terminal.el -*- lexical-binding: t; -*-

(after! vterm
  ;; vterm in emacs-state (Terminal-Apps nicht von evil stören)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook #'evil-emacs-state)

  ;; ───────────────────────────────────────────────────────────────────────────
  ;; vterm pro Workspace (Namen + "here"/popup helpers)
  ;; ───────────────────────────────────────────────────────────────────────────

  (after! vterm
    (defun my/ws-safe (s)
      "Make S safe to use in buffer names."
      (replace-regexp-in-string
       "[^[:alnum:]._+-]" "_" (or s "main")))

    (defun my/ws-name (&optional frame)
      "Return Doom workspace name for FRAME (defaults to selected frame)."
      (let ((frame (or frame (selected-frame))))
        (with-selected-frame frame
          (if (fboundp '+workspace-current-name)
              (+workspace-current-name)
            "main"))))

    (defun my/vterm-popup-buffer-for (ws)
      (format "*vterm-popup:%s*" (my/ws-safe ws)))

    (defun my/vterm-buffer-for (ws)
      (format "*vterm:%s*" (my/ws-safe ws)))

    (defun my/vterm-popup-buffer-name (&optional frame)
      "Popup vterm buffer name for FRAME's workspace."
      (my/vterm-popup-buffer-for (my/ws-name frame)))

    (defun my/vterm--window-showing (buf &optional frame)
      "Return a window displaying BUF in FRAME (defaults to selected frame), else nil."
      (get-buffer-window buf (or frame (selected-frame))))

    (defun my/vterm-popup-toggle ()
      "Toggle a dedicated vterm popup for the current Doom workspace."
      (interactive)
      (let* ((frame (selected-frame))
             (bufname (my/vterm-popup-buffer-name frame))
             (buf (get-buffer bufname))
             (win (and buf (my/vterm--window-showing buf frame))))
        (cond
         (win
          (delete-window win))

         ((buffer-live-p buf)
          (pop-to-buffer
           buf
           '((display-buffer-reuse-window display-buffer-at-bottom))))

         (t
          (let* ((vterm-buffer-name-string (substring bufname 1 -1))
                 (b (with-selected-frame frame (vterm))))
            (with-current-buffer b
              (rename-buffer bufname t))
            (pop-to-buffer
             (get-buffer bufname)
             '((display-buffer-reuse-window display-buffer-at-bottom))))))))

    (defun my/vterm-here ()
      "Open (or switch to) a workspace-specific, non-popup vterm."
      (interactive)
      (let* ((ws (my/ws-name))
             (bufname (my/vterm-buffer-for ws))
             (buf (get-buffer bufname)))
        (if (buffer-live-p buf)
            (pop-to-buffer buf)
          (let* ((vterm-buffer-name-string (substring bufname 1 -1))
                 (b (vterm)))
            (with-current-buffer b
              (rename-buffer bufname t))
            (pop-to-buffer (get-buffer bufname))))))

    ;; Optional: bind Doom-style keys to your workspace-specific vterms
    (map! :leader
          :desc "vterm popup (per-workspace)" "o t" #'my/vterm-popup-toggle
          :desc "vterm here (per-workspace)"  "o T" #'my/vterm-here))

  ;; ───────────────────────────────────────────────────────────────────────────
  ;; "SPC TAB …" Ersatz in vterm: C-c TAB …
  ;; (inkl. Nummern 1..9, exakt wie Doom gebunden hat)
  ;; ───────────────────────────────────────────────────────────────────────────

  (defvar my/vterm-workspace-map (make-sparse-keymap)
    "Workspace prefix map for vterm (C-c TAB …).")

  (define-key vterm-mode-map (kbd "C-c TAB") my/vterm-workspace-map)

  (defun my/doom-spc-tab-cmd (keyseq)
    "Return the command bound to Doom leader `TAB KEYSEQ'."
    (let ((cmd (lookup-key doom-leader-map (kbd (concat "TAB " keyseq)))))
      (when (commandp cmd) cmd)))

  ;; Standard workspace keys
  (dolist (k '("TAB" "l" "n" "r" "d" "[" "]" "s"))
    (when-let ((cmd (my/doom-spc-tab-cmd k)))
      (define-key my/vterm-workspace-map (kbd k) cmd)))

  ;; Nummern 1..9
  (dotimes (i 9)
    (let* ((n (number-to-string (1+ i)))
           (cmd (my/doom-spc-tab-cmd n)))
      (when cmd
        (define-key my/vterm-workspace-map (kbd n) cmd))))


  ;; ───────────────────────────────────────────────────────────────────────────
  ;; TMUX Session VTERM
  ;; ───────────────────────────────────────────────────────────────────────────

  (defvar my/tmux-default-session "main"
    "Fallback tmux session name when workspace name can't be determined.")

  (defun my/tmux-session-name (&optional frame)
    "Return a tmux-safe session name derived from Doom workspace."
    (let* ((ws (if (fboundp '+workspace-current-name)
                   (with-selected-frame (or frame (selected-frame))
                     (+workspace-current-name))
                 my/tmux-default-session))
           (ws (or ws my/tmux-default-session)))
      ;; tmux session names: keep it simple/safe
      (replace-regexp-in-string "[^[:alnum:]_-]" "_" ws)))

  (defun my/vterm--ensure ()
    "Ensure we have a vterm buffer to send commands to (uses +vterm/here)."
    (unless (derived-mode-p 'vterm-mode)
      (call-interactively #'+vterm/here))
    (unless (derived-mode-p 'vterm-mode)
      (user-error "No vterm buffer available")))

  (defun my/tmux-attach-or-create (&optional session)
    "Attach to SESSION or create it in the current vterm."
    (interactive)
    (my/vterm--ensure)
    (let* ((sess (or session (my/tmux-session-name)))
           ;; -A: attach or create, -s: session name
           (cmd (format "tmux new-session -A -s %s" (shell-quote-argument sess))))
      ;; If we're already inside tmux, just switch client
      (if (getenv "TMUX")
          (setq cmd (format "tmux switch-client -t %s" (shell-quote-argument sess))))
      (vterm-send-string cmd)
      (vterm-send-return)
      (message "tmux: %s" sess)))

  (map! :leader
        :desc "tmux for workspace (attach/create)" "o m" #'my/tmux-attach-or-create)

  (define-key vterm-mode-map (kbd "C-c m") #'my/tmux-attach-or-create))
