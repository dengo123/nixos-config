;;; lisp/terminal.el -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; Workspace helpers
;; ---------------------------------------------------------------------------

(defun my/ws-safe (s)
  "Make S safe to use in buffer names."
  (replace-regexp-in-string
   "[^[:alnum:]._+-]" "_" (or s "main")))

(defun my/ws-name (&optional frame)
  "Return Doom workspace name for FRAME."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (if (fboundp '+workspace-current-name)
          (+workspace-current-name)
        "main"))))

(defun my/vterm-popup-buffer-for (ws)
  "Return popup vterm buffer name for workspace WS."
  (format "*vterm-popup:%s*" (my/ws-safe ws)))

(defun my/vterm-buffer-for (ws)
  "Return full vterm buffer name for workspace WS."
  (format "*vterm:%s*" (my/ws-safe ws)))

(defun my/vterm-buffer-for-slot (ws slot)
  "Return workspace-specific vterm buffer name for SLOT."
  (format "*vterm:%s:%s*" (my/ws-safe ws) slot))

;; ---------------------------------------------------------------------------
;; vterm helpers
;; ---------------------------------------------------------------------------

(defun my/vterm--window-showing (buf &optional frame)
  "Return window showing BUF in FRAME."
  (get-buffer-window buf (or frame (selected-frame))))

(defun my/vterm--create-buffer (bufname)
  "Create a vterm buffer named BUFNAME without disturbing window layout."
  (let ((b nil))
    (save-window-excursion
      (setq b (vterm)))
    (with-current-buffer b
      (rename-buffer bufname t))
    (get-buffer bufname)))

(defun my/vterm-send-command (cmd)
  "Send CMD to current vterm buffer."
  (when (derived-mode-p 'vterm-mode)
    (vterm-send-string cmd)
    (vterm-send-return)))

(defun my/vterm-popup-toggle ()
  "Toggle workspace-specific vterm popup."
  (interactive)
  (let* ((frame (selected-frame))
         (ws (my/ws-name frame))
         (bufname (my/vterm-popup-buffer-for ws))
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
      (setq buf (my/vterm--create-buffer bufname))
      (pop-to-buffer
       buf
       '((display-buffer-reuse-window display-buffer-at-bottom)))))))

(defun my/vterm-here ()
  "Open or switch to workspace-specific full vterm in current window."
  (interactive)
  (let* ((ws (my/ws-name))
         (bufname (my/vterm-buffer-for ws))
         (buf (get-buffer bufname)))
    (unless (buffer-live-p buf)
      (setq buf (my/vterm--create-buffer bufname)))
    (switch-to-buffer buf)))

(defun my/vterm-open-in-current-window (bufname)
  "Open or create vterm BUFNAME in the selected window."
  (let ((buf (get-buffer bufname)))
    (unless (buffer-live-p buf)
      (setq buf (my/vterm--create-buffer bufname)))
    (switch-to-buffer buf)))

(defun my/main-dashboard ()
  "Create a 3-window terminal dashboard in workspace `main'."
  (interactive)
  (let ((ws (my/ws-name)))
    (unless (string= ws "main")
      (user-error "This dashboard is intended for workspace `main'"))
    (delete-other-windows)
    (let* ((top (selected-window))
           (bot (split-window top (floor (* 0.75 (window-total-height))) 'below))
           (mid (split-window top (floor (* 0.3333 (window-total-height))) 'below)))
      (select-window top)
      (my/vterm-open-in-current-window (my/vterm-buffer-for-slot ws "clock"))
      (my/vterm-send-command "peaclock")

      (select-window mid)
      (my/vterm-open-in-current-window (my/vterm-buffer-for-slot ws "btop"))
      (my/vterm-send-command "btop")

      (select-window bot)
      (my/vterm-open-in-current-window (my/vterm-buffer-for-slot ws "nvtop"))
      (my/vterm-send-command "nvtop")

      (select-window mid))))

;; ---------------------------------------------------------------------------
;; Global Doom leader bindings
;; ---------------------------------------------------------------------------

(map! :leader
      :desc "vterm popup (per-workspace)" "o t" #'my/vterm-popup-toggle
      :desc "vterm here (per-workspace)"  "o T" #'my/vterm-here
      :desc "Main dashboard"              "o M" #'my/main-dashboard)

;; ---------------------------------------------------------------------------
;; vterm setup
;; ---------------------------------------------------------------------------

(after! vterm
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook #'evil-emacs-state)

  ;; Doom leader replacement inside vterm
  (define-key vterm-mode-map (kbd "C-c SPC") doom-leader-map))

;;; terminal.el ends here
