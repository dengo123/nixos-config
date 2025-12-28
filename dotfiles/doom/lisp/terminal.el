;;; lisp/terminal.el -*- lexical-binding: t; -*-

(after! vterm
  ;; vterm in emacs-state (Terminal-Apps nicht von evil stören)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook #'evil-emacs-state)

  ;; ───────────────────────────────────────────────────────────────────────────
  ;; Popup vterm pro Workspace, aber weiterhin auf SPC o t
  ;; ───────────────────────────────────────────────────────────────────────────

  (defun my/ws-name ()
    (if (fboundp '+workspace-current-name)
        (+workspace-current-name)
      "main"))

  (defun my/ws-safe (s)
    ;; Buffername-sicher machen (Slash/Spaces etc.)
    (replace-regexp-in-string
     "[^[:alnum:]._+-]" "_"
     (or s "main")))

  (defun my/vterm-popup-buffer-name ()
    (format "*vterm-popup:%s*" (my/ws-safe (my/ws-name))))

  (defun my/vterm--window-showing (buf)
    "Return a window displaying BUF in the selected frame, else nil."
    (get-buffer-window buf (selected-frame)))

  (defun my/vterm-popup-toggle ()
    "Toggle a dedicated vterm popup for the current Doom workspace."
    (interactive)
    (let* ((bufname (my/vterm-popup-buffer-name))
           (buf (get-buffer bufname))
           (win (and buf (my/vterm--window-showing buf))))
      (cond
       ;; If visible -> hide it
       (win
        (delete-window win))

       ;; If exists but not visible -> show it
       ((buffer-live-p buf)
        (pop-to-buffer
         buf
         '((display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 0.30))))

       ;; Else create it -> show it
       (t
        (let* ((vterm-buffer-name-string (substring bufname 1 -1)) ; without *...*
               (b (vterm)))
          (with-current-buffer b
            (rename-buffer bufname t))
          (pop-to-buffer
           (get-buffer bufname)
           '((display-buffer-reuse-window display-buffer-at-bottom)
             (window-height . 0.30))))))))

  ;; Doom-style: SPC o t = popup, SPC o T = normal "here"
  ;; (wir überschreiben bewusst das Doom-default toggle)
  (map! :leader
        :desc "vterm popup (per-workspace)" "o t" #'my/vterm-popup-toggle
        :desc "vterm here"                 "o T" #'+vterm/here)

  ;; ───────────────────────────────────────────────────────────────────────────
  ;; "SPC TAB …" Ersatz in vterm: C-c TAB …
  ;; (inkl. Nummern 1..9, exakt wie Doom gebunden hat)
  ;; ───────────────────────────────────────────────────────────────────────────

  (defvar my/vterm-workspace-map (make-sparse-keymap)
    "Workspace prefix map for vterm (C-c TAB …).")

  (define-key vterm-mode-map (kbd "C-c TAB") my/vterm-workspace-map)

  (defun my/doom-spc-tab-cmd (keyseq)
    "Return the command bound to Doom leader 'TAB KEYSEQ'."
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
        (define-key my/vterm-workspace-map (kbd n) cmd)))))
