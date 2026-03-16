;;; lisp/projectile.el -*- lexical-binding: t; -*-

(after! projectile
  (setq projectile-indexing-method 'hybrid)

  (defun my/projectile-buffers-same-root (buffers)
    "Keep only buffers whose *own* Projectile root equals the current Projectile root."
    (let* ((cur-root (when (projectile-project-p)
                       (file-truename (projectile-project-root)))))
      (seq-filter
       (lambda (buf)
         (with-current-buffer buf
           (let* ((f (buffer-file-name))
                  (dir (or (and f (file-name-directory f))
                           default-directory))
                  (buf-root (ignore-errors
                              (let ((default-directory dir))
                                (projectile-project-root)))))
             (and cur-root buf-root
                  (string= (file-truename buf-root) cur-root)))))
       (projectile-buffers-with-file-or-process buffers))))

  (setq projectile-buffers-filter-function #'my/projectile-buffers-same-root)

  ;; Niemals Nix-Store als Projekt behandeln
  (setq projectile-ignored-project-function
        (lambda (root)
          (or (string-prefix-p "/nix/store/" root)
              (string-prefix-p "/nix/var/" root)
              (string-prefix-p "/run/" root)
              (string-prefix-p "/tmp/" root))))

  (defun my/projectile-kill-buffer ()
    "Kill one buffer belonging to the current Projectile project."
    (interactive)
    (let* ((bufs (projectile-project-buffers))
           (names (mapcar #'buffer-name bufs))
           (choice (completing-read "Kill project buffer: " names nil t)))
      (when-let ((b (get-buffer choice)))
        (kill-buffer b)
        (message "Killed: %s" choice))))

  (map! :leader
        :desc "Kill project buffer" "p K" #'my/projectile-kill-buffer))
