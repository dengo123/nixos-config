;;; init.el -*- lexical-binding: t; -*-

;; --- Completion --------------------------------------------------------------
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))

;; Wähle deine Doom-Module:
(doom! :input
       ;; layout            ; Colemak/etc.

       :completion
       vertico              ; the search engine of the future
       (company +childframe) ; text completion backend

       :ui
       doom                 ; what makes DOOM look the way it does
       doom-dashboard       ; a nifty splash screen
       hl-todo              ; highlight TODO/FIXME/NOTE
       indent-guides        ; visual indent guides
       modeline             ; snazzy, Atom-inspired modeline
       nav-flash            ; blink the current line after big motions
       (popup +defaults)    ; tame sudden yet inevitable temporary windows
       vc-gutter            ; version-control diff in the fringe
       workspaces           ; tab emulation, persistence & separate workspaces
       (treemacs +lsp)      ; file tree (optional, kann man ausknipsen)
       ophints              ; highlight the region an operation acts on
       unicode              ; extended unicode support

       :editor
       (evil +everywhere)   ; vim emulation
       file-templates       ; auto-snippets for empty files
       fold                 ; (nicer) code folding
       format               ; automated prettiness
       multiple-cursors     ; editing in many places at once
       snippets             ; my elves. They type so I don't have to
       word-wrap            ; soft wrapping

       :emacs
       dired                ; better dired
       (ibuffer +icons)     ; a better buffer list
       undo                 ; undo/redo
       vc                   ; version control

       :term
       vterm                ; the best terminal emulation in Emacs (optional)

       :tools
       direnv               ; direnv integration
       (eval +overlay)      ; repls
       (lookup +dictionary +docsets)
       lsp                  ; language server protocol
       magit                ; git porcelain inside Emacs
       tree-sitter          ; faster parsing (Emacs 29+)
       editorconfig

       :checkers
       (syntax +flymake)    ; alternative: +flycheck
       (spell +flyspell)    ; optional

       :lang
       (bash +lsp)
       (cc +lsp)
       (emacs-lisp +lsp)
       (json +lsp)
       (lua +lsp)
       (markdown +grip)     ; README’s etc.
       (nix +lsp)
       (org +pretty +pomodoro +roam2) ; falls du Org nutzt
       (python +lsp +pyright)
       (toml +lsp)
       (yaml +lsp)

       :email
       ;; mu4e

       :app
       ;; rss

       :config
       (default +bindings +smartparens))

