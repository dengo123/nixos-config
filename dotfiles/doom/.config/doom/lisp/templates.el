;;; lisp/templates.el -*- lexical-binding: t; -*-

(after! doom
  ;; Nix rules (specific before generic)
  (set-file-template! "/nixos-config/modules/.*\\.nix\\'"  :trigger "__nix-module"         :mode 'nix-mode)
  (set-file-template! "/nixos-config/homes/.*\\.nix\\'"    :trigger "__nix-home"           :mode 'nix-mode)
  (set-file-template! "/nixos-config/systems/.*\\.nix\\'"  :trigger "__nix-system"         :mode 'nix-mode)
  (set-file-template! "/nixos-config/overlays/.*\\.nix\\'" :trigger "__nix-overlay"        :mode 'nix-mode)

  ;; packages: specific subfolders first
  (set-file-template! "/nixos-config/packages/python/.*\\.nix\\'" :trigger "__nix-package-python" :mode 'nix-mode)
  (set-file-template! "/nixos-config/packages/npm/.*\\.nix\\'"    :trigger "__nix-package-npm"    :mode 'nix-mode)
  (set-file-template! "/nixos-config/packages/.*\\.nix\\'"        :trigger "__nix-package-generic":mode 'nix-mode)

  (set-file-template! "/nixos-config/lib/.*\\.nix\\'"      :trigger "__nix-lib"            :mode 'nix-mode))
