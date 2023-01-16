;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; USE-PACKAGE
(straight-use-package 'use-package)

;; START SERVER
(server-start)

;; HUD
(setq visible-bell 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; THEME
(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t))
(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(setq display-line-numbers 'relative)
(setq font-lock-maximum-decoration t)

;; WHICK-KEY
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; SYNTAX ERRORS
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; SYNTAX PARSER - TODO: remove with EMACS 29
(use-package tree-sitter
  :straight t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter-indent
  :straight t)

;; TEXT AUTOCOMPLETE
(use-package company
  :straight t
  :hook
  (after-init . global-company-mode)
  :config
  (company-mode 1)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))

;; CMD AUTOCOMPLETE
(use-package vertico
  :straight t
  :config
  (vertico-mode))

;; VIM
(use-package evil
  :straight t 
  :config
  (evil-mode 1))

(use-package evil-leader
  :after (evil)
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "pc" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "pr" '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
    "bb" 'bs-show
    "fb" 'dired))

;; TREE VIEW API
(use-package treemacs
  :straight t
  :config
  (evil-leader/set-key
    "ft" 'treemacs))

(use-package treemacs-evil
  :after (treemacs)
  :straight t)

(use-package treemacs-magit
  :after (treemacs)
  :straight t)

;; GIT
(use-package magit
  :straight t
  :config
  (evil-leader/set-key
    "gs" 'magit-status
    "gg" 'magit-dispatch
    "gx" 'magit-file-dispatch))

;; UNITY
(use-package unity
  :straight (unity :type git :host github :repo "elizagamedev/unity.el")
  :hook after-init)

;; LSP
(use-package lsp-mode
  :straight t
  :bind-keymap
  ("C-c lo" . lsp-command-map)
  :custom
  (lsp-keymap-prefix "C-c lo")
  :config
  (evil-leader/set-key
    "kf" 'lsp-format-buffer
    "kd" 'lsp-find-definition
    "kr" 'lsp-find-references
    "ka" 'lsp-execute-code-action))

(use-package lsp-ui
  :straight t)

(use-package lsp-treemacs
  :straight t
  :after (lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))

;; C#
(use-package csharp-mode
  :straight t
  :init
  (defun my/csharp-mode-hook ()
    (setq-local lsp-auto-guess-root t)
    (lsp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))

;; RUST
(use-package rust-mode
  :straight t)

;; INTERNAL
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit org-modern evil-leader company use-package evil dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
