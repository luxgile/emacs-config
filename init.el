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

;; HUD
(setq visible-bell 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; THEME
(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t))
(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; AUTOCOMPLETE
(use-package company
  :straight t
  :hook
  (after-init . global-company-mode)
  :config
  (company-mode 1)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))

;; VIM
(use-package evil
  :straight t 
  :init
  (use-package evil-leader
    :straight t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "pc" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "pr" '(lambda () (interactive) (load-file "~/.emacs.d/init.el"))
      "bb" 'bs-show
      "fb" 'dired))
  :config
  (evil-mode 1))

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
