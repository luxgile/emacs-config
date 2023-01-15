(require 'package)
(eval-when-compile
  (require 'use-package))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; HUD
(setq visible-bell 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; THEME
(load-theme 'dracula t)
(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; AUTOCOMPLETE
(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (company-mode 1)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))

;; VIM
(use-package evil
  :init
  (use-package evil-leader
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
  :config
  (evil-leader/set-key
    "gs" 'magit-status
    "gg" 'magit-dispatch
    "gx" 'magit-file-dispatch))

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
