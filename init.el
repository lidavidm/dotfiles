;;; init.el --- Personal Emacs config
;;; Commentary:

;;; Code:
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)

;; GUI settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq frame-title-format "%b - emacs")
(global-linum-mode 1)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-frame-font "Iosevka 11")

;; Powerline
(require 'powerline)
(powerline-center-evil-theme)

;; Indentation
(setq-default indent-tabs-mode nil)

;; ido
(require 'ido)
(ido-mode t)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; electric-pair
(electric-pair-mode 1)

;; whitespace
(global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; evil
(require 'evil)
(evil-mode 1)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(define-key evil-normal-state-map (kbd ";") 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map (kbd ";") 'comment-or-uncomment-region-or-line)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-mc)
(global-evil-mc-mode 1)

(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "fd")

;; expand-region
(require 'expand-region)
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-visual-state-map (kbd "V") 'er/contract-region)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("561ba4316ba42fe75bc07a907647caa55fc883749ee4f8f280a29516525fc9e8" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
