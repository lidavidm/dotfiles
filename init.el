;;; init.el --- Personal Emacs config
;;; Commentary:

;;; Code:
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; GUI settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq frame-title-format "%b - emacs")
(global-linum-mode 1)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-frame-font (font-spec :family "Input" :size 10.5))

;; (defun set-80-editing-columns ()
;;   "Set the right window margin so the editable space is only 80 columns."
;;   (interactive)
;;   (setq right-margin-width (max (- (window-width) 80) 0))
;;   (set-window-buffer nil (current-buffer))
;;   )

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (set-80-editing-columns)
;;             ))

(global-auto-revert-mode 1)

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
;; (global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; fixmes

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|XXX\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;; evil
(require 'evil)
(evil-mode 1)
(setq-default evil-cross-lines t)
;; http://stackoverflow.com/a/32660401/262727
(defun evil-next-line--check-visual-line-mode (orig-fun &rest args)
  (if visual-line-mode
      (apply 'evil-next-visual-line args)
    (apply orig-fun args)))

(advice-add 'evil-next-line :around 'evil-next-line--check-visual-line-mode)

(defun evil-previous-line--check-visual-line-mode (orig-fun &rest args)
  (if visual-line-mode
      (apply 'evil-previous-visual-line args)
    (apply orig-fun args)))

(advice-add 'evil-previous-line :around 'evil-previous-line--check-visual-line-mode)

(require 'evil-little-word)
(define-key evil-motion-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "glw") 'evil-forward-word-begin)
(define-key evil-motion-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "glb") 'evil-backward-word-begin)

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
(evil-define-key 'normal map (kbd "C-p") 'evil-paste-pop)

(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "fd")

;; expand-region
(require 'expand-region)
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-visual-state-map (kbd "V") 'er/contract-region)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;;; needed for flychcek to check whether the crate is binary or library
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; typescript: tide

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; latex-mode


(add-hook 'LaTeX-mode-hook (lambda ()
                             (flyspell-mode 1)
                             (visual-line-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "561ba4316ba42fe75bc07a907647caa55fc883749ee4f8f280a29516525fc9e8" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default)))
 '(doc-view-continuous t)
 '(js2-strict-trailing-comma-warning nil))

;; custom variables

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
