;;; init.el --- Personal Emacs config
;;; Commentary:

;;; Code:

(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "https://melpa.org/packages/")
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
(xterm-mouse-mode 1)

(set-variable 'nord-comment-brightness 15)
(load-theme 'nord t)

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

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;; Powerline
(require 'powerline)
(powerline-center-evil-theme)
;; Fix issue with alt-tab not resetting colors
(remove-hook 'focus-out-hook 'powerline-unset-selected-window)

;; Indentation
(setq-default indent-tabs-mode nil)

;; ivy

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c k") 'counsel-ag)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;; make ivy like ido
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(setq ivy-wrap 't)

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

(eval-when-compile (require 'cl))
(defun evil--check-visual-line-mode (visual-fun)
  "Use VISUAL-FUN if in 'visual-line-mode', or ORIG-FUN otherwise (optional ARGS)."
  (lexical-let ((visual-fun visual-fun))
    #'(lambda(orig-fun &rest args)
        (if visual-line-mode
            (apply visual-fun args)
          (apply orig-fun args)))))

(advice-add 'evil-next-line :around (evil--check-visual-line-mode 'evil-next-visual-line))
(advice-add 'evil-previous-line :around (evil--check-visual-line-mode 'evil-previous-visual-line))
(advice-add 'evil-beginning-of-line :around (evil--check-visual-line-mode 'evil-beginning-of-visual-line))
(advice-add 'evil-end-of-line :around (evil--check-visual-line-mode 'evil-end-of-visual-line))

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
(define-key evil-normal-state-map (kbd "C-.") 'ido-imenu-anywhere)
(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-normal-state-map (kbd "?") 'swiper)

(define-key evil-visual-state-map (kbd ";") 'comment-or-uncomment-region-or-line)
(define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
(define-key evil-insert-state-map (kbd "M-d") 'kill-word)
(define-key evil-insert-state-map (kbd "C-.") 'ido-imenu-anywhere)

(require 'evil-surround)
(global-evil-surround-mode 1)

;; (require 'evil-mc)
;; (global-evil-mc-mode 1)
;; (evil-define-key 'normal map (kbd "C-p") 'evil-paste-pop)

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

;; rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook '(lambda ()
                             (racer-mode 1)
                             (eldoc-mode 1)))

;; javascript: js2

(defun my/use-eslint-from-node-modules ()
  "Use local NPM eslint when available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))

;; typescript: tide

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (company-mode-on)))
(add-to-list 'auto-mode-alist `(,(rx ".tsx" string-end) . web-mode))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; latex-mode


(add-hook 'LaTeX-mode-hook (lambda ()
                             (flyspell-mode 1)
                             (visual-line-mode 1)))

;; go-mode

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save nil t)
            (go-eldoc-setup)
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
            ))

;; Ocaml/Merlin

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var"
   "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

;; org-mode

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (visual-line-mode t)
            (adaptive-wrap-prefix-mode t)
            (flyspell-mode t)
            ))

(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

(eval-after-load "ox-latex"

  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(setq org-latex-listings t)
;; (add-to-list 'org-latex-packages-alist '("" "listings"))

;; adaptive-wrap when available

(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xdg-open")
     (output-html "xdg-open"))))
 '(company-racer-executable "~/.cargo/bin/racer")
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".log")))
 '(custom-safe-themes
   (quote
    ("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "a2dd771a05705be2a6e6adb6ddbc7a27ebf49edab1dffdbefe243096becba7c9" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "561ba4316ba42fe75bc07a907647caa55fc883749ee4f8f280a29516525fc9e8" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default)))
 '(doc-view-continuous t)
 '(font-latex-fontify-sectioning 1.0)
 '(gofmt-command "goimports")
 '(hl-sexp-background-color "#1c1f26")
 '(ido-auto-merge-work-directories-length -1)
 '(jabber-account-list
   (quote
    (("david@genki.is"
      (:password . "CWr2jexUWcvuzMyLLEhI")))))
 '(js2-strict-trailing-comma-warning nil)
 '(org-agenda-files nil)
 '(org-html-container-element "section")
 '(org-html-divs
   (quote
    ((preamble "div" "preamble")
     (content "article" "content")
     (postamble "footer" "postamble"))))
 '(org-html-doctype "html5")
 '(org-html-html5-fancy t)
 '(org-html-htmlize-output-type (quote css))
 '(org-preview-latex-default-process (quote imagemagick))
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (rainbow-delimiters nord-theme ace-window counsel ivy moe-theme ag imenu-anywhere org-ref htmlize ox-reveal color-theme-sanityinc-tomorrow white-theme org adaptive-wrap company-go go-eldoc go-guru go-mode lua-mode company-racer material-theme yaml-mode web-mode tuareg tide smex racket-mode racer powerline paredit multiple-cursors markdown-mode magit json-mode js2-mode ido-vertical-mode haskell-mode goto-last-change glsl-mode flycheck-rust flycheck-elm fill-column-indicator expand-region evil-vimish-fold evil-surround evil-rsi evil-mc evil-escape elm-yasnippets elm-mode deferred cyberpunk-theme cargo auctex alchemist)))
 '(racer-cmd "/home/lidavidm/.cargo/bin/racer")
 '(racer-rust-src-path "$(rustc --print sysroot)/lib/rustlib/src/rust/src")
 '(safe-local-variable-values
   (quote
    ((coq-prog-args "-emacs" "-I" "./cpdtlib/")
     (org-latex-pdf-process "latexmk -shell-escape -bibtex -pdf %f")
     (org-html-htmlize-output-type . "inline-css")
     (org-html-htmlize-output-type . "css")
     (org-html-metadata-timestamp-format . "%A, %B %d, %Y")
     (org-src-fontify-natively)
     (js2-basic-offset . 4)
     (js2-basic-offset 4))))
 '(tide-tsserver-executable "node_modules/typescript/bin/tsserver"))

;; custom variables

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-5-face ((t (:foreground "deep sky blue" :slant italic :height 1.15 :family "Charis SIL")))))

(provide 'init)
;;; init.el ends here
