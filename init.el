;;; init.el --- Personal Emacs config
;;; Commentary:

;;; Code:

(require 'package)
(push '("melpa" . "https://melpa.org/packages/")
      package-archives)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; For lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 4 1024 1024)) ;; 4mb

;; GUI settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq frame-title-format "%b - emacs")

;; Some minor modes

(add-hook 'text-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            ))
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            ))

(global-auto-revert-mode 1)

(global-undo-tree-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;; yasnippet
(yas-global-mode 1)

;; doom-modeline

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-minor-modes nil)
(column-number-mode 1)

;; Indentation
(setq-default indent-tabs-mode nil)

;; selectrum

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;; projectile

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; electric-pair
(electric-pair-mode 1)

;; whitespace
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
(evil-define-key 'normal 'global
  "g+" 'evil-numbers/inc-at-pt
  "g=" 'evil-numbers/inc-at-pt
  "g-" 'evil-numbers/dec-at-pt)
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

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
(define-key evil-insert-state-map (kbd "M-d") 'kill-word)
(define-key evil-normal-state-map (kbd ";") 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map (kbd ";") 'comment-or-uncomment-region-or-line)

(require 'evil-surround)
(global-evil-surround-mode 1)

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
(add-to-list 'auto-mode-alist `(,(rx ".mjs" string-end) . js2-mode))

;; web-mode

(add-to-list 'auto-mode-alist `(,(rx ".svelte" string-end) . web-mode))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; C/C++

(require 'google-c-style)
(add-hook 'c++-mode-hook
          (lambda ()
            (google-set-c-style)
            ;; turn off flycheck since it's not very useful to us
            (flycheck-mode 0)
            (lsp-deferred)))

;; Markdown

(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (flyspell-mode t)
            ))

;; org-mode

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (flyspell-mode t)
            ))

;; adaptive-wrap when available

(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

;; macros for Arrow development

(fset 'assign-or-raise
   (kmacro-lambda-form [?i ?A ?R ?R ?O ?W ?_ ?A ?S ?S ?I ?G ?N ?_ ?O ?R ?_ ?R ?A ?I ?S ?E ?\( delete escape ?f ?= ?a backspace backspace ?, escape ?f ?\; ?s ?\) ?\; escape] 0 "%d"))

(fset 'return-not-ok
   (kmacro-lambda-form [?v ?% ?S ?\) ?i ?R ?E ?T ?U ?R ?N ?_ ?N ?O ?T ?_ ?O ?K escape] 0 "%d"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments '("verbatim" "verbatim*" "lstlisting"))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xdg-open")
     (output-html "xdg-open")))
 '(company-racer-executable "~/.cargo/bin/racer")
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".log"))
 '(custom-safe-themes
   '("e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "37a4701758378c93159ad6c7aceb19fd6fb523e044efe47f2116bc7398ce20c9" "6096a2f93610f29bf0f6fe34307587edd21edec95073cbfcfb9d7a3b9206b399" "d6c5b8dc6049f2e9dabdfcafa9ef2079352640e80dffe3e6cc07c0f89cbf9748" "ce3e6c12b48979ce89754884d913c7ecc8a7956543d8b09ef13abfab6af9aa35" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8f83397e58db2a094d534aef83bbecbeefa1fb473141e7504609dc07580b66af" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "a2dd771a05705be2a6e6adb6ddbc7a27ebf49edab1dffdbefe243096becba7c9" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "561ba4316ba42fe75bc07a907647caa55fc883749ee4f8f280a29516525fc9e8" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default))
 '(doc-view-continuous t)
 '(ess-style 'RStudio)
 '(evil-undo-system 'undo-tree)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-markdownlint-cli markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-python-pycompile-executable "python3")
 '(font-latex-fontify-sectioning 1.0)
 '(gofmt-command "goimports")
 '(hl-sexp-background-color "#1c1f26")
 '(ido-auto-merge-work-directories-length -1)
 '(jabber-account-list '(("david@genki.is" (:password . "CWr2jexUWcvuzMyLLEhI"))))
 '(js2-highlight-level 3)
 '(js2-mode-assume-strict t)
 '(lsp-clients-clangd-args '("--header-insertion-decorators=0"))
 '(org-agenda-files nil)
 '(org-html-container-element "section")
 '(org-html-divs
   '((preamble "div" "preamble")
     (content "article" "content")
     (postamble "footer" "postamble")))
 '(org-html-doctype "html5")
 '(org-html-html5-fancy t)
 '(org-html-htmlize-output-type 'css)
 '(org-preview-latex-default-process 'imagemagick)
 '(org-startup-folded nil)
 '(package-selected-packages
   '(prescient selectrum selectrum-prescient mermaid-mode yasnippet evil-numbers string-inflection ess undo-tree lsp-mode flycheck protobuf-mode pollen-mode ripgrep doom-modeline prettier cider company cython-mode cmake-mode counsel-projectile projectile js2-mode leuven-theme rust-mode merlin merlin-eldoc eglot doom-themes poet-theme js2-refactor org-bullets rainbow-delimiters nord-theme ace-window counsel ivy moe-theme ag imenu-anywhere org-ref htmlize ox-reveal color-theme-sanityinc-tomorrow white-theme adaptive-wrap company-go go-eldoc go-guru go-mode lua-mode company-racer material-theme yaml-mode web-mode tuareg tide smex racket-mode racer powerline paredit multiple-cursors markdown-mode magit json-mode ido-vertical-mode haskell-mode goto-last-change glsl-mode flycheck-rust flycheck-elm fill-column-indicator expand-region evil-vimish-fold evil-surround evil-rsi evil-mc evil-escape elm-yasnippets elm-mode deferred cyberpunk-theme cargo auctex alchemist))
 '(racer-cmd "/home/lidavidm/.cargo/bin/racer")
 '(ripgrep-executable "/home/lidavidm/.nix-profile/bin/rg")
 '(rust-format-on-save t)
 '(safe-local-variable-values
   '((eval add-to-list 'lsp-clients-clangd-args "--query-driver=/home/lidavidm/miniconda3/envs/dev/bin/g++")
     (nxml-child-indent . 4)
     (js2-global-externs . import)
     (js2-include-node-externs . t)
     (js2-include-node-externs quote t)
     (eval prettier-mode t)
     (js2-basic-offset . 2)
     (coq-prog-args "-emacs" "-I" "./cpdtlib/")
     (org-latex-pdf-process "latexmk -shell-escape -bibtex -pdf %f")
     (org-html-htmlize-output-type . "inline-css")
     (org-html-htmlize-output-type . "css")
     (org-html-metadata-timestamp-format . "%A, %B %d, %Y")
     (org-src-fontify-natively)
     (js2-basic-offset . 4)
     (js2-basic-offset 4)))
 '(tide-tsserver-executable "node_modules/typescript/bin/tsserver")
 '(web-mode-script-padding 4)
 '(web-mode-style-padding 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 105 :family "JetBrains Mono"))))
 '(selectrum-prescient-primary-highlight ((t (:foreground "black" :underline t :weight bold)))))

(if (display-graphic-p)
    (load-theme 'doom-flatwhite))

(provide 'init)
;;; init.el ends here
