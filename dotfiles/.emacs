;; vars

(defvar home-dir (concat (expand-file-name "~") "/"))


;; buffer

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Emacs" (or
                         (mode . help-mode)
                         (mode . Info-mode)
                         (mode . Custom-mode)
                         (mode . apropos-mode)
                         (mode . emacs-lisp-mode)
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("SLIME" (or
                         (mode . lisp-mode)
                         (name . "^\\*slime")
                         (name . "^\\*inferior-lisp\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(defadvice ibuffer (around ibuffer-jump-to-recent-buffer)
  "Jump to the most recent buffer after switching to *Ibuffer*."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))

(ad-activate 'ibuffer)

(require 'uniquify)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(put 'dired-find-alternate-file 'disabled nil)


;; info

(setenv "INFOPATH" nil t)
(setq-default Info-default-directory-list
              (cons "/home/vs/lib/hyperspec/info/"
                    Info-default-directory-list))


;; python

(require 'pycomplete)
(autoload 'python-mode "python-mode" "Python Mode." t)
(autoload 'pymacs-load  "pymacs" nil t)
(autoload 'pymacs-eval  "pymacs" nil t)
(autoload 'pymacs-apply "pymacs" nil t)
(autoload 'pymacs-call  "pymacs" nil t)
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq ipython-command "/usr/local/bin/ipython")
(require 'ipython)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; whitespace

(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(setq whitespace-line-column 100)
(setq whitespace-style '(tabs tab-mark lines-tail))
(setq whitespace-global-modes '(lisp-mode python-mode clojure-mode js-mode))
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'lisp-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'whitespace-mode)

;; tabs

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;; php

(require 'php-mode)

(defun my-php-mode-common-hook ()
  ;; my customizations for php-mode
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'class-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro '+))

(add-hook 'php-mode-hook 'my-php-mode-common-hook)


;; linum

(column-number-mode t)
(line-number-mode t)


;; colors

(require 'color-theme)
(defun color-theme-tango ()
  (interactive)
  (color-theme-install
   '(color-theme-tango
     ((background-color . "#2e3436")
      (background-mode . dark)
      (border-color . "#888a85")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "#8ae234"))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (border ((t (:background "#888a85"))))
     (fringe ((t (:background "grey10"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#555753"))))
     (font-lock-builtin-face ((t (:foreground "#729fcf"))))
     (font-lock-comment-face ((t (:foreground "#888a85"))))
     (font-lock-constant-face ((t (:foreground "#8ae234"))))
     (font-lock-doc-face ((t (:foreground "#888a85"))))
     (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
     (font-lock-string-face ((t (:foreground "#ad7fa8" :italic t))))
     (font-lock-type-face ((t (:foreground "#8ae234" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
     (font-lock-warning-face ((t (:bold t :foreground "#f57900"))))
     (font-lock-function-name-face ((t (:foreground "#edd400" :bold t :italic t))))
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (isearch ((t (:background "#f57900" :foreground "#2e3436"))))
     (isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
     (show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
     (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))
     )))

(color-theme-tango)


;; basic

(show-paren-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(defconst query-replace-highlight t)
(defconst search-highlight t)
(global-font-lock-mode t)
(tool-bar-mode nil)
(menu-bar-mode nil)
(setq inhibit-startup-message t)
(setq x-select-enable-clipboard   t
      interprogram-cut-function   'x-select-text
      interprogram-paste-function 'x-cut-buffer-or-selection-value)
(require 'diminish)
(eval-after-load "filladapt" '(diminish 'filladapt-mode))
(prefer-coding-system 'utf-8-unix)


;; global keybindings

(global-set-key [f5] 'slime)
(global-set-key [(C f5)] 'slime-restart-inferior-lisp)
(global-set-key [(C .)] 'slime-connect)
(global-set-key (kbd "C-x v") 'slime-selector)
(global-set-key [f6] 'linum-mode)
(global-set-key [f7] 'py-shell)
(global-set-key [f8] 'shell)
(global-set-key [(C q)] 'query-replace-regexp)
(global-set-key [(C l)] 'backward-kill-line)
(global-set-key [(C S l)] 'delete-indentation)
(global-set-key (kbd "C-;") 'recenter-top-bottom)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key [(C z)] 'rgrep)
(global-set-key (kbd "C-<") 'goto-line)

(defun force-exit ()
  "Prevent accidental exit"
  )

(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0)
  (backward-delete-char 1))


;; shell

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")


;; autocompletion

(require 'hippie-exp)
(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))


;; common lisp

(defvar cltl2-root-url (concat home-dir "docs/cltl/"))
(defvar cltl2-prog (concat home-dir "site/ilisp/extra/cltl2"))
(defvar common-lisp-hyperspec-root (concat home-dir "docs/Hyperspec/"))
(defvar common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
(defvar hyperspec-prog (concat home-dir "site/ilisp/extra/hyperspec"))

(add-hook 'lisp-mode-hook (lambda ()
                            (slime-mode)))




;; elisp

;(require 'cl)


;; tramp

(setq tramp-default-method "ssh")


;; start server

(server-start)

;; swap windows

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(global-set-key (kbd "C-c s") 'swap-windows)

;; js2-mode

;(add-hook 'js2-mode-hook 'my-js2-mode-bindings)
;(defun my-js2-mode-bindings ())
;  (define-key js2-mode-map [tab] 'self-insert-command)
;  (setq tab-width 4))

;; spelling

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; (dolist (hook '(lisp-mode-hook php-mode-hook))
;;   (add-hook hook (lambda () (flyspell-prog-mode))))


;; scala

(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))


;; css

(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)


;; mustache

(require 'mustache-mode)


;; clojure

(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key [(C t)] 'transpose-sexps)))

;; slime

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/bin/slime-2009-12-10")
(require 'slime-autoloads)

(slime-setup '(slime-scratch slime-editing-commands slime-asdf slime-fuzzy slime-repl))

(setq slime-net-coding-system 'utf-8-unix)
(add-to-list 'minor-mode-alist
             '(slime-fuzzy-target-buffer-completions-mode
               " Fuzzy Target Buffer Completions"))

(defun slime-keys ()
  (local-set-key [f1] '(lambda ()
                         (interactive)
                         (info (concatenate 'string "(gcl) " (thing-at-point 'symbol)))))
  (local-set-key [tab] 'slime-indent-and-complete-symbol)
  (local-set-key [(C t)] 'transpose-sexps))
(add-hook 'slime-mode-hook 'slime-keys)

(add-hook 'slime-mode-hook
          (lambda ()
            (setq slime-truncate-lines nil)
            (slime-redirect-inferior-output)))

(require 'swank-clojure)

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--sbcl-nolineedit"))
        (ecl ("ecl"))
        (ccl ("ccl"))
        (ccl64 ("ccl64"))
        (clojure ("lein swank") :init swank-clojure-init)))

(defmacro defslime-start (name mapping)
  `(defun ,name ()
     (interactive)
     (let ((slime-default-lisp ,mapping))
       (slime))))

(defslime-start ccl 'ccl)
(defslime-start ccl64 'ccl64)
(defslime-start clojure 'clojure)
(defslime-start ecl 'ecl)
(defslime-start sbcl 'sbcl)


;; fw/bw

(defun fw ()
  (interactive)
  (if (= (following-char) ?\n)
      (progn (forward-char)
             (skip-chars-forward "[:blank:]"))
    (let ((skipped (skip-chars-forward "[:alnum:]") ))
      (when (= skipped 0)
        (skip-chars-forward "[:punct:]")
        (skip-chars-forward "[:blank:]")))))

(defun bw ()
  (interactive)
  (if (= (preceding-char) ?\n)
      (progn (backward-char)
             (skip-chars-backward "[:blank:]"))
    (flet ((skip-alnum-or-punct-backward ()
             (let ((alnum-skipped (skip-chars-backward "[:alnum:]")))
               (if (= alnum-skipped 0)
                   (skip-chars-backward "[:punct:]")
                   alnum-skipped))))
      (when (= 0 (skip-alnum-or-punct-backward))
        (skip-chars-backward "[:blank:]")
        (skip-alnum-or-punct-backward)))))

(defun fw-kill-word ()
  (interactive)
  (kill-region (point) (progn (fw) (point))))

(defun bw-kill-word ()
  (interactive)
  (kill-region (point) (progn (bw) (point))))

(global-set-key [C-left] 'bw)
(global-set-key [C-right] 'fw)
(global-set-key [C-backspace] 'bw-kill-word)
(global-set-key [C-delete] 'fw-kill-word)

