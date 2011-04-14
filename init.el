;;;;;;;;;;;;;;;
;;;; PATH ;;;;;
;;;;;;;;;;;;;;;

(push "/usr/local/bin" exec-path)


;;;;;;;;;;;;;;;;
;;;; DEFUNS ;;;;
;;;;;;;;;;;;;;;;

(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (ding)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)  (delete-trailing-whitespace))

;;;;;;;;;;;;;;;;
;;;; PREFS ;;;;;
;;;;;;;;;;;;;;;;

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'super)

(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-{") 'previous-buffer)

(delete-selection-mode t)
(scroll-bar-mode t)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(global-linum-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(normal-erase-is-backspace-mode 1)
(set-frame-font "Monaco-16")
(server-start)

;; Don't ring bell at top of buffer, when canceling minibuffer command, etc.
;; Source: http://stackoverflow.com/questions/324457/disable-carbon-emacs-scroll-beep/731660#731660

(setq ring-bell-function 'my-bell-function)

(global-set-key (kbd "C-c n") 'cleanup-buffer)
;;;;;;;;;;;;;;;;;;
;;;; PACKAGES ;;;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;;; To make the latest package.el work with Emacs 23
(defconst package-subdirectory-regexp
  "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)")

(setq el-get-sources
      '(el-get color-theme color-theme-twilight color-theme-zenburn elein
        (:name package24
               :after (lambda ()
                        (add-to-list 'package-archives
                                     '("marmalade" . "http://marmalade-repo.org/packages/")
                                     t)))
        (:name color-theme-zen-and-art
               :after (lambda ()
                        (setq color-theme-is-global t)
                        (color-theme-zen-and-art)))
        (:name magit
               :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
        (:name paredit
               :after (lambda () 
                        (add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
                        (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
                        (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
                        (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))))
        (:name slime-repl :type elpa)
        (:name clojure-mode :type elpa)
        (:name durendal :after (lambda () (durendal-enable)))
        (:name midje-mode
               :type git
               :url "http://github.com/marick/Midje.git"
               :load-path "emacs"
               :features midje-mode
               :after (lambda () (add-hook 'clojure-mode-hook 'midje-mode)))
        (:name swank-clojure
               :after (lambda () (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))
        (:name textmate
               :type git
               :url "git://github.com/defunkt/textmate.el"
               :load "textmate.el"
               :after (lambda () (textmate-mode)))))

(el-get)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

