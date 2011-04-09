;;;;;;;;;;;;;;;
;;;; PATH ;;;;;
;;;;;;;;;;;;;;;

(push "/usr/local/bin" exec-path)

;;;;;;;;;;;;;;;;
;;;; PREFS ;;;;;
;;;;;;;;;;;;;;;;

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

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
               :after (lambda () (add-hook 'clojure-mode-hook 'paredit-mode)))
        (:name slime-repl :type elpa)
        (:name clojure-mode :type elpa)
        (:name midje-mode
               :type git
               :url "http://github.com/marick/Midje.git"
               :load-path "emacs"
               :load "midje-mode.el"
               :features midje-mode
               :after (lambda () (add-hook 'clojure-mode-hook 'midje-mode)))
        (:name swank-clojure
               :after (lambda () (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))
        (:name textmate
               :type git
               :url "git://github.com/defunkt/textmate.el"
               :load "textmate.el")))

(el-get)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)