;;;;;;;;;;;;;;;
;;;; PATH ;;;;;
;;;;;;;;;;;;;;;

(push "/usr/local/bin" exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;;;;;;;;;;;;;;;;
;;;; EL-GET ;;;;
;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(require 'el-get)
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

;; Cursor-style setting functions
;; Hat-tip: https://github.com/candera/emacs
(defun set-cursor-type (cursor)
  "Modify the cursor to the specified type"
  (interactive "sCursor type (bar, box, etc.): ")
  (modify-frame-parameters
   (selected-frame)
   (list (cons 'cursor-type (intern cursor)))))

(defun set-bar-cursor ()
  "Change the cursor to a bar rather than the (default) box"
  (interactive)
  (set-cursor-type "bar"))

(defun set-box-cursor ()
  "Change the cursor to a box (the default style)"
  (interactive)
  (set-cursor-type "box"))

;;;;;;;;;;;;;;;;
;;;; PREFS ;;;;;
;;;;;;;;;;;;;;;;

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

(global-auto-revert-mode 1)

(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-{") 'previous-buffer)

(delete-selection-mode t)
(scroll-bar-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(global-hl-line-mode t)
(unless window-system (setq linum-format "%d "))

(tool-bar-mode -1)
(set-fringe-style -1)
(tooltip-mode -1)
(if window-system (normal-erase-is-backspace-mode 1))


(add-hook 'text-mode-hook (lambda () (longlines-mode t)))
(set-bar-cursor)

;; Mac OS X conditional preferences
(unless (string-match "apple-darwin" system-configuration)
  (menu-bar-mode -1)
  (set-frame-font "Monospace-10"))

(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (set-face-font 'default "Monaco-16"))

(server-start)

;; Don't ring bell at top of buffer, when canceling minibuffer command, etc.
;; Source: http://stackoverflow.com/questions/324457/disable-carbon-emacs-scroll-beep/731660#731660

(setq ring-bell-function 'my-bell-function)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;;;;;;;;;;;;;;;;;;
;;;; CUSTOM ;;;;;;
;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;
;;;; PACKAGES ;;;;
;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             t)
(package-initialize)

(setq el-get-sources
      '(el-get elein coffee-mode haml-mode sass-mode
               (:name markdown-mode
                      :after (lambda ()
                               (add-hook 'markdown-mode-hook (lambda () (longlines-mode t)))))
               (:name zenburn-theme
                      :type git
                      :url  "https://github.com/djcb/elisp.git"
                      :load "themes/zenburn-theme.el")
               (:name magit
                      :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
               (:name paredit
                      :after (lambda ()
                               (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
                               (add-hook 'clojure-mode-hook             (lambda () (paredit-mode +1)))
                               (add-hook 'emacs-lisp-mode-hook          (lambda () (paredit-mode +1)))
                               (add-hook 'lisp-mode-hook                (lambda () (paredit-mode +1)))
                               (add-hook 'lisp-interaction-mode-hook    (lambda () (paredit-mode +1)))))
               (:name clojure-mode :type elpa)
               (:name clojure-test-mode :type elpa)
               (:name slime-repl :type elpa
                      :after (lambda ()
                               (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))
               (:name textmate
                      :type git
                      :url "git://github.com/defunkt/textmate.el"
                      :load "textmate.el"
                      :after (lambda () (textmate-mode)))))

(el-get)
