;;;;;;;;;;;;;;;
;;;; PATH ;;;;;
;;;;;;;;;;;;;;;

(push "/usr/local/bin" exec-path)
(push "/Users/bobby/local/bin" exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/bobby/local/bin"))

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
  (untabify-buffer)
  (delete-trailing-whitespace))

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
(blink-cursor-mode t)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(ido-mode t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;;(global-hl-line-mode t)

(unless window-system (setq linum-format "%d "))

(when window-system
      (tool-bar-mode -1)
      (scroll-bar-mode t)
      (normal-erase-is-backspace-mode 1)
      (set-fringe-style -1)
      (tooltip-mode -1)
      (set-bar-cursor)

      ;; Mac OS X conditional preferences
      (unless (string-match "apple-darwin" system-configuration)
              (menu-bar-mode -1)
              (set-frame-font "Monospace-10"))

      (when (string-match "apple-darwin" system-configuration)
            (setq mac-allow-anti-aliasing t)
            (set-face-font 'default "Anonymous Pro-20")))

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

;;;;;;;;;;;;;;;;
;;;; EL-GET ;;;;
;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
        (with-current-buffer
         (url-retrieve-synchronously
          "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
         (goto-char (point-max))
         (eval-print-last-sexp)))

(setq el-get-sources
      '((:name align-cljlet
               :type github
               :pkgname  "gstamp/align-cljlet"
               :depends  clojure-mode
               :load     "align-cljlet.el"
               :features align-cljlet)
        (:name nrepl
               :depends clojure-mode)
        (:name nrepl-ritz
               :type elpa
               :depends (nrepl))
        (:name gambit-mode
               :type github
               :pkgname "feeley/gambit"
               :load "misc/gambit.el")))

(setq my-packages
      (append
       '(paredit clojure-mode
                 haml-mode sass-mode
                 color-theme color-theme-solarized kpm-list magit
                 org-mode markdown-mode textile-mode
                 coffee-mode
                 ruby-mode inf-ruby ruby-compilation rhtml-mode yaml-mode ruby-end)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
