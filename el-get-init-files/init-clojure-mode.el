(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(set-variable 'inferior-lisp-program "lein repl")
(add-hook 'clojure-mode-hook
          (lambda ()
            (define-clojure-indent
              (defroutes 'defun)
              (GET 2)
              (POST 2)
              (PUT 2)
              (DELETE 2)
              (HEAD 2)
              (ANY 2)
              (context 2))))