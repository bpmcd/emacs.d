(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-a") 'back-to-indentation)
(define-key paredit-mode-map (kbd "M-m") 'move-beginning-of-line)
(let ((paredit-modes '(clojure
                       emacs-lisp
                       lisp
                       lisp-interaction
                       ielm
                       scheme)))
  (dolist (mode paredit-modes)
          (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                    (lambda () (paredit-mode +1)))))
