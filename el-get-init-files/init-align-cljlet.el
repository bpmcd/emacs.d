(print "initializing align-cljlet...")
(add-hook 'clojure-mode-hook
          (lambda ()
            (print "binding 'C-c |' to 'align-cljlet...")
            (define-key clojure-mode-map (kbd "C-c |") 'align-cljlet)
            (print "...finished binding 'C-c |' to 'align-cljlet")))
(print "...finished initializing align-cljlet")
