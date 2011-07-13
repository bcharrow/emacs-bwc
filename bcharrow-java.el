(add-hook 'java-mode-hook
          (lambda()
            (define-key java-mode-map "\r" 'newline-and-indent)))

