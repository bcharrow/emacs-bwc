(require 'cc-mode)
(add-hook 'c++-mode-hook
          (lambda()
            (define-key c++-mode-map "\r" 'newline-and-indent)))
(setq c-basic-offset 4)