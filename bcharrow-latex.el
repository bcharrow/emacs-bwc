(require 'yasnippet)

(defun load-ysnippet ()
  (interactive)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/snippets"))
(add-hook 'LaTeX-mode-hook 'load-ysnippet)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

