(require 'cc-mode)
(defun c-compile ()
  "Use make to compile"
  (interactive)
  (compile (concat "python " (buffer-name))))
(setq compilation-scroll-output t)
 (add-hook 'python-mode-hook (lambda ()
   (local-set-key "\C-c\C-c" 'py-compile)))
