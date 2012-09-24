;(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (let ((dir default-directory))
	 (cd "~/.emacs.d/vendor")
	 (normal-top-level-add-subdirs-to-load-path)
	 (cd dir)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ack-project-root-file-patterns (quote (".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'" "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'")))
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(show-paren-mode t nil (paren))
 '(tex-suscript-height-ratio 0.9))

(add-hook 'term-setup-hook
          (lambda ()
            (custom-set-faces

             ;; custom-set-faces was added by Custom.
             ;; If you edit it by hand, you could mess it up, so be careful.
             ;; Your init file should contain only one such instance.
             ;; If there is more than one, they won't work right.
             '(default ((t (:background "black"
                            :foreground "gray"
                            :inverse-video nil
                            :box nil
                            :strike-through nil
                            :overline nil
                            :underline nil
                            :slant normal))) t)
             '(magit-item-highlight ((((class color) (background dark))
                                      (:background "gray13")))))))

(load-file "~/.emacs.d/safe-load.el")
;(safe-load "~/.emacs.d/vendor/auto-complete.el")
(safe-load "~/.emacs.d/bcharrow-standard.el" nil t)
(safe-load "~/.emacs.d/bcharrow-python.el" nil t)
(safe-load "~/.emacs.d/bcharrow-c.el" nil t)
(safe-load "~/.emacs.d/bcharrow-cpp.el" nil t)
(safe-load "~/.emacs.d/bcharrow-matlab.el" nil t)
(safe-load "~/.emacs.d/bcharrow-latex.el" nil t)
(safe-load "~/.emacs.d/bcharrow-java.el" nil t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
