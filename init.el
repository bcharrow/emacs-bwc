; Add vendor directory to path
(progn (let ((dir default-directory))
	 (cd "~/.emacs.d/vendor")
	 (normal-top-level-add-subdirs-to-load-path)
	 (cd dir)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(c-offsets-alist (quote ((innamespace . 0))))
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
                                      (:background "gray10"))))
             '(magit-item-highlight ((((class color) (background dark))
                                      (:background "gray10"))))
             '(diff-removed 
               ((t (:inherit diff-changed :background "gray20"))))
             '(diff-removed-face 
               ((t (:inherit diff-changed :background "gray20"))) t)
             '(diff-added 
               ((t (:inherit diff-changed :background "gray20"))))
             '(diff-added-face 
               ((t (:inherit diff-changed :background "gray20"))) t)
             '(diff-file-header 
               ((t (:inherit diff-changed :background "gray35"))))
             '(diff-file-header-face 
               ((t (:inherit diff-changed :background "gray35"))) t)
             '(diff-header 
               ((t (:inherit diff-changed :background "gray25"))))
             '(diff-header-face 
               ((t (:inherit diff-changed :background "gray25"))) t)
             )))


(load-file "~/.emacs.d/bcharrow-standard.el")


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
