(require 'python)
; pep-8 compliant
(setq-default fill-column 79)       ; Column width = 79

;;;   (require 'show-wspace)
;; ;;;   (show-ws-toggle-show-tabs) ;; show evil tabs

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq pymacs-load-path '("~/.emacs.d/rope/rope"
                         "~/.emacs.d/ropemacs/ropemacs"))

(add-hook 'python-mode-hook
          (define-key python-mode-map "\r" 'newline-and-indent))

;================================= DEBUGGING =================================;
;;http://www.emacswiki.org/emacs/PdbNotes
;;pdb setup, note the python version
;; (setq pdb-path '/usr/lib/python2.5/pdb.py
;;       gud-pdb-command-name (symbol-name pdb-path))
;; (defadvice pdb (before gud-query-cmdline activate)
;;   "Provide a better default command line when called interactively."
;;   (interactive
;;    (list (gud-query-cmdline pdb-path
;; 	 		    (file-name-nondirectory buffer-file-name)))))

;================================= ROPEMACS ==================================;
;; Initialize Rope & Pymacs whenever python is opened
;; Inspired/copied from http://www.cse.ust.hk/~lars/emacs-dotemacs.html
(defun load-ropemacs ()
  (interactive)
;;   (setenv "PYMACS_PYTHON" "python2.5")
  (autoload 'pymacs-load "pymacs" nil t)
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs")
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t) ;; use rope-generate-autoimport-cache
  (setq ropemacs-guess-project t)     ;; Guess project location
  (setq ropemacs-confirm-saving 'nil) ;; Auto-save before refactoring
  (ropemacs-mode t))
(add-hook 'python-mode-hook 'load-ropemacs)

;================================= YASNIPPET =================================;
;; Initialize Yasnippet
;Don't map TAB to yasnippet
;In fact, set it to something we'll never use because
;we'll only ever trigger it indirectly.
;; (defun load-ysnippet ()
;;   (interactive)
;;   (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
;;   (yas/initialize)
;;   (yas/load-directory "~/.emacs.d/snippets"))
;; (add-hook 'python-mode-hook 'load-ysnippet)

;============================== AUTOCOMPLETION ===============================;
;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;       (setq value (cons (format "%s%s" prefix element) value))))))
;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")
;; (defun ac-python-find ()
;;   "Python `ac-find-function'."
;;   (require 'thingatpt)
;;   (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;     (if (null symbol)
;;         (if (string= "." (buffer-substring (- (point) 1) (point)))
;;             (point)
;;           nil)
;;       symbol)))
;; (defun ac-python-candidate ()
;;   "Python `ac-candidates-function'"
;;   (let (candidates)
;;     (dolist (source ac-sources)
;;       (if (symbolp source)
;;           (setq source (symbol-value source)))
;;       (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;              (requires (cdr-safe (assq 'requires source)))
;;              cand)
;;         (if (or (null requires)
;;                 (>= (length ac-target) requires))
;;             (setq cand
;;                   (delq nil
;;                         (mapcar (lambda (candidate)
;;                                   (propertize candidate 'source source))
;;                                 (funcall (cdr (assq 'candidates source)))))))
;;         (if (and (> ac-limit 1)
;;                  (> (length cand) ac-limit))
;;             (setcdr (nthcdr (1- ac-limit) cand) nil))
;;         (setq candidates (append candidates cand))))
;;     (delete-dups candidates)))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;                  (auto-complete-mode 1)
;;                  (set (make-local-variable 'ac-sources)
;;                       (append ac-sources '(ac-source-yasnippet)  '(ac-source-rope)))
;;                  (set (make-local-variable 'ac-find-function) 'ac-python-find)
;;                  (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;                  (set (make-local-variable 'ac-auto-start) nil)))

;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))

;; ; Try the following:
;; ; 1) Do a yasnippet expansion
;; ; 2) Do a Rope code completion
;; (define-key python-mode-map [(ctrl .)] 'ac-start)

;; ;================================== FLYMAKE ==================================;
;; ; Snagged from http://www.plope.com/Members/chrism/flymake-mode
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; ;; Only use flymake for python
;; (add-hook 'python-mode-hook 'flymake-find-file-hook)

;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:background "LightPink" :foreground "black"))))
;;  '(flymake-warnline ((((class color)) (:background "LightBlue2" :foreground "black")))))


;================================== COMPILE ==================================;
(defun py-compile ()
  "Use compile to run python programs"
  (interactive)
  (setq compilation-window-height 16)
  (compile (concat "python " (buffer-name))))

(setq compilation-scroll-output t)
 (add-hook 'python-mode-hook (lambda ()
   (local-set-key "\C-c\C-c" 'py-compile)))

(define-key python-mode-map "\C-cp"
  (lambda () (interactive) (insert "import pdb; pdb.set_trace()")))
