;============================== Load Utilities ===============================;
; Only apply a function if the function is defined
(defun safe-apply (fn args)
  (if (fboundp fn)
      (apply fn args)))

;================================= Packages ==================================;
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

;=============================== GENERIC EMACS ===============================;
(setq make-backup-files nil)        ; Disable backups
(safe-apply 'tool-bar-mode '(-1))   ; hide the {tool,scroll,menu} bar
(safe-apply 'menu-bar-mode '(-1))
(safe-apply 'scroll-bar-mode '(-1))
(setq-default indent-tabs-mode nil) ; indent with spaces instead of tabs
(column-number-mode t)              ; Display column numbers
(setq-default fill-column 79)       ; Column width = 79

(set-cursor-color "gray")

;; new font
(if (equal 'darwin system-type)
    (progn
      (setq ns-antialias-text nil)
      (set-frame-font "Monaco 10"))
  (set-frame-font "Dejavu Sans Mono-10"))

; Make it unlikely that windows are split automatically
(setq split-height-threshold nil)
; (setq split-width-threshold 100)

;; save a list of open files in ~/.emacs.desktop
(desktop-save-mode 1)
(setq desktop-save 'ask-if-new)
(setq desktop-globals-to-save
      (append '((file-name-history        . 100)
                (query-replace-history    . 60)
                (compile-history          . 30)
                )))

;=================================== MODES ===================================;
(require 'ido)
(ido-mode t)

(auto-fill-mode t) ;; have things automatically wrap

;;; Tramp tips obtained from http://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "rsyncc")

; Check disk for changes to buffers every 2s
(global-auto-revert-mode t)
(setq auto-revert-interval 2)
(auto-revert-set-timer)

(setq remote-shell-program "/usr/bin/ssh")

;; disable vc
(defun vc-svn-registered (file) nil)
(defun vc-git-registered (file) nil)

(add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)

;============================= CUSTOM FUNCTIONS ==============================;
;; Fullscreen editing
(defun fullscreen ()
 (interactive)
 (set-frame-parameter nil 'fullscreen
                      (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;; make those oh so pretty separators
(defun insert-header (header)
  "Inserts a comment header at the current point.

Note that. 'comment-end and 'comment-start' specified by the
current major mode.  Also, 'fill-column' to determine the total
width of the header"
  (interactive "sEnter header: ")

  ;; Copied from newcomment.el; ask user specify comment syntax if none exists
  (unless comment-start
      (let ((cs (read-string "No comment syntax is defined.  Use: ")))
	(if (zerop (length cs))
	    (error "No comment syntax defined")
	  (set (make-local-variable 'comment-start) cs))))
  (let* (;; Comment syntax for the start and end of the string.  Remove spaces.
         (start (replace-regexp-in-string " " "" comment-start))
         (end (if (zerop (length comment-end))
                  (apply 'string (reverse (string-to-list start)))
                  (replace-regexp-in-string " " "" comment-end)))
         ;; If it's not possible to make things symmetric, add an extra '='
         (filler (if (zerop (% (- fill-column (length header)) 2)) "" "="))
         ;; What appears in the center.  If text, surround w/ spaces
         (body (if (zerop (length header)) "" (concat " " header " ")))
         ;; a long string of '=' chars
         (equal-str (make-string (/ (- fill-column (+ (length body)
                                                      (* 2 (length start))
                                                      (current-column)))
                                    2)
                                 ?=)))
    (insert (concat start equal-str body filler equal-str end "\n"))))

(defun two-frames-compile ()
  (interactive)
  (set-frame-size (selected-frame) 165 (display-pixel-height))
  (delete-other-windows)
  (compile "")
  (split-window-horizontally)
  (other-window 2)
  (shrink-window 1000)
  (enlarge-window 8)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (other-window 1))

(defun one-frame-compile ()
  (interactive)
  (set-frame-size (selected-frame) 80 (display-pixel-height))
  (delete-other-windows)
  (compile "")
  (other-window 1)
  (shrink-window 1000)
  (enlarge-window 8)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (other-window 1))

(defun sman()
  (interactive)
  (other-window 1)
  (call-interactively 'man))

(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;================================ Compilation ================================;
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-skip-threshold 2)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;============================= File Associations =============================;
(setq auto-mode-alist (cons '("\\.launch" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu" . c-mode) auto-mode-alist))

;=============================== KEY BINDINGS ================================;
;http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html
(global-set-key (kbd "<f6>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f7>") 'magit-status)
(global-set-key (kbd "<f9>") 'toggle-window-dedicated)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "C-c i") 'insert-header)
(global-set-key (kbd "<ESC> <RET>") 'two-frames-compile)
(global-set-key (kbd "C-<return>") 'one-frame-compile)

(require 'windmove)
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;=================================== C/C++ ===================================;
(require 'cc-mode)
(add-hook 'c-mode-hook
          (lambda()
            (define-key c-mode-map "\r" 'newline-and-indent)))

(add-hook 'c++-mode-hook
          (lambda()
            (define-key c++-mode-map "\r" 'newline-and-indent)))

(setq c-basic-offset 2)
(setq c-default-style "linux")
(setq tab-width 2)

;================================== Python ===================================;
(require 'python)
(add-hook 'python-mode-hook
          (lambda()
            (define-key python-mode-map "\r" 'newline-and-indent)))
(define-key python-mode-map "\C-cp"
  (lambda () (interactive) (insert "import pdb; pdb.set_trace()")))

;=================================== Latex ===================================;
(add-hook 'latex-mode-hook
          (lambda ()
            (progn
              (set (make-local-variable 'compile-command)
                   (concat "rubber -d "
                           (file-name-nondirectory buffer-file-name)))
              (visual-line-mode t)
              (define-key latex-mode-map (kbd "<ESC> <RET>") 'two-frames-compile)
              (define-key latex-mode-map (kbd "C-<return>") 'one-frame-compile)
              )))

;=================================== Magit ===================================;
(require 'magit)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (add-hook 'magit-log-edit-mode-hook
               '(lambda () (setq fill-column 74) (auto-fill-mode t)))
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;================================= yaml-mode =================================;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;================================= org-mode ==================================;
(require 'org)
(add-hook 'org-mode-hook
          (lambda()
            (visual-line-mode t)
            (org-indent-mode t)))

;================================= nxml-mode =================================;
(require 'mz-comment-fix)
(add-to-list 'comment-strip-start-length (cons 'nxml-mode 3))
(add-hook 'nxml-mode-hook
          '(lambda ()
             (define-key nxml-mode-map "\r" 'newline-and-indent)))
