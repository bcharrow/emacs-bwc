;=============================== GENERIC EMACS ===============================;

(setq make-backup-files nil)        ; Disable backups
(tool-bar-mode -1)                  ; hide the tool bar
(setq-default indent-tabs-mode nil) ; indent with spaces instead of tabs

(column-number-mode t)              ; Display column numbers
(setq-default fill-column 79)       ; Column width = 79

;; new font
(set-frame-font "Droid Sans Mono 8")
;; (setq default-frame-alist '((font-backend . "xft")
;;                             (font . "Droid Sans Mono 10")
;;                             (cursor-color . "white")))
;; feel vim users pain!
;; (setq-default show-trailing-whitespace f)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;=================================== MODES ===================================;
(require 'ido)
(ido-mode t)

(auto-fill-mode t) ;; have things automatically wrap

;;; Tramp tips obtained from http://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "ssh")

;; disable vc-svn
(defun vc-svn-registered (file) nil)

;;; Easy movement between windows with 'windows' key + arrow
(require 'windmove)
(windmove-default-keybindings 'super)


;============================= CUSTOM FUNCTIONS ==============================;
;; Fullscreen editing
(defun fullscreen ()
 (interactive)
 (set-frame-parameter nil 'fullscreen
                      (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;; make those oh so pretty separators
(defun insert-header (header)
  "Inserts a comment header at the current point.

   Note that. 'comment-end and 'comment-start' specified by the current major
   mode.  Also, 'fill-column' to determine the total width of the header"
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
(global-set-key "\C-ci" 'insert-header) ;; bind the function

(defun split-quads ()
  (interactive)
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally))

;=============================== KEY BINDINGS ================================;
(global-set-key [f7] 'save-buffer)
(global-set-key [f9] 'compile)
(global-set-key [f5] 'fullscreen)
(global-set-key [f6] 'split-quads)
(global-set-key "\C-ci" 'insert-header)

;================================== COMPILE ==================================;
(setq compilation-window-height 8) ;; comp window only takes up 8 rows
;; (setq compilation-finish-function  ;; close comp window if successfully compile
;;       (lambda (buf str)
;;         (if (string-match "exited abnormally" str)
;;             ;;there were errors
;;             (message "compilation errors, press C-x ` to visit")
;;           ;;no errors, make the compilation window go away in 0.5 seconds
;;           (run-at-time 0.5 nil 'kill-buffer buf)
;;           (message "Compiled succesfully"))))
