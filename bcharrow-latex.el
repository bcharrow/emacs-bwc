;================== Commands for converting word documents ===================;

(defun sub-pairs (string pairs)
  "Transform 'string' using regexps and return the result.

'pairs' is an asoc list whose elements are (rep . string)"
  (if (eq nil pairs)
      string
    (sub-pairs
     (replace-regexp-in-string (car (car pairs)) (cdr (car pairs)) string t)
     (cdr pairs))))


(defun extract-region ()
  "Extract and delete the current region and then return it"
  (defun er (beg end)
    (interactive (list (point) (mark)))
    (unless (and beg end)
      (error "The mark is not set now, so there is no region"))
    (delete-and-extract-region beg end))
  (call-interactively 'er))

(setq latex-math-asoc
      (list '("\\[" . "\\\\left[")
            '("\\]" . "\\\\right]")))

(setq latex-standard-asoc
      (list '("–" . "-")
            '("−" . "-")
            '("”" . "''")
            '("“" . "``")
            '("→" . "\\\\longrightarrow")
            '("%" . "\\\\\\&")
            '("∙" . " \\\\times ")
            '("×" . " \\\\times ")
            '("*" . "\\\\times ")
            '("’" . "'")
            '("MAX " . "\\\\max")
            '("\\$" . "\\\\$")
            '("\\\\\\$\\\\\\$" . "$$")
            '("σ" . "\\\\sigma")
            '("μ" . "\\\\mu")
            '("≤" . "\\\\leq")
            '("Δ" . "\\\\Delta")
            '("Γ" . "\\\\Gamma")
            '("θ" . "\\\\theta")))

(defun fix-word-math ()
  "Convert the highlighted region from microsoft word math to something
approaching TeX in an eqalign environment"
  (interactive)
  (defun alignment (string)
    (interactive "sEnter regexp to align on: ")
    (if (zerop (length string))
        nil
      string))

  (defun subscript (string)
    (interactive "sEnter sequence for subscript: ")
    (let ((off "[-+]*[0-9]*")
          (vars (concat "[" string "]\\{1,\\}")))
      (if (zerop (length string))
          nil
        (concat vars off))))

  (let* ((al (call-interactively 'alignment))
         (sub (call-interactively 'subscript))
         (asoc (append latex-math-asoc latex-standard-asoc)))
    (when al
      (push (cons al "&\\&") asoc)
      (push '("$" . " \\\\cr") asoc))
    (when sub (push (cons sub "_{\\&}") asoc))
    (let* ((region (extract-region)))
      (insert (sub-pairs region asoc)))))

(defun eqn (string)
  "Surround equations like 'a = 4' with $"
  (replace-regexp-in-string "[\\./\\\\\\$a-zA-Z\\*0-9{}_,-\\+]* *= *[\\sw0-9\\.\\(\\)A-Za-z-_{}/\\$,-\\\\%\\+]*" "$\\&$"
                                    string))

(defun leq (string)
  "Surround equations like '1234.23<334' with $"
  (replace-regexp-in-string "[0-9\\.<>{}a-zA-Z_\\(\\)-]* *\\(<\\|>\\|\\leq\\|\\geq\\)+ *[0-9\\.<>{}a-zA-Z_\\(\\)-]*" "$\\&$" string))

(defun fix-word-text ()
  "Convert the highlighted region from microsoft word text
containing math to something approaching TeX words with math
embedded in it"
  (interactive)
  (let* ((region (extract-region)))
    (insert (leq (eqn (sub-pairs region latex-standard-asoc))))))

(defun parts ()
  "Replae text like 'Part a.' with '\expart'"
  (interactive)
  (insert (replace-regexp-in-string "^Part [a-z]." "\\\\expart"
                                    (extract-region))))


(defun eqalign ()
  "Put region into an eqalignno environment"
  (interactive)
  (insert (concat "$$\\eqalignno{\n" (extract-region) "\n}$$")))


;=================================== Hooks ===================================;
;(require 'yasnippet)

;(defun load-ysnippet ()
;  (interactive)
;  (yas/initialize)
;  (yas/load-directory "~/.emacs.d/snippets"))
;(add-hook 'LaTeX-mode-hook 'load-ysnippet)

;; (add-hook 'LaTeX-mode-hook
;;           (lambda()
;;             (define-key LaTeX-mode-map "\r" 'newline-and-indent)))

(add-hook 'LaTeX-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'LaTeX-mode-hook
          (lambda()
            (local-set-key "\C-b" 'fix-word-text)
            (local-set-key "\C-n" 'fix-word-math)
            (local-set-key "\C-c p" (lambda () (insert "")))))
(add-hook 'LaTeX-mode-hook
          (lambda()
            (define-key LaTeX-mode-map "\C-cp"
              (lambda () (interactive) (insert "$$\\includegraphics{placeholder}$$")))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "rubber -d "
                         (file-name-nondirectory buffer-file-name)))))
