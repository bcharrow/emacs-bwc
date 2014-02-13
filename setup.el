; Install / update packages needed
; emacs --no-init-file --script setup.el
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar required-packages
  '(markdown-mode protobuf-mode yaml-mode multiple-cursors expand-region
    dummy-h-mode magit yasnippet
    )
  "List of packages needs to be installed at launch")

(package-refresh-contents)

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; Update / remove old packages
(package-list-packages)
(package-menu-mark-upgrades)
(package-menu-mark-obsolete-for-deletion)
(package-menu-execute)
