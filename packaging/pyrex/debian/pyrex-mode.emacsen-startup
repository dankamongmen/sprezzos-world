;; -*-emacs-lisp-*-
;;
;; Emacs startup file, e.g.  /etc/emacs/site-start.d/50pyrex-mode.el
;; for the Debian pyrex-mode package
;;
;; The pyrex-mode package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...). The compiled code is then installed
;; in a subdirectory of the respective site-lisp directory.

;; If package-dir does not exist, the pyrex-mode package must have
;; been removed but not purged, and we should skip the setup.
(if (not (file-exists-p "/usr/share/emacs/site-lisp/pyrex-mode.el"))
  (message "Package pyrex-mode removed but not purged. Skipping setup")

;; Otherwise, autoload the pyrex-mode for the following extensions 
  (autoload 'pyrex-mode "pyrex-mode" "Pyrex editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.pyx$" . pyrex-mode))
  (add-to-list 'auto-mode-alist '("\\.pxd$" . pyrex-mode))
  (add-to-list 'auto-mode-alist '("\\.pxi$" . pyrex-mode))

;; endif
)
