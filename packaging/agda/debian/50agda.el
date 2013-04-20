;; This conffile is left around if the package is removed but not purged.
;; agda-mode is also not compatible with XEmacs so let's case against loading there

(when (and (file-exists-p "/usr/share/emacs/site-lisp/agda/agda2.el") (not (string-match "XEmacs" emacs-version)))
    (debian-pkg-add-load-path-item "/usr/share/emacs/site-lisp/agda/")
    (load "agda2")
)
