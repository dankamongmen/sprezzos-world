;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux autoconf package

(if (not (file-exists-p "/usr/share/emacs/site-lisp/autoconf"))
    (message "Package autoconf removed but not purged.  Skipping setup.")
  ;; To avoid a dependency on emacsen for our modes and avoid having a
  ;; separate autoconf-el package, we don't byte-compile the .el
  ;; files, so we only need to add a source directory to load-path.
  (debian-pkg-add-load-path-item
   (concat "/usr/share/emacs/site-lisp/autoconf"))

  ;; autoloads for autotest-mode.el
  (autoload 'autotest-mode "autotest-mode"
    "Major mode for editing autotest files." t)
  (setq auto-mode-alist
        (cons '("\\.at\\'" . autotest-mode) auto-mode-alist)))
