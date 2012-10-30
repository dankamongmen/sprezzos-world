;; -*-emacs-lisp-*-
;;
;; Emacs startup file, e.g.  /etc/emacs/site-start.d/50git-core.el
;; for the Debian git package


(let ((dir (concat "/usr/share/"
                   (symbol-name debian-emacs-flavor)
                   "/site-lisp/git")))
  (if (not (file-exists-p dir))
      (message "git removed but not purged, skipping setup")

    ;; debian-pkg-add-load-path-item as from debian 3.1 "sarge",
    ;; emacsen-common 1.4.14 of June 2002
    (debian-pkg-add-load-path-item dir)

    ;; Compatibility note: In debian git-core 1:1.7.0-1 there was a long
    ;; list of generated autoloads here, but now pruned back to the
    ;; interactive entrypoints.  If you were using those autoloads in elisp
    ;; code, don't do that, use (require 'git) to express the dependency.

    ;; git.el

    (autoload 'git-status "git" "Entry point into git-status mode." t)

    ;; git-blame.el

    ;; this autoload as recommended by git-blame.el comments
    (autoload 'git-blame-mode "git-blame"
      "Minor mode for incremental blame for Git." t)))

;; End of file
