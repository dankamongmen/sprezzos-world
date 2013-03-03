;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux ocaml package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The ocaml package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(let ((package-dir (concat "/usr/share/"
                           (symbol-name debian-emacs-flavor)
                           "/site-lisp/ocaml-mode")))
;; If package-dir does not exist, the #PACKAGE# package must have
;; removed but not purged, and we should skip the setup.
  (when (file-directory-p package-dir)
    (if (fboundp 'debian-pkg-add-load-path-item)
	(debian-pkg-add-load-path-item package-dir)
      (setq load-path (cons package-dir load-path)))
    (setq auto-mode-alist
          (cons '("\\.ml[iylp]?\\'" . caml-mode) auto-mode-alist))
    (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
    (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)))
