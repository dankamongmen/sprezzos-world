;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian latex-cjk-common package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The latex-cjk package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(let ((package-dir (concat "/usr/share/"
                           (symbol-name flavor)
                           "/site-lisp/latex-cjk-common")))
  (when (file-directory-p package-dir)
        (setq load-path (cons package-dir load-path))))

(load-library "cjk-enc")
;!;(global-set-key "\C-c \C-x" 'cjk-write-file) ; CTRL-C SPC CTRL-X writes CJK file
