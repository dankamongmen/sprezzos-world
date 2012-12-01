(if (not (file-exists-p "/usr/share/emacs/site-lisp/gettext"))
  (message "Package gettext-el removed but not purged. Skipping setup.")
(setq auto-mode-alist
(cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "gettext/po-mode"
  "Major mode for translators to edit PO files" t)
(autoload 'po-find-file-coding-system "gettext/po-mode")
(unless (featurep 'po-find-file-coding-system)
  (autoload 'po-find-file-coding-system "gettext/po-compat"))
(if (fboundp 'modify-coding-system-alist)
  (modify-coding-system-alist 'file "\\.po[tx]?\\'\\|\\.po\\."
    'po-find-file-coding-system))
)
