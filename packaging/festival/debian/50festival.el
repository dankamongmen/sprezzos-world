;; Autoloads (most probable entry points) for Festival

(autoload 'say-minor-mode "festival" "Menu for using Festival." t)

(autoload 'run-festival "festival"
  "Run an inferior FESTIVAL process, input and output via buffer *festival*." t)

(autoload 'festival-say-buffer "festival"
  "Send given region to festival for saying.  This saves the region
as a file in /tmp and then tells festival to say that file.  The
major-mode is passed as a text mode to Festival." t)

(autoload 'festival-say-region "festival"
  "Send given region to festival for saying.  This saves the region
as a file in /tmp and then tells festival to say that file.  The
major mode is *not* passed as text mode name to Festival." t)

(autoload 'festival-say-string "festival"
  "Send string to festival and have it said" t)
