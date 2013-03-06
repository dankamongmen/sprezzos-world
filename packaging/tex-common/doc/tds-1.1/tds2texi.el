;;; tds2texi.el --- convert TDS draft from LaTeX to Texinfo

;; Copyright (C) 1996, 1999, 2003, 2004 Ulrik Vieth and the TeX Users Group.

;; This programm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have a copy of the GNU General Public License; see the
;; file COPYING.  If not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


;; Author: Ulrik Vieth <vieth@thphy.uni-duesseldorf.de>
;; Maintainer: karl@mail.tug.org
;; Version: $Id: tds2texi.el,v 1.12 2004/06/04 17:25:49 karl Exp $
;; Keywords: TDS, LaTeX, Texinfo, Info, HTML, tex, maint

;;; Commentary:

;; Description:
;;
;; This file provides a limited LaTeX-to-Texinfo conversion function
;; `tds2texi-convert' which is intended to convert the LaTeX
;; source of the TDS draft document `tds.tex', using a couple of
;; special markup tags defined in the document class `tdsguide.cls'.
;; It is definitely *not* suitable to be used as a general-purpose
;; LaTeX-to-Texinfo converter and is not intended to be used as such.
;;

;; Usage (also see the Makefile):
;;
;; 0. load or autoload this file, e.g: (load-library "tds2texi")
;;
;; 1. go to a directory containing `tds.tex', e.g. in dired mode
;;
;; 2. run M-x tds2texi-convert -- this does all the conversion and
;;    leaves you in a buffer `tds.texi' containing the converted file
;; 
;; 3. edit the buffer if necessary and save it -- you will be asked
;;    for a file name (which should be `tds.texi', of course)
;; 
;; 4. run M-x makeinfo-buffer or invoke makeinfo and/or texi2html
;;    from the shell -- don't try texi2dvi, it may work all right,
;;    but it's not recommended since it will only cause confusion
;;    with the DVI file created from the original LaTeX source!
;;

;; Bugs:
;;
;; * whitespace (esp. blank lines) before and after environments
;;   will be inconsistent (should be fixed in the LaTeX version!)
;;
;; * whitespace (esp. indentation) in tdsSummary environments
;;   will be inconsistent (should be fixed in the LaTeX version!)
;;
;; * labels are references are assumed to coincide with names
;;   of @nodes -- this is not always true, e.g. for MF vs. \MF{}
;;   
;; * the last comma in the list of contributors should be a period
;;
;; * the list of contributors should be handled in a better way
;;   (currently: LaTeX tabbing -> @quotation -> HTML <blockquote>)
;;
;; * the contents of the top node may get lost in the texi2html
;;   conversion under some circumstances, or else it may end up
;;   in the wrong split file -- this seems to be a texi2html bug
;;   

;; History:
;;
;; v 0.0 -- 1996/01/23  UV  created
;; v 0.1 -- 1996/01/24  UV  first rough version, posted to twg-tds
;; v 0.2 -- 1996/01/24  UV  added some commentary and doc strings
;; v 0.3 -- 1996/01/25  UV  modularized code, handle header and trailer,
;;                          call texinfo routines for @nodes and @menus
;; v 0.4 -- 1996/01/27  UV  slightly touched-up and some doc fixes,
;;                          improved handling of legalnotice header 
;; v 0.5 -- 1996/01/28  UV  more documentation added, code freeze
;;
;; v 1.0 -- 1996/02/20  UV  renamed to tds2texi, bumped version number,
;;                          also fixed a minor typo in "@item samp"
;; v 1.1 -- 1996/11/12  KB  no node pointers, no double blank line after
;;                          sections, allow braces after abbrevs.
;; v 1.2 -- 1996/11/16  KB  ??? (see ChangeLog)
;; v 1.3 -- 1996/11/18  UV  added translation for umlaut accents,
;;                          added tds2texi-direntry header lines,
;;                          added function tds2texi-fixup-texinfo,
;;                          fixed bug when combining multiple "@file"s.


;;; Code:

(require 'texinfo)	; needed to update @nodes and @menus

;; file name variables

(defvar tds2texi-source-file "tds.tex"
  "File name of TDS LaTeX source to be converted.")

(defvar tds2texi-target-file "tds.texi"
  "File name of TDS Texinfo source to be created.")

(defvar tds2texi-filename "tds.info"
  "File name of Info file to be inserted in Texinfo header.")

(defvar tds2texi-direntry
  (concat
   "@dircategory TeX\n"
   "@direntry\n"
   "* TeX Directories: (tds).       A directory structure for TeX files.\n"
   "@end direntry\n"))

;; translation tables

(defvar tds2texi-logos-alist
  '(("{\\TeX}"       . "TeX")		; when only doing Info
    ("\\TeX{}"       . "TeX")		; no need to use "@TeX{}"
    ("{\\LaTeX}"     . "LaTeX")
    ("\\LaTeX{}"     . "LaTeX")
    ("{\\LaTeXe}"    . "LaTeX2e")
    ("\\LaTeXe{}"    . "LaTeX2e")
    ("{\\AmS}"       . "AMS")
    ("\\AmS{}"       . "AMS")
    ("{\\AMSTeX}"    . "AMS-TeX")
    ("\\AMSTeX{}"    . "AMS-TeX")
    ("\\MF{}"        . "Metafont")
    ("\\MP{}"        . "MetaPost")
    ("\\BibTeX{}"    . "BibTeX")
    ("{\\iniTeX}"    . "INITEX")
    ("\\iniTeX{}"    . "INITEX")
    ("{\\iniMF}"     . "INIMF")
    ("\\iniMF{}"     . "INIMF")
    ("{\\iniMP}"     . "INIMP")
    ("\\iniMP{}"     . "INIMP")
    ("{\\PS}"        . "PostScript")
    ("\\PS{}"        . "PostScript")
    ("{\\copyright}" . "@copyright{}")
    )
  "List of TeX logos and their replacement text after conversion.")

(defvar tds2texi-logos-regexp-1 "\\(\\\\[A-Za-z]+{}\\)"
  "Regexp for TeX logos to be converted using `tds2texi-logos-alist'.")

(defvar tds2texi-logos-regexp-2 "\\({\\\\[^}]+}\\)"
  "Regexp for TeX logos to be converted using `tds2texi-logos-alist'.")

;;

(defvar tds2texi-tags-alist
  '(("\\emphasis"    . "@emph")
    ("\\citetitle"   . "@cite")
    ("\\literal"     . "@file")
    ("\\replaceable" . "@var")
    ("\\command"     . "@code")			; defined, but not used
    ;; ("\\application" . "@r")			; unnecessary, but ...
    ;; ("\\abbr"        . "@sc")		; unnecessary, but ...
    )
  "List of markup tags and their replacement text after conversion.")

(defvar tds2texi-tags-regexp "\\(\\\\[a-z]+\\)"
  "Regexp for markup tags to be converted using `tds2texi-tags-alist'.")

;;

(defvar tds2texi-env-alist
  '(("\\begin{ttdisplay}"           . "@example")
    ("\\end{ttdisplay}"             . "@end example")
    ("\\begin{tdsSummary}"          . "@example")
    ("\\end{tdsSummary}"            . "@end example")
    ("\\begin{enumerate}"           . "@enumerate")
    ("\\end{enumerate}"             . "@end enumerate")
    ("\\begin{enumerate-squeeze}"   . "@enumerate")
    ("\\end{enumerate-squeeze}"     . "@end enumerate")
    ("\\begin{itemize}"             . "@itemize @bullet")
    ("\\end{itemize}"               . "@end itemize")
    ("\\begin{itemize-squeeze}"     . "@itemize @bullet")
    ("\\end{itemize-squeeze}"       . "@end itemize")
    ("\\begin{description}"         . "@itemize @bullet")
    ("\\end{description}"           . "@end itemize")
    ("\\begin{description-squeeze}" . "@itemize @bullet")
    ("\\end{description-squeeze}"   . "@end itemize")
    ("\\begin{legalnotice}"         . "@titlepage")	; special hack
    ("\\end{legalnotice}"           . "@end titlepage")
    ("\\begin{tabbing}"             . "@quotation")	; special hack
    ("\\end{tabbing}"               . "@end quotation")
    )
  "List of environments and their replacement text after conversion.")

(defvar tds2texi-env-regexp "\\(\\\\\\(begin\\|end\\){[^}]+}\\)"
  "Regexp for environments to be converted using `tds2texi-env-alist'.")


;;; some utility functions

(defun tds2texi-string-replace (x-string x-replace)
  "Searches for occurences of X-STRING, replacing them by X-REPLACE."
  (save-excursion
    (while (search-forward x-string nil t)
      (replace-match x-replace t t)))) 		; use fixed case!

(defun tds2texi-regexp-replace (x-regexp x-replace)
  "Searches for occurences of X-REGEXP, replacing them by X-REPLACE."
  (save-excursion
    (while (re-search-forward x-regexp nil t)
      (replace-match x-replace t nil)))) 	; use fixed case!


(defun tds2texi-alist-replace (x-regexp x-alist)
  "Searches for ocurrences of X-REGEXP, replacing them using X-ALIST.
If no match is found in X-ALIST, leaves the original text unchanged."
  (save-excursion
    (let (x-match 
	  x-replace)
      (while (re-search-forward x-regexp nil t)
	(setq x-match (match-string 1))
	(setq x-replace (or (cdr (assoc x-match x-alist)) x-match))
	(replace-match x-replace t t)))))	; use fixed case!


;;; the main conversion function

(defun tds2texi-convert ()
  "Have a try at converting LaTeX to Texinfo.  Good luck!"
  (interactive)

  ;; get a buffer to operate on and insert the LaTeX source
  (set-buffer (get-buffer-create tds2texi-target-file))
  (erase-buffer)
  (insert-file-contents-literally tds2texi-source-file)
  
  ;; tab characters can mess up tds-summary envrionments,
  ;; so get rid of them as soon as possible
  (untabify (point-min) (point-max))
  (goto-char (point-min))
  
  ;; do the conversion steps for the text body
  (tds2texi-do-simple-tags)	; mostly general
  (tds2texi-do-fancy-logos)	; mostly specific to TDS
  (tds2texi-do-sectioning)	; mostly general
  (tds2texi-do-markup-tags)	; mostly specific to TDS
  (tds2texi-do-environments)	; partly specific to TDS
  
  ;; do the conversion for header and trailer
  (tds2texi-do-header)		; partly specific to TDS
  (tds2texi-do-trailer)		; partly specific to TDS
  
  ;; standard Texinfo functions
  ; (texinfo-every-node-update) ; I don't like the extra node pointers.
  (texinfo-all-menus-update)
  (texinfo-master-menu nil)

  (tds2texi-fixup-texinfo)

  ;; all that's left to do is saving the buffer to a file
  ;; -- we simply select the buffer and leave saving it to
  ;; the user in case some manual intervention is needed
  (switch-to-buffer tds2texi-target-file)
  )


;;; various steps of the conversion process

(defun tds2texi-do-simple-tags ()
  "First step of \\[tds2texi-convert].  Not useable by itself."

  ;; literal `@' -- should come before anything else, since it's
  ;; the Texinfo control character.
  (tds2texi-regexp-replace "\\([^\\\\]\\)@" "\\1@@")
  
  ;; fancy spacing -- should come early before we have many `@'

  ;; "\@" -- space factor corrections before sentence end `.'
  (tds2texi-regexp-replace "\\\\@\\." "@.")
  ;; "\ " -- control space after `.' in the middle of sentences.  Also
  ;; converts \\ to \.
  (tds2texi-regexp-replace "\\.\\\\\\([ \n]+\\)" ".@:\\1")
  ;; "\ " -- control space used otherwise
  (tds2texi-regexp-replace "\\\\\\([ \n]+\\)" "\\1")

  ;; "\," -- thin space used with dimensions like "dpi" or "pt"
  (tds2texi-regexp-replace "\\\\,\\([a-z]+\\)" "@dmn{\\1}")

  ;; "\\" -- forced line breaks in references, where it's always
  ;; preceded by a :.  Previous "\ " conversion reduced \\ to \.
  (tds2texi-string-replace ":\\ " ":@*\n")

  ;; "\"" -- accents etc.:
  (tds2texi-string-replace "\\\"" "@\"")
  (tds2texi-string-replace "\\ss" "@ss")

  ;; special TeX characters that needn't be quoted in Texinfo:
  (tds2texi-string-replace "\\_" "_")
  (tds2texi-string-replace "\\&" "&")
  (tds2texi-string-replace "\\%" "%")

  ;; special TeX characters that we prefer to transliterate:
  (tds2texi-regexp-replace "\\\\slash[ ]*" "/")

  ;; we could translate $...$ into @math{...}, but why bother
  ;; when we can transliterate it easily?
  (tds2texi-string-replace "$" "")
  (tds2texi-string-replace "\\pm" "+-")
)


(defun tds2texi-do-fancy-logos ()
  "Second step of \\[tds2texi-convert].  Not useable by itself."

  ;; fancy TeX logos -- these are used in arguments of sections,
  ;; so we have to do them early before doing sectioning commands.
  (tds2texi-alist-replace tds2texi-logos-regexp-1 tds2texi-logos-alist)
  (tds2texi-alist-replace tds2texi-logos-regexp-2 tds2texi-logos-alist)
  
  ;; acronyms -- these are also used in arguments of sections.
  ;; There's no need to use @sc markup, just upcase the argument.
  (save-excursion
    (while (re-search-forward "\\\\abbr{\\([^}]+\\)}" nil t)
      (replace-match (upcase (match-string 1)) nil t)))
  
  ;; applications -- similar to acronyms, so done here as well.
  ;; There's no need to use @r markup either, it's the default!
  (tds2texi-regexp-replace "\\\\application{\\([^}]+\\)}" "\\1")
  )


(defun tds2texi-do-sectioning ()
  "Third step of \\[tds2texi-convert].  Not useable by itself."

  ;; first do @chapter and @appendix by narrowing
  (save-excursion
    (save-restriction
      (narrow-to-region 
       (point-min) (search-forward "\\appendix" nil t))
      (goto-char (point-min))
      (tds2texi-regexp-replace 
       "\\\\section{\\([^}]+\\)}[ ]*" "@node \\1\n@chapter \\1")
      ))
  (save-excursion
    (save-restriction
      (narrow-to-region 
       (search-forward "\\appendix" nil t) (point-max))
      (tds2texi-regexp-replace 
       "\\\\section{\\([^}]+\\)}[ ]*" "@node \\1\n@appendix \\1")
      ))
  
  ;; @section and @subsection are just shifted a level up
  (tds2texi-regexp-replace 
   "\\\\subsection{\\([^}]+\\)}[ ]*"    "@node \\1\n@section \\1")
  (tds2texi-regexp-replace 
   "\\\\subsubsection{\\([^}]+\\)}[ ]*" "@node \\1\n@subsection \\1")
  
  ;; now we no longer need \appendix as a marker
  (tds2texi-regexp-replace "\\\\appendix[ ]*\n" "")

  ;; \newpage can go as well
  (tds2texi-regexp-replace "%?\\\\newpage[ ]*\n" "")
  
  ;; \labels are redundant since we have @nodes
  (tds2texi-regexp-replace "\\\\label{sec:\\([^}]+\\)}[ ]*\n" "")

  ;; \refs now refer to @nodes instead of \labels
  (tds2texi-regexp-replace "\\\\ref{sec:\\([^}]+\\)}" "@ref{\\1}")
  )


(defun tds2texi-do-markup-tags ()
  "Fourth step of \\[tds2texi-convert].  Not usable by itself."

  ;; special tags -- for \CTAN, we must used fixed-case replace!
  (tds2texi-string-replace "\\the\\tdsVersion" "@value{version}")
  (tds2texi-string-replace "\\texmf{}" "@file{texmf}")
  (tds2texi-string-replace "\\CTAN:"   "@file{@var{CTAN}:}")
  
  ;; convert simple tags without expanding their arguments:
  ;; \emphasis, \citetitle, \literal, \replaceable
  (tds2texi-alist-replace tds2texi-tags-regexp tds2texi-tags-alist)
  
  ;; \\systemitem -- a silly tag with an extra argument that
  ;; isn't printed.  It is used exactly once!
  (tds2texi-regexp-replace
   "\\\\systemitem{\\([^}]+\\)}{\\([^}]+\\)}" "@file{\\2}")
  
  ;; \path, \url -- here we can't avoid shuffling the argument
  (tds2texi-regexp-replace "\\\\path|\\([^|]+\\)|" "@file{\\1}") 
  (tds2texi-regexp-replace "\\\\url|\\([^|]+\\)|" "@uref{\\1}") 
  (tds2texi-regexp-replace "\\\\email|\\([^|]+\\)|" "@email{\\1}") 
  
  ;; After turning \replaceable into @var above we now have to
  ;; turn @var{...} into @file{@var{...}} to get quotation marks
  ;; around file names consistent.  (Read: those extra quotation
  ;; marks inserted automatically by makeinfo in the @file tag.)

  ;; For simplicity we first do the change everywhere and then
  ;; undo it again inside `ttdisplay' environments, where we
  ;; can leave @var by itself as @file isn't used there anyway.

  (tds2texi-regexp-replace "@var{\\([^}]+\\)}" "@file{@var{\\1}}")
  (save-excursion
    (while (search-forward "\\begin{ttdisplay}" nil t)			     		   
      (save-restriction
	(narrow-to-region
	 (point) (search-forward "\\end{ttdisplay}" nil t))
	(goto-char (point-min))
	(tds2texi-regexp-replace "@file{@var{\\([^}]+\\)}}" "@var{\\1}")
	)))
  
  ;; eliminate redundant quotation marks around @file
  (tds2texi-regexp-replace "``\\(@file{[^}]+}\\)''" "\\1")
  (tds2texi-regexp-replace "`\\(@file{[^}]+}\\)'" "\\1")
  
  ;; ... and combine multiple @file{}s in one line
  (tds2texi-regexp-replace 
   "@file{\\([^ ]+\\)}@file{\\([^ ]+\\)}@file{\\([^ ]+\\)}" "@file{\\1\\2\\3}")
  (tds2texi-regexp-replace 
   "@file{\\([^ ]+\\)}@file{\\([^ ]+\\)}" "@file{\\1\\2}")

  ;; ... also simplify cases of nested @file{}s
  (tds2texi-regexp-replace 
   "@file{@file{\\([^ ]+\\)}\\([^ ]+\\)}" "@file{\\1\\2}")

  ;; literal `~' -- if it hasn't been converted to space earlier,
  ;; we can now do the conversion to @w{word1 word2} without
  ;; running the risk of confusion the regexp matcher somewhere.
  ;; Unfortunately @w will get lost again in the HTML conversion
  ;; because &nbsp; or &#160; are not yet standard HTML tags.
  (tds2texi-string-replace "~" "@tie{}")
  )


(defun tds2texi-do-environments ()
  "Fifth step of \\[tds2texi-convert].  Not useable by itself."
  
  ;; convert \begin and \end of environments
  (tds2texi-alist-replace tds2texi-env-regexp tds2texi-env-alist)

  ;; convert \items
  (tds2texi-string-replace "\\item"  "@item")

  ;; insert newlines after description items where appropriate
  (tds2texi-regexp-replace
   "@item\\[\\([^]]+\\)\\][ ]*\n" "@item \\1\n")
  (tds2texi-regexp-replace
   "@item\\[\\([^]]+\\)\\][ ]*"   "@item \\1\n")

  ;; insert newlines after @item @file, also replace @item @file
  ;; by @item @samp -- this is done because it may look nicer
  ;; in the HTML version, but it depends on the style sheet used
  (tds2texi-regexp-replace 
   "@item @file\\([^,\n]*\\),[ ]*" "@item @samp\\1,\n")
  (tds2texi-regexp-replace 
   "@item @file" "@item @samp")
  )


;;; handling the header and trailer

(defun tds2texi-do-header ()
  "Convert LaTeX header to Texinfo.  Used in \\[tds2texi-convert]."
  (let (title-string
	author-string
	version-string)

    ;; collect information
    (save-excursion
      (re-search-forward "\\\\tdsVersion{\\(.*\\)}" nil t)
      (setq version-string (match-string 1))
      (re-search-forward "\\\\title{\\(.*\\)}" nil t)
      (setq title-string (match-string 1))
      (re-search-forward "\\\\author{\\(.*\\)}" nil t)
      (setq author-string (match-string 1))
      )

    ;; discard information lines
    (tds2texi-regexp-replace "\\\\title{.*}[ ]*\n"         "")
    (tds2texi-regexp-replace "\\\\author{.*}[ ]*\n"        "")
    (tds2texi-regexp-replace "\\\\tdsVersion{.*}[ ]*\n\n"  "")
    
    ;; discard pre-title lines
    (tds2texi-regexp-replace "%&latex[ ]*\n"               "")
    (tds2texi-regexp-replace "\\\\NeedsTeXFormat.*\n"      "")
    
    ;; convert \documentclass to \input texinfo
    (tds2texi-regexp-replace "\\\\documentclass.*\n\n"  "\\\\input texinfo\n")

    ;; insert Texinfo header lines
    (save-excursion
      (goto-char (search-forward "texinfo\n" nil t))
      (insert "@setfilename " tds2texi-filename "\n")
      (insert "@settitle " title-string "\n\n")
      (insert "@set version " version-string "\n\n")
      (insert tds2texi-direntry "\n")
      )
 
    ;; discard \begin{document} and \maketitle
    (tds2texi-regexp-replace "\\\\begin{document}[ ]*\n\n" "")
    (tds2texi-regexp-replace "\\\\maketitle[ ]*\n\n"       "")

    ;; copy contents of `legalnotice' environments
    (save-excursion
      (let ((begin (search-forward "@titlepage\n\n" nil t))
	    (end (progn
		   (search-forward "@end titlepage" nil t)
		   (match-beginning 0))))
	(copy-region-as-kill begin end)
	))

    ;; insert stuff for title page before `legalnotice' environment
    ;; -- \begin{legalnotice} has been converted to @titlepage
    (save-excursion
      (goto-char (search-forward "@titlepage\n" nil t))
      (insert "@title " title-string "\n")
      (insert "@subtitle Version @value{version}\n")
      (insert "@author " author-string "\n\n")
      (insert "@page\n@vskip 0pt plus 1filll\n")
      )

    ;; insert stuff for @node Top and master menu after `legalnotice'
    ;; -- \end{legalnotice} has been converted to @end titlepage
    (save-excursion
      (goto-char (search-forward "@end titlepage\n\n" nil t))
      
      (insert "@ifnottex\n")
      (insert "@node Top\n@top " title-string "\n\n")
      
      ;; insert contents of `legalnotice' copied above
      (yank)	; should include a "\n\n" at the end

      (insert "@menu\n@end menu\n")
      (insert "@end ifnottex\n\n")
      )

    ;; discard \tableofcontents -- @contents belongs in the trailer
    (tds2texi-regexp-replace "\\\\tableofcontents[ ]*\n\n" "")
    ))

(defun tds2texi-do-trailer ()
  "Convert LaTeX trailer to Texinfo.  Used in \\[tds2texi-convert]."
  
  ;; The list of contributors is a LaTeX tabbing environment, which 
  ;; is difficult to convert -- we convert it to a normal paragraph
  ;; inside a @quotation environment, so it gets indented a little.

  ;; Conversion of the tabbing environment is already done elsewhere,
  ;; so we just have to remove some redundant tags.

  ;; discard alignment preamble consisting of lines starting with
  ;; \hspace{0.25\linewidth}.  We use \hspace in other places too.
  (tds2texi-regexp-replace "\\\\hspace.*\n" "")

  ;; convert "\>" and "\\" in tabbing environment to comma
  (tds2texi-regexp-replace " +\\\\>[ ]*" ", ")

  ;; In the earlier conversion of "\ ", "\\" followed by newline
  ;; was already converted to "\", so we just have to convert
  ;; the remaining instances of "\" followed by space or newline.
  (tds2texi-regexp-replace "\\s +\\\\\\s +" ", ")

  ;; convert \end{document} to @contents and @bye
  (tds2texi-string-replace "\\end{document}"
                           "@iftex\n@contents\n@end iftex\n@bye")
  )

(defun tds2texi-fixup-texinfo ()
  "Fix up a few last-minute items.  Used in \\[tds2texi-convert]."

  ;; Fix the reference to the node name with the logo.
  (tds2texi-string-replace "Non-font MF" "Non-font Metafont")
  )

;;; tds2texi.el ends here
