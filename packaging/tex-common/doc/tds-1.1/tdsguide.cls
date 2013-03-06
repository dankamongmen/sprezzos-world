% $Id: tdsguide.cls,v 1.19 2004/03/28 14:21:10 karl Exp $
% LaTeX document class tdsguide
% (history at end)
%
% This class provides markup & layout for the TDS report.
%
\NeedsTeXFormat{LaTeX2e}[1994/12/01]    % star-form of \CheckCommand, etc.


% The code below is explained in the implementation documentation of the
% rcs package.

\begingroup
    \def\RCSClass#1#2 $#3: #4 #5\endRCS $#6: #7 #8\endRCS{%
        \def\date{#4}\def\id{v#7}%
        \ProvidesClass{#1}[\date\space\id\space #2]%
        }

  \RCSClass{tdsguide}{markup and layout for TDS document}
  $Date: 2004/03/28 14:21:10 $: 9999/00/00 \endRCS
  $Revision: 1.19 $: 0.0 \endRCS
\endgroup


% default setup
\ifx \CatEscape\undefined
    \chardef\CatEscape=0
    \chardef\CatOpen=1
    \chardef\CatClose=2
    \chardef\CatIgnore=9
    \chardef\CatLetter=11
    \chardef\CatOther=12
    \chardef\CatActive=13

    \chardef\CatUsCode=\catcode`\_
\fi



%%% ======================================================================

%
%       OPTIONS
%

\catcode`\_=\CatLetter


% We can select to use Roman for METAFONT/METAPOST logos by the
% nomflogo option. That should be done if no current logo fonts (with
% `P' & `S') are available. Of course, it would be best to install
% current fonts.
%     The mflogo options is provided for upward compatibility.

\let\tds_mflogo\relax
\DeclareOption{nomflogo}{%
    \def\tds_mflogo{\let\textlogo\tds@transfer@logo} % gimme refinements! :(
    }
\DeclareOption{mflogo}{}


\newif\ifTdsDraft
        \TdsDraftfalse
\DeclareOption{draft}{%
    \TdsDrafttrue
    \PassOptionsToClass{\CurrentOption}{article}%
    }
\DeclareOption{final}{%
    \TdsDraftfalse
    \PassOptionsToClass{\CurrentOption}{article}%
    }


\catcode`\_=\CatUsCode


%%% ------------------------------------------------------------


%
%       CONFIGURATION, INHERITANCE, AND OTHER MODULES
%


% configure this class

\InputIfFileExists{tdsguide.cfg}{%
        \typeout{*************************************^^J%
                 *^^J%
                 * Local config file tdsguide.cfg used^^J%
                 *^^J%
                 *************************************%
                 }%
    }{}


% inherit article class

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}


% add more modules

\RequirePackage{url}                    % file names & email addresses
\RequirePackage{mflogo}                 % MF & MP logos (but see below)
%\RequirePackage{hyperref}               % navigation specials in .dvi
\RequirePackage{fancyhdr}               % rev. no & draft notice in headers

% Put hypertext links in the DVI file on the URL's:
% \def\UrlLeft#1\UrlRight{\special{html:<a href="#1">}#1\special{html:</a>}}
% Don't do this because xdvi doesn't know what do with ftp url's, which
% is the vast majority of links; also because we use the url package for
% mere directory names.


%%% ============================================================


%
%       NEW MARKUP
%

\catcode`\_=\CatLetter

% info about original SGML files

\def\sourcefile#1{}
\def\formatterfile#1{}

% revision number (should use rcs package)

\newtoks\tdsVersion



%
%       RENDER DOCUMENT-SPECIFIC MARKUP
%

% categories for terms

% typewriter type.
%  `_', `\_', and `\\' produce respective chars.

\def\tds_tt{%
    \begingroup
        \catcode`\_=\CatOther
        \chardef\_=`\_
        \chardef\\=`\\
        \tds_tt_set
    }
\def\tds_tt_set#1{%
        \ttfamily
        #1%
    \endgroup
    }

\def\systemitem#1{\tds_tt}
\def\replaceable#1{{\rmfamily $\langle$\textit{#1}$\rangle$}}
\let\command=\tds_tt
\def\application#1{\textrm{#1}}
\let\literal=\tds_tt
\let\email=\url

% \abbr may be called with lowercase & uppercase letters, actually all
% abbreviations are to be typeset with reduced-size uppercase letters.
% (That lowercase letters are used as arguments was a half-hearted
% attempt by Norm who did use small caps at this time.) `Reduced size'
% means one point smaller than the current size. If the reduced size
% is larger than 12pt, we issue a warning; if no font-generating
% driver is used, this situation will lead to character replacements
% most probably.
%
% This macro may be written to an auxilliary file, we must not use
% underscores in its name.

\DeclareRobustCommand\tds@reduced@size{%
    \dimen@\f@size\p@
    \advance \dimen@ -\p@               % dimen@ = font_size - 1
    \ifdim \dimen@ >12\p@
        \@font@warning{%
Using font size `\the\dimen@' for abbreviations. That might lead%
\MessageBreak
to character replacements if you don't use a driver that\MessageBreak
generates fonts.}%
    \fi
    \math@fontsfalse
    \fontsize{\the\dimen@}\z@
    \selectfont
    }
\def\abbr#1{{\tds@reduced@size \uppercase{#1}}}

% references

\let\xref\relax
\let\citetitle=\textit

% aliases for LaTeX markup

\let\emphasis=\emph

% environment `legalnotice', part of front matter

\let\legalnotice=\par
\let\endlegalnotice=\par

% environment `ttdisplay', used to explicate some fact. Basically,
% it's the alltt environment without parskip and with an additional
% level of indentation.

\def\ttdisplay{%
    \begingroup
        \list{}{%
            \parsep\z@skip              % no skip before env
            \parskip\z@skip
            \relax
            }%
        \item\relax
            \parshape\z@
            %% code below is copied from alltt.dtx.
            \leftskip\@totalleftmargin
            \@tempswafalse
            \def\par{%
                \if@tempswa
                    \leavevmode\null\@@par\penalty\interlinepenalty
                \else
                    \@tempswatrue
                    \ifhmode
                        \@@par
                        \penalty\interlinepenalty
                    \fi
                \fi
                }%
            \obeylines
            \verbatim@font
            \let\org@prime~%
            \@noligs
            \everymath \expandafter{\the\everymath \let~\org@prime}%
            \everydisplay \expandafter{\the\everydisplay \let~\org@prime}%
            \let\org@dospecials\dospecials
            \g@remfrom@specials{\\}%
            \g@remfrom@specials{\{}%
            \g@remfrom@specials{\}}%
            \let\do\@makeother \dospecials
            \let\dospecials\org@dospecials
            \frenchspacing\@vobeyspaces
            \everypar \expandafter{\the\everypar \unpenalty}%
            %}
    }
\def\endttdisplay{%
        \endlist
    \endgroup
    \vskip -\parskip                    % undo skip after env
    }

% Code below is from alltt.dtx. I don't understand why it's not part
% of the LaTeX kernel.
\def\g@remfrom@specials#1{%
    \def\@new@specials{}%
    \def\@remove##1{%
        \ifx ##1#1%
        \else
            \g@addto@macro\@new@specials{\do ##1}%
        \fi
        }%
    \let\do\@remove \dospecials
    \let\dospecials\@new@specials
    }




%%% ------------------------------------------------------------


%
%       LOGOS
%


% Some of them are rather document-specific.

\def\texmf{\path|texmf|}
\def\CTAN:{\replaceable{\abbr{CTAN}:}}


% Actually, I should use my logos package for the rest... [-js]

% The AmS definition is from AmS-LaTeX, that's more stable in this
% document's context than the one from AmS-TeX.
\def\AmS{%
    \begingroup
        \protect\usefont{OMS}{cmsy}{m}{n}%
        A\kern-.1667em \lower.5ex\hbox{M}\kern-.125em S%
    \endgroup
    }
\def\AMSTeX{\AmS-\TeX}

\def\iniTeX{\texttt{INITEX}\@}
\def\iniMF{\texttt{INIMF}\@}
\def\iniMP{\texttt{INIMP}\@}

\def\PS{\textsc{PostScript}}


% If we don't use logo fonts, both the Metafont and the MetaPost logo
% is typeset in the current font, as shown in this comment. The macros
% `\MF' & `\MP' expand to two \textfont macros, with uppercase
% syllables as arguments. We check for the syllable `FONT', this has
% to be typeset in lowercase. All other syllables are capitalized.

\def\tds_logo_fontarg{FONT}
\DeclareRobustCommand\tds@transfer@logo[1]{%
    \def\tds_arg{#1}%
    \ifx \tds_arg\tds_logo_fontarg
        font%
    \else
        \tds_capitalize#1\tds_argend
    \fi
    }
\def\tds_capitalize#1#2\tds_argend{%
    \uppercase{#1}\lowercase{#2}%
    }

\tds_mflogo


% Use a small caps fake for BibTeX's `ib'. This way we can typeset it
% in bold face or sans-serif, too. The code is copied from the LaTeX
% logo definition, from ltlogos.dtx.

\DeclareRobustCommand\tds@smsize{%
    $\m@th$%                            % force math size calculation
    \csname S@\f@size\endcsname
    \fontsize\sf@size\z@
    \math@fontsfalse \selectfont
    }

\def\BibTeX{B\kern-.05em{\tds@smsize IB}\kern-.08em\TeX}


%%% ============================================================

%
%       TDS SUMMARIES
%

% Typeset a figure that shows the TeX directory layout. The layout is
% input with optical markup:
%
%     \begin{tdsSummary}
%     dir/                    explanation of dir
%      . subdir/              explanation of subdir
%      . . subsubdir/         next subdir level
%      . dir/dir/             more than one dir in one line
%      . <category>/          explanation of category
%     \end{tdsSummary}
%
% Directories and category names don't have dots.

% We transform that input into a table. The first table column (the
% directory spec) is terminated by a slash that's followed by white
% space. Directory names are typeset in monocase. <categories> are
% tagged with \replaceable. Subdirectory levels are indented by one
% quad. A quad is placed between columns. The explanation text
% is typeset in one line.

% As this is a special implementation for a special document, these
% design decisions are hardwired, no protected interface is available.
% Design changes will be realized by changes to the macro code.


% The environment is realized as a trivlist to behave correctly in list
% environments. I don't use a tabular environment, since I don't know
% exactly how the first column is tokenized. Instead a halign is used.
% IMO it does not matter, as tdsSummary does not use table markup
% anyhow. If somebody wants to use longer explanations and if they must
% be broken then, I have to use one of the tabulars of the tools bundle.

% The first column (the directory names) is typeset in typewriter type. The
% font chosen hereby must have the underscore at its ASCII position. If
% that is not the case, one has to generalize \tds_dir_tags below.
% Namely, this macro sets up the special lexical convention for this
% column. As TeX will look at the first token of the column to check for
% the table end, \cr, or \omit before \tds_dir_tags is evaluated, the
% redefinition of the lexical analysis will not have happened for that
% token. So we have to check this token specially if it's one of the
% special optical markup chars.

% After we started the trivlist, we need to supply the item (as this
% macro really adds the vertical space). But it does not actually
% start an item, it sets up the everypar register to do so. We fake
% the item start as we don't want to go in horizontal mode, \halign
% needs to be evaluated in vertical mode.
%
% \par is not evaluated by \endtrivlist and \end, so we don't need
% another group around its redefinition.

\def\tdsSummary{%
    \trivlist \item\relax
    \vskip\parskip
    \global\@inlabelfalse \global\@newlistfalse \global\everypar{}%
    \tabskip 0pt
    \let\par\crcr  \obeylines       % can't use \obeycr, redefined by LaTeX
    % redefine \obeylines to do nothing; url.sty v3.0 calls it, and the
    % ^^M character therein causes a `Missing \endgroup inserted' error
    % at a \path|...| within a tdsSummary, if it's already defined as \par.
    \let\obeylines=\relax
    \halign\bgroup
        \ttfamily \tds_dir_tags \tds_first_token ##\unskip\hfil &%
            \quad ##\unskip\hfil \cr
    }
\def\endtdsSummary{%
        \crcr
        \egroup
    \endtrivlist
    }


% Our special lexical conventions are: `.' denotes the next directory
% level, `<...>' denotes a directory category, and `/' is a column
% separator if it is followed by space or tab, a typeset slash
% otherwise. Underscores don't have any special meaning.

\begingroup
    \catcode`\.=\CatActive
    \catcode`\<=\CatActive
    \catcode`\/=\CatActive
    \catcode`\:=\CatActive
    \gdef\tds_dir_tags{%
        \catcode`\.=\CatActive  \let.=\tds_next_dir_level
        \catcode`\<=\CatActive  \let<=\tds_dir_category
        \catcode`\/=\CatActive  \let/=\tds_slash
        \catcode`\:=\CatActive  \let:=\tds_colon
        \catcode`\_=\CatOther
        \tt
        }
\endgroup

% There are spaces between the dots, ignore them.

\def\tds_next_dir_level{\quad \ignorespaces}
\def\tds_dir_category#1>{\replaceable{#1}}

\chardef\tds_dir_sep=`\/                % typeset a directory separator

% The slash has to check the stuff behind. If it's a space or a tab,
% \next is bound to the action `blank-space' (the binding of all
% '(space . #\Space) tokens). A tab is usually tokenized to the same
% token, but we care for redefinitions below.
% 
\def\tds_slash{%
    \futurelet\next \tds_check_dir_end
    }

% For colons in the examples.
% 
\def\tds_colon{:&}

% We need two control sequencess that are bound to the actions currently
% bound to '(space . #\Space) and (tex:parse "\t"). A direct \let
% evaluation is difficult as the lexical analysis collapses subsequent
% spaces to one token. But we need one space token behind the equal
% sign, to be able to \let our cseq to the next space token. The first
% token is stored in the replacement list of a macro, the second token
% comes as its argument.

\def\tds_let#1#2{\let#1= #2}
\tds_let\tds_space{ }
\tds_let\tds_tab{^^I}

\def\TdsColSep{/&}                      % use trailing slashes for dir names

\def\tds_check_dir_end{%
    \ifx \next\tds_space
        \let\next\TdsColSep
      \else
        \ifx \next\tds_tab
            \let\next\TdsColSep
          \else
            \let\next\tds_dir_sep
        \fi
    \fi
    \next
    }


% We're almost finished with the tdsSummary setup. The stuff above is
% not used for the first column token, as it's already tokenized when
% the catcode changes take effect. So let's check and remap this token
% by hand. Actually, I'm sloppy here; I assume that `/' won't be a
% used as a directory name and that no directory name starts with an
% underscore. But then, I know that this won't happen in TDS... :-)

\def\tds_first_token{%
    \futurelet\next \tds_check_special
    }
\def\tds_check_special{%
    \ifx \next.%
        \def\next{\afterassignment\tds_next_dir_level \let\next}%
      \else
        \ifx \next<%
            \def\next{\afterassignment\tds_dir_category \let\next}%
          \else
            \let\next\relax
        \fi
    \fi
    \next
    }



%%% ============================================================

%
%       LAYOUT
%


% text area size

% We use the same height as article's default, but increase the width.

\textwidth=35pc
\oddsidemargin = \paperwidth
  \advance\oddsidemargin by -\textwidth
  \divide\oddsidemargin by 2
\evensidemargin = \oddsidemargin
\hoffset = -1in


% spacing

\parindent=0pt                          % no paragraph indentation
\emergencystretch=2em                   % fewer overfull hboxes, only a memo

\parskip=1ex plus 1ex minus .5ex        % between paragraphs; if you change it,
                                        % change the spacing in section headers
                                        % below, too!

% Our spacing for lists is very simplistic, but uniform: We use (almost)
% parskip everywhere, and don't distinguish vertical spacing for
% different list levels. Actually, the latter is the thing we might want
% to introduce later again.

\labelsep=1.1em                         % increase distance between item & text
\topsep=0pt                             % no extra skip above list in paragraph
\partopsep=0pt                          % no extra skip above list starting par.
\itemsep=0pt                            % no extra space between items
\parsep=.9\parskip                      % between paragraphs in items

\def\@listI{%
    \leftmargin\leftmargini
    }
\let\@listi\@listI
\@listi

\def\@listii{%
    \leftmargin\leftmarginii
    \labelwidth\leftmarginii  \advance \labelwidth-\labelsep
    }

\def\@listiii{%
    \leftmargin\leftmarginiii
    \labelwidth\leftmarginiii  \advance \labelwidth-\labelsep
    }


% Like itemize, but squeeze the items.

\newenvironment{itemize-squeeze}
    {%
        \parsep\z@skip
        \itemize
    }{%
        \enditemize
    }

% Ditto, for description.

\newenvironment{description-squeeze}
    {%
        \parsep\z@skip
        \description
    }{%
        \enddescription
    }


% Redefine section headers, different spacing and other fonts.

\newskip\TdsPreSubSectionSkip
        \TdsPreSubSectionSkip = 2.5ex plus 0.5ex minus 0.2ex

\def\section{%
    \@startsection{section}{1}%         % name and level
        {\z@}%                          % indentation
        {-3ex plus-.75ex minus-.2ex}%   % skip above and don't indent next par
        {1.5ex plus.2ex}%               % skip below
        {\reset@font \Large \sffamily}% % type
    }
\def\subsection{%
    \@startsection{subsection}{2}%      % name and level
        {\z@}%                          % indentation
        {-\TdsPreSubSectionSkip}%       % skip above and don't indent next par
        \parskip                        % skip below
        {\reset@font \large \sffamily}% % type
    }
\def\subsubsection{%
    \@startsection{subsubsection}{3}%   % name and level
        {\z@}%                          % indentation
        {-\TdsPreSubSectionSkip}%       % skip above and don't indent next par
        \parskip                        % skip below
        {\reset@font \normalsize \sffamily}% % type
    }
\def\paragraph{%
    \@startsection{paragraph}{4}%       % name and level
        {\z@}%                          % indentation
        \TdsPreSubSectionSkip           % skip above and indent next par
        {-1.5em}%                       % skip after, run in text
        {\reset@font \normalsize \scshape}% % type
    }


% Table of contents

\setcounter{tocdepth}{2}                % only sections on first two levels

% Less space between TOC entries, discard the parskip. For that, we
% have to redefine \@starttoc, since no hook is available.
\let\tds_orig_starttoc=\@starttoc
\def\@starttoc#1{%
    \begingroup
        \parskip\z@skip
        \tds_orig_starttoc{#1}%
    \endgroup
    }

% Section TOC entries in sans serif, and less space above. The code
% below is copied from the article view on classes.dtx, I have changed
% space and font. But let's first assure that we shouldn't use a newer
% version of this code.
\CheckCommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \addvspace{1.0em \@plus\p@}%
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}
\def\l@section#1#2{%
    \ifnum \c@tocdepth >\z@
        \addpenalty\@secpenalty
        \addvspace{.75ex \@plus\p@ \@minus\p@}% % was:  1em plus 1pt
        \setlength\@tempdima{1.5em}%
        \begingroup
            \parindent \z@
            \rightskip \@pnumwidth
            \parfillskip -\@pnumwidth
            \leavevmode \sffamily       % was: \bfseries
            \advance\leftskip\@tempdima
            \hskip -\leftskip
            #1\nobreak \hfil \nobreak \hbox to\@pnumwidth{\hss #2}%
            \par
        \endgroup
    \fi}


% Not so spacey a title, please.

\def\@maketitle{%
  \null
  \kern-90pt \relax \kern0pt
  \begin{center}%
    {\LARGE \@title \par}%
    %\vskip 1.5em%
    {\large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \vskip 1em%
    {\large  version \the\tdsVersion \quad \@date}%
  \end{center}%
  \par
  \vskip 1.5em}


% Make labels in the description environment come out in the
% requested type rather than bold.

\def\descriptionlabel#1{\hspace\labelsep #1}


% Headline & footline

\pagestyle{fancy}

\lhead{}
\chead{%
    \ifTdsDraft draft \fi               % <-- space!
    TDS version \the\tdsVersion
    }
\rhead{}
\renewcommand{\headrulewidth}{0.4pt}

\lfoot{}
\cfoot{\thepage}
\rfoot{}
\renewcommand{\footrulewidth}{0pt}



%%% ============================================================

\catcode`\_=\CatUsCode

\endinput



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Log: tdsguide.cls,v $
% Revision 1.19  2004/03/28 14:21:10  karl
% (\tdsSummary): \let\obeylines=\relax after we set
% it, so url.sty v3.0's usage of \obeylines does not
% cause errors.
%
% Revision 1.18  1998/01/26 21:26:10  karl
% (\TeXinfo): Remove unused and incorrect name.
% (\parsep): Decrease slightly.
% (\l@section): Decrease section spacing for toc slightly.
% (\@maketitle): Remove word `draft'.
%
% Revision 1.17  1997/02/07 20:47:16  karl
% Fix : in tdsSummary, more doc.
%
% Revision 1.16  1996/11/16 14:23:58  karl
% fancyhdr requires different commands.
%
% Revision 1.13  1995/11/21  18:57:35  schrod
%     Add description-squeeze environment.
%
% Revision 1.12  1995/11/14  18:33:18  schrod
%     Drop draft hint in legalnotice environment, make title more
% squeezed. Changes by Karl, to make title & toc fit on one page.
%
% Revision 1.11  1995/11/14  12:36:54  schrod
%     Use url.sty instead of path.sty. (On request of Karl, better input
% syntax and different line breaks [0.103].)
%     Rename environment explicate to ttdisplay. (Proposal of Karl.)
%
% Revision 1.10  1995/11/11  16:29:01  schrod
%     Identify version of class in log file.
%     Change layout of table of contents. Implementation of a discussion
% between Karl Berry and myself.
%
% Revision 1.9  1995/11/11  15:43:34  schrod
%     Replace alltt environment by explicate environment. Change
% requested by Karl at 09 Nov 95.
%
% Revision 1.8  1995/11/02  10:52:29  schrod
%     Add option nomflogo, mflogo is default now.
%     Don't need provision for slanted typewriter type any more,
% replacable texts are set in italics now.
%     Add logos \texmf and \CTAN:. [kb]
%     Set margins in addition to \textwidth. It's a bit wider, too. [kb]
%     Wider distance between item label and item text. [kb]
%     New environment itemize-squeeze, without space between items. [kb]
%
% Revision 1.7  1995/05/18  09:06:39  schrod
%     Don't parse arguments unnecessarily early, one might change
% catcodes in them.
%     Use \tds_tt to switch to typewriter, also typesets backslashs and
% underscores [kb].
%     Increase width to 33pc [kb].
%
% Revision 1.6  1995/05/12  23:19:07  schrod
%     \f@size may be a fraction, so use dimension register to compute
% the reduced size for abbreviations [dc].
%     In summary tables, terminate the directory names with slashes [kb, nw].
%
% Revision 1.5  1995/05/09  01:46:24  schrod
%     Typeset abbreviation one point smaller than surrounding font [uv,
% kb, bb], use always uppercase letters for them. Rename the tag from
% \acronym to \abbr [kb, nw].
%
% Revision 1.4  1995/05/09  00:39:57  schrod
%     Repair vertical spacing around tdsSummary environment.
%
% Revision 1.3  1995/05/08  17:14:52  schrod
%     For TDS 0.72 Norm changed the document to use \abbr (argument with
% lowercase letters) instead of \acronym. It's not looking better, of
% course; small caps is not really a good font for abbreviations. This
% revision just tracks his change, I'll look later if I can improve the
% rendering.
%
% Revision 1.2  1995/05/07  18:22:53  schrod
%     In environment tdsSummary: Next directory level indented by one
% quad, without dots for indentation [kb, !uv, js].
%     Renamed Draft flag to TdsDraft, to keep namespace clean.
%     Headline now features a centered short title [kb, uv, nw]. It's
% still ruled.
%     \replaceable used spurious \selectfont [uv]. \dir & \ext are not
% used [uv].
%     Subsubsections are also numbered and not indented [uv]. But they
% are still not added to the table of contents. Actually, we have only
% _one_ subsubsection...
%     Set erroneously counter `secnumdepth' when I wanted fewer section
% headings in the table of contents. That's controlled by counter
% `tocdepth' [uv].
%     Inherit tdsguide from article class, not from ltxguide. The tags
% of the latter class are not appropriate and we define our layout
% ourselves.
%

%
% pre-CVS Log:
%
% 19 Apr 95 js  Made a LaTeX2e class.
%               Use mflogo option to select MF/MP logo typesetting,
%               support configuration file.
% 1994-1995 nw  Initial revision.



%%%------------------------------------------------------------

% mode: LaTeX
% Local Variables:
% TeX-brace-indent-level: 4
% indent-tabs-mode: t
% TeX-auto-untabify: nil
% TeX-auto-regexp-list: LaTeX-auto-regexp-list
% page-delimiter: "^%%%"
% End:
