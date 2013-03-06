# $Id: tds.sed,v 1.2 2004/06/04 17:11:34 karl Exp $
# Written by Ulrik Vieth and Karl Berry.
# Public domain.
# 
# Things that are too hard to do in Elisp.  Run after tds2texi-convert.

# Indentation blocks in tdsSummary environments.
s/ \./  /g

# References that are too hard to convert automatically.
s/Table, ref{tab:summary}/@pxref{Summary},/

/^Copyright.*Group/a\
This is version @value{version}.
