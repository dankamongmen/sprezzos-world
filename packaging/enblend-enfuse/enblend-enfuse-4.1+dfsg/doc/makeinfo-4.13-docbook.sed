# name:     makeinfo-4.13-docbook.sed
# synopsis: Fix most of the cr@p makinfo-4.13 produces with "--docbook".
#           As we are at it, replace the DOCTYPE with the one we need.
# sed:      GNU sed 4.1.5


# We must splice in the DTD for MathML anyhow, so we recreate the
# whole DOCTYPE.

/<!DOCTYPE/, /]>/c\
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook MathML Module V1.1b1//EN"\
          "http://www.oasis-open.org/docbook/xml/mathml/1.1b1/dbmathml.dtd" [\
  <!ENTITY tex "TeX">\
  <!ENTITY latex "LaTeX">\
]>\


# Makeinfo puts anchors into para-elements right in front of table- or
# figure elements where they rather belong.  This is semantically
# wrong and prevents some formatters from referencing the table or
# figure.

s#<para><anchor id="Table:[^"]*"></anchor><table \([^>]*\)></para>#<table \1>#
s#<para><anchor id="Figure:[^"]*"></anchor><figure \([^>]*\)></para>#<figure \1>#
s#<anchor id="\([^"]*\)"></anchor><\(table\|figure\)>#<\2 id="\1">#
s#<figure \([^>]*\)></para>#</para><figure \1>#


# Makeinfo is double-minded (schizophrenic?) with "Table:foo" or
# "Figure:bar" IDs, some are converted to "Table-foo" or "Figure-bar",
# but some remain unchanged.  So we convert all of them to the dashed
# form.

s#id="\(Table\|Figure\):\([^"]*\)"#id="\1-\2"#g


# Related to the problem with anchors-in-para-elements instead to the
# correct floating elements we must attach IDs to
# variablelist-elements.

\#<para><anchor id="[^"]*"></anchor></para>#N;s#<para><anchor id="\([^"]*\)"></anchor></para>[ \t\n]*<variablelist#<variablelist id="\1"#


# Our graphics are never inline.  Mazel tov!

s#<\(/*\)inlinemediaobject>#<\1mediaobject>#g


# WTF?  PDF is just not supported by the DTD.

s#<imageobject><imagedata fileref="[^"]*" format="PDF"></imagedata></imageobject>##


# When slurping in our *.txt files, Makeinfo picks up a ^L that breaks
# well-formedness.

s#<literallayout>\f$#<literallayout>#


# The lists of figures and tables are required to have some contents.

s#<title>List of \(Figures\|Tables\)</title>#<title>List of \1</title><para></para>#


# Remove the stupid Emacs syntactic comment at the end of the file.

\#</book><!--#, $c\
</book>
