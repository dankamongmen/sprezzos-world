
# these files aren't installed
\+usr/share/doc/slrn/COPY\(ING\|RIGHT\)+ d

# GroupLens isn't enabled
\+usr/share/doc/slrn/README.GroupLens+ d

\+/doc/slrn/slrnpull/+ s./slrnpull/.pull/.

\+usr/share/doc/slrn/changes.txt+ s/changes.txt/changelog/

\+usr/share/doc/slrn/help.txt+ cetc/news/slrn-help.txt

\+usr/share/doc/slrnpull/\(score\|slrnpull.sh\|slrn.rc\)+ s./\([^/]*\)$./examples/\1.

\+usr/share/doc/slrnpull/slrnpull.conf+ s+.*/+etc/news/+

\+usr/share/doc/slrn/\(score.sl\|slrn.rc\)+ s./\([^/]*\)$./examples/\1.
