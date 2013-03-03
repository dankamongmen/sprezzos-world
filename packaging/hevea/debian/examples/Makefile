all: index.html
	(cd simple; make)
	(cd date; make)
	(cd bibtex; make)
	(cd pictures; make)
	(cd style_sheets; make)

index.html: index.tex
	hevea index.tex

clean:
	-rm index.html
	(cd simple; make clean)
	(cd date; make clean)
	(cd bibtex; make clean)
	(cd pictures; make clean)
	(cd style_sheets; make clean)
