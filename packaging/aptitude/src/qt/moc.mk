%.moc : %.h
	$(MOC) -i -o "$@" "$<"

%.mocc : %.cc
	$(MOC) -i -o "$@" "$<"

clean-moc-extra:
	rm -vf *.moc
	rm -vf *.mocc

clean-am: clean-moc-extra

