.PHONY: tidy

SUBDIRS += additions manpages patches po

tidy:
	rm -f *~

mrproper: tidy
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir tidy; done

