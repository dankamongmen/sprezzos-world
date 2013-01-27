build: ;

test:
	tests/dh_xul-ext/test
	tests/test-moz-version

install:
	python setup.py install --root="$(DESTDIR)" --no-compile --install-layout=deb

clean:
	rm -rf build *.pyc

.PHONY: build clean install test
