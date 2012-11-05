
NULL =
INSTALL_STUFF =			\
	vim-policy.html/	\
	vim-policy.txt		\
	$(NULL)

all: html text

# to be used on alioth for updating policy linked by pkg-vim project page
install: all
	cp -r $(INSTALL_STUFF) ~/pkg-vim/htdocs/

html: vim-policy.html/index.html
text: vim-policy.txt

vim-policy.html/index.html: vim-policy.xml *.xml
	docbook2html $< -o $(dir $@)
vim-policy.txt: vim-policy.xml *.xml
	docbook2txt $<

clean:
	$(RM) -rf $(INSTALL_STUFF)

upload_web: all
	scp -rC $(INSTALL_STUFF) alioth.debian.org:/srv/home/groups/pkg-vim/htdocs/

.PHONY: html text upload_web

