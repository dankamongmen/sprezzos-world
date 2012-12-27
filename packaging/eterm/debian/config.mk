config_opts := \
	--enable-xim \
	--with-backspace=del \
	--with-delete=execute \
	--enable-share=yes \
	--enable-static=no \
	--enable-mmx=no \
	--enable-multi-charset=unicode \
	--enable-escreen \
	--with-pty-group=tty \
	LDFLAGS="$(LDFLAGS) -Wl,--as-needed"
