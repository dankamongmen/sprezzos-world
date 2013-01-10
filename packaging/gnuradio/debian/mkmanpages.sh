#!/bin/sh

for i in \
create-gnuradio-out-of-tree-project \
file_rx_hrpt \
gnuradio-companion \
gnuradio-config-info \
gr_filter_design \
gr_plot_char \
gr_plot_const \
gr_plot_fft \
gr_plot_fft_c \
gr_plot_fft_f \
gr_plot_float \
gr_plot_int \
gr_plot_iq \
gr_plot_psd \
gr_plot_psd_c \
gr_plot_psd_f \
gr_plot_qt \
gr_plot_short \
hrpt_decode \
hrpt_demod \
uhd_fft \
uhd_rx_cfile \
uhd_rx_nogui \
uhd_siggen \
uhd_siggen_gui \
usrp_flex \
usrp_flex_all \
usrp_flex_band \
usrp_rx_hrpt \
usrp_rx_hrpt_nogui \
; do help2man --no-discard-stderr --version-string=3.5 /usr/bin/$i.py > $i.1 ; done

exit 0

W: gnuradio: binary-without-manpage usr/bin/create-gnuradio-out-of-tree-project
N: 
N:    Each binary in /usr/bin, /usr/sbin, /bin, /sbin or /usr/games should
N:    have a manual page
N:    
N:    Note that though the man program has the capability to check for several
N:    program names in the NAMES section, each of these programs should have
N:    its own manual page (a symbolic link to the appropriate manual page is
N:    sufficient) because other manual page viewers such as xman or tkman
N:    don't support this.
N:    
N:    If the name of the man page differs from the binary by case, man may be
N:    able to find it anyway; however, it is still best practice to make the
N:    case of the man page match the case of the binary.
N:    
N:    If the man pages are provided by another package on which this package
N:    depends, lintian may not be able to determine that man pages are
N:    available. In this case, after confirming that all binaries do have man
N:    pages after this package and its dependencies are installed, please add
N:    a lintian override.
N:    
N:    Refer to Debian Policy Manual section 12.1 (Manual pages) for details.
N:    
N:    Severity: normal, Certainty: possible
N:    
N:    Check: manpages, Type: binary
N: 
W: gnuradio: binary-without-manpage usr/bin/file_rx_hrpt
W: gnuradio: binary-without-manpage usr/bin/gnuradio-companion
W: gnuradio: binary-without-manpage usr/bin/gnuradio-config-info
W: gnuradio: binary-without-manpage usr/bin/gr_filter_design
W: gnuradio: binary-without-manpage usr/bin/gr_plot_char
W: gnuradio: binary-without-manpage usr/bin/gr_plot_const
W: gnuradio: binary-without-manpage usr/bin/gr_plot_fft
W: gnuradio: binary-without-manpage usr/bin/gr_plot_fft_c
W: gnuradio: binary-without-manpage usr/bin/gr_plot_fft_f
W: gnuradio: binary-without-manpage usr/bin/gr_plot_float
W: gnuradio: binary-without-manpage usr/bin/gr_plot_int
W: gnuradio: binary-without-manpage usr/bin/gr_plot_iq
W: gnuradio: binary-without-manpage usr/bin/gr_plot_psd
W: gnuradio: binary-without-manpage usr/bin/gr_plot_psd_c
W: gnuradio: binary-without-manpage usr/bin/gr_plot_psd_f
W: gnuradio: binary-without-manpage usr/bin/gr_plot_qt
W: gnuradio: binary-without-manpage usr/bin/gr_plot_short
W: gnuradio: binary-without-manpage usr/bin/hrpt_decode
W: gnuradio: binary-without-manpage usr/bin/hrpt_demod
W: gnuradio: binary-without-manpage usr/bin/uhd_fft
W: gnuradio: binary-without-manpage usr/bin/uhd_rx_cfile
W: gnuradio: binary-without-manpage usr/bin/uhd_rx_nogui
W: gnuradio: binary-without-manpage usr/bin/uhd_siggen
W: gnuradio: binary-without-manpage usr/bin/uhd_siggen_gui
W: gnuradio: binary-without-manpage usr/bin/usrp_flex
W: gnuradio: binary-without-manpage usr/bin/usrp_flex_all
W: gnuradio: binary-without-manpage usr/bin/usrp_flex_band
W: gnuradio: binary-without-manpage usr/bin/usrp_rx_hrpt
W: gnuradio: binary-without-manpage usr/bin/usrp_rx_hrpt_nogui
