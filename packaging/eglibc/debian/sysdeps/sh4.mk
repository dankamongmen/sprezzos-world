libc_add-ons = ports nptl $(add-ons)

# Renesas SH enabled -ffinte-math-only. Some software need -mieee.
extra_cflags = -mieee
