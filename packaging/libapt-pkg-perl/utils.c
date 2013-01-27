#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"

#include <stdarg.h>
#include "utils.h"

/*
 * Parse an array reference in a similar manner to python's ParseTuple
 *
 *   p_avref  a pointer to an SV containing (hopefully) an AV reference
 *   fmt      a format string defining how to parse the elements of the
 *	      AV:
 *
 *		s	String (char const *).  If followed by `#', the
 *		s#	length is placed into the argument following
 *			(STRLEN *).
 *
 *		z	A variant of `s' in which permits the value to
 *		z#	be undefined (converted to NULL).
 *
 *		h	Short integer (short int *).
 *		i	Integer (int *).
 *		l	Long integer (long *).
 *		c	The first character of a string (char *).
 *		f	Floating point number (float *).
 *		d	Double precision floating point number (double *).
 *		@	Array reference (AV **).
 *		%	Hash reference (HV **).
 *		&	Code reference (CV **).
 *		|	Optional elements follow.
 *
 * The return value is a string giving the first error encountered (if
 * any).
 */

char const *parse_avref(pTHX_ SV **p_avref, char const *fmt, ...)
{
    va_list ap;
    char const *err = 0;
    AV *av;
    I32 len = 0;

    va_start(ap, fmt);
    if (p_avref && SvROK(*p_avref) && SvTYPE(SvRV(*p_avref)) == SVt_PVAV)
    {
	av = (AV *) SvRV(*p_avref);
	len = av_len(av) + 1;
    }
    else
	err = "array reference required";

    I32 i = 0;
    int opt = 0;

    while (*fmt)
    {
	SV *v = 0;
	char type = *fmt++;

	if (!err)
	{
	    if (i < len)
	    {
		SV **tmp = av_fetch(av, i++, 0);
		if (tmp)
		    v = *tmp;
	    }
	    else if (!opt)
	    {
		err = "too few elements";
	    }
	}

	switch (type)
	{
	case 's':
	case 'z':
	    {
		char const **s = va_arg(ap, char const **);
		STRLEN *l = &PL_na;
		if (*fmt == '#')
		{
		    l = va_arg(ap, STRLEN *);
		    fmt++;
		}

		*s = 0;
		*l = 0;
		if (v)
		{
		    if (SvOK(v))
		    {
			if (SvPOK(v) || SvNIOK(v))
			    *s = SvPV(v, *l);
			else
			    err = "invalid string";
		    }
		    else if (type != 'z')
			err = "undefined element";
		}
	    }
	    break;

	case 'h':
	    {
		short *s = va_arg(ap, short *);
		*s = 0;

		if (v)
		{
		    if (SvNIOK(v))
			*s = (short) SvIV(v);
		    else
			err = "short int expected";
		}
	    }
	    break;

	case 'i':
	    {
		int *i = va_arg(ap, int *);
		*i = 0;

		if (v)
		{
		    if (SvNIOK(v))
			*i = (int) SvIV(v);
		    else
			err = "integer expected";
		}
	    }
	    break;

	case 'l':
	    {
		long *l = va_arg(ap, long *);
		*l = 0;

		if (v)
		{
		    if (SvNIOK(v))
			*l = (long) SvIV(v);
		    else
			err = "long integer expected";
		}
	    }
	    break;

	case 'c':
	    {
		char *c = va_arg(ap, char *);
		*c = 0;

		if (v)
		{
		    char const *s;
		    STRLEN l;

		    if ((SvPOK(v) || SvNIOK(v)) && (s = SvPV(v, l)) && l)
			*c = *s;
		}
	    }
	    break;

	case 'f':
	    {
		float *f = va_arg(ap, float *);
		*f = 0;

		if (v)
		{
		    if (SvNIOK(v))
			*f = (float) SvNV(v);
		    else
			err = "float expected";
		}
	    }
	    break;

	case 'd':
	    {
		double *d = va_arg(ap, double *);
		*d = 0;

		if (v)
		{
		    if (SvNIOK(v))
			*d = (double) SvNV(v);
		    else
			err = "double expected";
		}
	    }
	    break;

	case '@':
	    {
		AV **a = va_arg(ap, AV **);
		*a = 0;

		if (v)
		{
		    if (SvROK(v) && SvTYPE(SvRV(v)) == SVt_PVAV)
			*a = (AV *) SvRV(v);
		    else
			err = "array ref expected";
		}
	    }
	    break;

	case '%':
	    {
		HV **h = va_arg(ap, HV **);
		*h = 0;

		if (v)
		{
		    if (SvROK(v) && SvTYPE(SvRV(v)) == SVt_PVHV)
			*h = (HV *) SvRV(v);
		    else
			err = "hash ref expected";
		}
	    }
	    break;

	case '&':
	    {
		CV **c = va_arg(ap, CV **);
		*c = 0;

		if (v)
		{
		    if (SvROK(v) && SvTYPE(SvRV(v)) == SVt_PVCV)
			*c = (CV *) SvRV(v);
		    else
			err = "code ref expected";
		}
	    }
	    break;

	default:
	    croak("parse_avref: invalid format character `%c'", type);
	}

	/* "|" = args following are optional */
	if (*fmt == '|')
	{
	    fmt++;
	    opt++;
	}
    }

    if (!err && i < len)
	err = "extra elements";

    va_end(ap);
    return err;
}
