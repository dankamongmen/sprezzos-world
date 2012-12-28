/*
 * cdebconf frontend, corba servant, text frontend implementation
 */

#include "common.h"
#include "template.h"
#include "question.h"
#include "frontend.h"
#include "database.h"
#include "strutl.h"

#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <ORBitservices/CosNaming.h>
#include <libgnorba/gnorba.h>
#include <gnome.h>

#include "dcf.h"

/*** App-specific servant structures ***/

typedef struct {
  POA_Debconf_Frontend servant;
  PortableServer_POA poa;

} impl_POA_Debconf_Frontend;

/*** Implementation stub prototypes ***/

static void impl_Debconf_Frontend__destroy(impl_POA_Debconf_Frontend *
					   servant, CORBA_Environment * ev);

static CORBA_boolean
impl_Debconf_Frontend_Boolean(impl_POA_Debconf_Frontend * servant,
			      CORBA_char * prompt, CORBA_Environment * ev);

static CORBA_unsigned_long_long
impl_Debconf_Frontend_Multiselect(impl_POA_Debconf_Frontend * servant,
				  CORBA_unsigned_short count,
				  Debconf_Frontend_selects * choices,
				  CORBA_Environment * ev);

static void
impl_Debconf_Frontend_Note(impl_POA_Debconf_Frontend * servant,
			   CORBA_char * note, CORBA_Environment * ev);

static CORBA_char *impl_Debconf_Frontend_Password(impl_POA_Debconf_Frontend *
						  servant,
						  CORBA_char * prompt,
						  CORBA_Environment * ev);

static CORBA_unsigned_short
impl_Debconf_Frontend_Select(impl_POA_Debconf_Frontend * servant,
			     CORBA_unsigned_short count,
			     Debconf_Frontend_selects * choices,
			     CORBA_Environment * ev);

static CORBA_char *impl_Debconf_Frontend_String(impl_POA_Debconf_Frontend *
						servant, CORBA_char * prompt,
						CORBA_Environment * ev);

static CORBA_char *impl_Debconf_Frontend_Text(impl_POA_Debconf_Frontend *
					      servant, CORBA_char * prompt,
					      CORBA_Environment * ev);

/*** epv structures ***/

static PortableServer_ServantBase__epv impl_Debconf_Frontend_base_epv = {
  NULL,			/* _private data */
  NULL,			/* finalize routine */
  NULL,			/* default_POA routine */
};
static POA_Debconf_Frontend__epv impl_Debconf_Frontend_epv = {
  NULL,			/* _private */

  (gpointer) & impl_Debconf_Frontend_Boolean,

  (gpointer) & impl_Debconf_Frontend_Multiselect,
  
  (gpointer) & impl_Debconf_Frontend_Note,
  
  (gpointer) & impl_Debconf_Frontend_Password,
  
  (gpointer) & impl_Debconf_Frontend_Select,
  
  (gpointer) & impl_Debconf_Frontend_String,
  
  (gpointer) & impl_Debconf_Frontend_Text,

};

/*** vepv structures ***/

static POA_Debconf_Frontend__vepv impl_Debconf_Frontend_vepv = {
  &impl_Debconf_Frontend_base_epv,
  &impl_Debconf_Frontend_epv,
};

/*** Stub implementations ***/

static Debconf_Frontend
impl_Debconf_Frontend__create(PortableServer_POA poa, CORBA_Environment * ev)
{
  Debconf_Frontend retval;
  impl_POA_Debconf_Frontend *newservant;
  PortableServer_ObjectId *objid;
  
  newservant = g_new0(impl_POA_Debconf_Frontend, 1);
  newservant->servant.vepv = &impl_Debconf_Frontend_vepv;
  newservant->poa = poa;
  POA_Debconf_Frontend__init((PortableServer_Servant) newservant, ev);
  objid = PortableServer_POA_activate_object(poa, newservant, ev);
  CORBA_free(objid);
  retval = PortableServer_POA_servant_to_reference(poa, newservant, ev);
  
  return retval;
}

static void
impl_Debconf_Frontend__destroy(impl_POA_Debconf_Frontend * servant,
			       CORBA_Environment * ev)
{
  PortableServer_ObjectId *objid;

  objid = PortableServer_POA_servant_to_id(servant->poa, servant, ev);
  PortableServer_POA_deactivate_object(servant->poa, objid, ev);
  CORBA_free(objid);
  
  POA_Debconf_Frontend__fini((PortableServer_Servant) servant, ev);
  g_free(servant);
}

static CORBA_boolean
impl_Debconf_Frontend_Boolean(impl_POA_Debconf_Frontend * servant,
			      CORBA_char * prompt, CORBA_Environment * ev)
{
  CORBA_boolean retval;

  return retval;
}

static CORBA_unsigned_long_long
impl_Debconf_Frontend_Multiselect(impl_POA_Debconf_Frontend * servant,
				  CORBA_unsigned_short count,
				  Debconf_Frontend_selects * choices,
				  CORBA_Environment * ev)
{
  CORBA_unsigned_long_long retval;

  return retval;
}

static void
impl_Debconf_Frontend_Note(impl_POA_Debconf_Frontend * servant,
			   CORBA_char * note, CORBA_Environment * ev)
{
}

static CORBA_char *
impl_Debconf_Frontend_Password(impl_POA_Debconf_Frontend * servant,
			       CORBA_char * prompt, CORBA_Environment * ev)
{
  CORBA_char *retval;

  return retval;
}

static CORBA_unsigned_short
impl_Debconf_Frontend_Select(impl_POA_Debconf_Frontend * servant,
			     CORBA_unsigned_short count,
			     Debconf_Frontend_selects * choices,
			     CORBA_Environment * ev)
{
  CORBA_unsigned_short retval;

  return retval;
}

static CORBA_char *
impl_Debconf_Frontend_String(impl_POA_Debconf_Frontend * servant,
			     CORBA_char * prompt, CORBA_Environment * ev)
{
  CORBA_char *retval;

  return retval;
}

static CORBA_char *
impl_Debconf_Frontend_Text(impl_POA_Debconf_Frontend * servant,
			   CORBA_char * prompt, CORBA_Environment * ev)
{
  CORBA_char *retval;
  char *out = '\0';
  char buf[1024];
  int sz = 1;

  printf(_("Enter . on a line by itself when you are done\n"));
  while (fgets(buf, sizeof(buf), stdin)) {
    if (strcmp(buf, ".\n") == 0) break;
    sz += strlen(buf);
    out = (char *) realloc(out, sz);
    memcpy(out + sz - strlen(buf) - 1, buf, strlen(buf));
  }
  out[sz-1] = 0;
  retval = CORBA_string_dup((CORBA_char *) out);
  free(out);
  
  return retval;
}
