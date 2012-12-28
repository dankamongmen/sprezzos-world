/*
 * cdebconf frontend, corba servant, text frontend main program, will
 * only work in an xterm, haha
 */

/*
 * To test:
 * $ xemacs dcf-text.gnorba
 * $ GNOME_GNORBA_PATH=`pwd` goad-browser
 */

#include "dcf-textimpl.c"

int main(int argc, char* argv[]) {
  CORBA_ORB orb;
  CORBA_Environment *ev;
  PortableServer_POA root_poa;
  PortableServer_POAManager pm;
  CORBA_Object name_service;
  Debconf_Frontend dcf_text;
  CORBA_char *ior;
  
  ev = g_new0(CORBA_Environment,1);

  CORBA_exception_init(ev);

  orb = gnorba_CORBA_init(&argc, argv, GNORBA_INIT_SERVER_FUNC, ev);
  if (ev->_major != CORBA_NO_EXCEPTION) {
    fprintf(stderr, "Error: could not initializing ORB: %s\n", CORBA_exception_id(ev));
    exit(1);
  }

  root_poa = (PortableServer_POA)
    CORBA_ORB_resolve_initial_references(orb, "RootPOA", ev);
  if (ev->_major != CORBA_NO_EXCEPTION) {
    fprintf(stderr, "Error: could not get RootPOA: %s\n", CORBA_exception_id(ev));
    exit(1);
  }

  dcf_text = impl_Debconf_Frontend__create(root_poa, ev);

  pm = PortableServer_POA__get_the_POAManager(root_poa, ev);
  PortableServer_POAManager_activate(pm, ev);

  ior = CORBA_ORB_object_to_string(orb, dcf_text, ev);
  fprintf(stderr, "%s\n", ior);
  CORBA_free(ior);

  name_service = gnome_name_service_get();
  switch (goad_server_register(CORBA_OBJECT_NIL, dcf_text, "dcf-text", "server", ev)) {
  case -2:
    /* There is a dcf-text running; bring it forward and quit */
    dcf_text = goad_server_activate_with_repo_id(NULL, "IDL:GNOME/dcf-text:1.0",
						 0, NULL);
    CORBA_exception_free (ev);
    exit(0);
  case -1:
    g_warning("There was trouble with the name server.\n"
	      "aborting...\n");
    exit(1);
  default:
    g_warning("Beautiful, isn't it?\n");
  }
  
  CORBA_ORB_run(orb, ev);

  CORBA_exception_free (ev);
  return 0;
}
