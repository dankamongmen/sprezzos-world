// config_signal.h			-*-c++-*-
//


#ifndef CONFIG_SIGNAL_H
#define CONFIG_SIGNAL_H

#include <apt-pkg/configuration.h>
#include <sigc++/signal.h>
#include <sigc++/functors/slot.h>
#include <sigc++/trackable.h>

#include <iosfwd>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>
#include <vector>

/** \brief A framework for signals that are based on the apt configuration code.
 *
 * 
 *  This uses a thin wrapper around the apt Configuration class.
 *
 *  This works in large part because apt-pkg itself does not (thankfully)
 *  modify configuration items after it is initialized.
 *  
 *  Important note: this configuration class supports "transparent themes".
 *  If PACKAGE::Theme is set, Find*(key) operations will look up key,
 *  followed by PACKAGE::Themes::(PACKAGE::theme)::key.
 *  
 *  This only breaks for things that use Tree -- which means that such code
 *  might need to explicitly understand themes in order to work.  That is: if
 *  it is appropriate for two disparate hierarchies to be "merged", you have to
 *  do this on your own.  The default Tree call simply uses the theme as
 *  a backing store.
 *  
 *  IMPORTANT NOTE: setting values in the PACKAGE::Themes hierarchy will
 *  not trigger signal emission on "shadowing" nodes!  Because themes are
 *  expected to be static, I do not think this is a big deal, but you should
 *  be aware of it.
 *  
 *  MORE IMPORTANT NOTE: Altering the value of PACKAGE::Theme will
 *  not trigger ANY signal emission right now.  (this *will* be a nuisance
 *  and I hope to change it eventually)
 *  
 *  
 *  In order to allow only non-default options to be saved, this does the
 *  following (hack :P ) -- it stores TWO configuration dictionaries, updating
 *  BOTH whenever an option is set, but only saving from the user's.  (in
 *  fact, it uses three -- ouch!)
 * 
 * \file config_signal.h
 */

class signalling_config:public sigc::trackable
{
  Configuration *user_config, *system_config, *theme_config;

  typedef std::map<string, sigc::signal0<void> *> connmap;

  connmap conn_table;

  // The current "theme" root, cached.  (ie, if the theme is "bland",
  // this will be "PACKAGE::UI::Themes::bland::")
  std::string themeroot;
  void update_theme(std::string theme);
  void do_update_theme();

public:
  // Assumes that all settings initially contained in user_config are
  // also contained in system_config.
  signalling_config(Configuration *_user_config,
		    Configuration *_system_config,
		    Configuration *_theme_config);
  ~signalling_config();

  inline string Find(const char *Name,const char *Default = 0)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->Find(Name, Default);
    else
      return theme_config->Find(themeroot+Name, Default);
  }
  inline string Find(string Name,const char *Default = 0)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->Find(Name, Default);
    else
      return theme_config->Find(themeroot+Name, Default);
  }

  inline string FindFile(const char *Name,const char *Default = 0)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindFile(Name, Default);
    else
      return theme_config->FindFile((themeroot+Name).c_str(), Default);
  }

  inline string FindDir(const char *Name,const char *Default = 0)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindDir(Name, Default);
    else
      return theme_config->FindDir((themeroot+Name).c_str(), Default);
  }

  inline std::vector<string> FindVector(const char *Name)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindVector(Name);
    else
      return theme_config->FindVector(themeroot+Name);
  }

  inline std::vector<string> FindVector(string Name)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindVector(Name);
    else
      return theme_config->FindVector(themeroot+Name);
  }

  inline int FindI(const char *Name,int Default = 0)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindI(Name, Default);
    else
      return theme_config->FindI(themeroot+Name, Default);
  }

  inline int FindI(string Name,bool Default = 0)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindI(Name, Default);
    else
      return theme_config->FindI(themeroot+Name, Default);
  }

  inline bool FindB(const char *Name,bool Default = false)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindB(Name, Default);
    else
      return theme_config->FindB(themeroot+Name, Default);
  }

  inline bool FindB(string Name,bool Default = false)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->FindB(Name, Default);
    else
      return theme_config->FindB(themeroot+Name, Default);
  }

  void Set(string Name,string Value);
  void Set(string Name,int Value);

  /** Sets Name to Value without modifying the user's configuration. */
  void SetNoUser(string Name, string Value);

  /** Sets Name to Value without modifying the user's configuration. */
  void SetNoUser(string name, int Value);

  /** Switches to a different configuration tree (this will involve
   *  emitting various signals)
   *
   *  This assumes that the "user" configuration tree has been
   *  appropriately modified and that new_system_cfg has the
   *  appropriate correspondence with it.
   */
  void setcfg(Configuration *new_user_cfg,
	      Configuration *new_system_cfg,
	      Configuration *new_theme_cfg);

  inline bool Exists(string Name)
  {
    if(system_config->Exists(Name))
      return true;
    else
      return !themeroot.empty() && theme_config->Exists(themeroot+Name);
  }

  bool Exists(const char *Name)
  {
    if(system_config->Exists(Name))
      return true;
    else
      return !themeroot.empty() && theme_config->Exists(themeroot+Name);
  }

  inline bool ExistsUser(string Name)
  {
    return user_config->Exists(Name);
  }

  inline const Configuration::Item *Tree(const char *Name)
  {
    if(themeroot.empty() || system_config->Exists(Name))
      return system_config->Tree(Name);
    else
      return theme_config->Tree((themeroot+Name).c_str());
  }

  // FIXME: this is a hack needed to support the workaround.  (for load_config)
  //        I think.
  Configuration *get_cfg(bool theme)
  {
    return theme?theme_config:system_config;
  }

  void connect(string name, const sigc::slot0<void> &slot);

  void Dump(std::ostream &out);
};

// Used to generate stack-scoped changes to a config member.
//
// TODO: should take a config_signal, but I need Clear() and
// config_signal doesn't implement that.
class config_change_pusher
{
  const std::string key;

  std::string old_value;

  Configuration &cfg;

public:
  config_change_pusher(const std::string &_key, const std::string &val,
		       Configuration &_cfg)
    : key(_key), cfg(_cfg)
  {
    old_value = cfg.Find(key);
    cfg.Set(key, val);
  }

  config_change_pusher(const std::string &_key, const char *val,
		       Configuration &_cfg)
    : key(_key), cfg(_cfg)
  {
    old_value = cfg.Find(key);
    cfg.Set(key, val);
  }

  config_change_pusher(const std::string &_key, int val,
		       Configuration &_cfg)
    : key(_key), cfg(_cfg)
  {
    old_value = cfg.Find(key);
    cfg.Set(key.c_str(), val);
  }

  config_change_pusher(const std::string &_key, bool val,
		       Configuration &_cfg)
    : key(_key), cfg(_cfg)
  {
    old_value = cfg.Find(key);
    cfg.Set(key, val ? "true" : "false");
  }

  ~config_change_pusher()
  {
    cfg.Set(key, old_value);
  }
};

#endif
