// config_signal.cc			-*-c++-*-
//
// Implementations and stuff for config_signal

#include "config_signal.h"

#include <sigc++/functors/mem_fun.h>
#include <apt-pkg/strutl.h>

signalling_config::signalling_config(Configuration *_user_config,
				     Configuration *_system_config,
				     Configuration *_theme_config)
  :user_config(_user_config), system_config(_system_config),
   theme_config(_theme_config)
{
  update_theme(system_config->Find(PACKAGE "::Theme", ""));

  this->connect(PACKAGE "::Theme", sigc::mem_fun(*this, &signalling_config::do_update_theme));
}

signalling_config::~signalling_config()
{
  for(connmap::iterator i=conn_table.begin();
      i!=conn_table.end();
      i++)
    delete i->second;
}

void signalling_config::do_update_theme()
{
  update_theme(system_config->Find(PACKAGE "::Theme", ""));
}

void signalling_config::update_theme(string newtheme)
{
  themeroot=newtheme;
  if(!themeroot.empty())
    themeroot=PACKAGE "::Themes::"+themeroot+"::";
}

void signalling_config::Set(string Name, string Value)
{
  user_config->Set(Name, Value);
  system_config->Set(Name, Value);

  connmap::iterator found=conn_table.find(Name);

  if(found!=conn_table.end())
    found->second->emit();
}

void signalling_config::Set(string Name, int Value)
{
  user_config->Set(Name.c_str(), Value);
  system_config->Set(Name.c_str(), Value);

  connmap::iterator found=conn_table.find(Name);

  if(found!=conn_table.end())
    found->second->emit();
}

void signalling_config::SetNoUser(string Name, string Value)
{
  system_config->Set(Name, Value);

  connmap::iterator found=conn_table.find(Name);

  if(found!=conn_table.end())
    found->second->emit();
}

void signalling_config::SetNoUser(string Name, int Value)
{
  system_config->Set(Name.c_str(), Value);

  connmap::iterator found=conn_table.find(Name);

  if(found!=conn_table.end())
    found->second->emit();
}

void signalling_config::connect(string name, const sigc::slot0<void> &slot)
{
  connmap::iterator found=conn_table.find(name);

  if(found!=conn_table.end())
    found->second->connect(slot);
  else
    {
      sigc::signal0<void> *sig=new sigc::signal0<void>;

      conn_table[name]=sig;
      sig->connect(slot);
    }
}

// right now, may emit too many signals..that's ok..?
void signalling_config::setcfg(Configuration *new_user_cfg,
			       Configuration *new_system_cfg,
			       Configuration *new_theme_cfg)
{
  Configuration *old_user_cfg=user_config;
  Configuration *old_system_cfg=system_config;
  Configuration *old_theme_cfg=theme_config;

  user_config=new_user_cfg;
  system_config=new_system_cfg;
  theme_config=new_theme_cfg;

  update_theme(system_config->Find(PACKAGE "::Theme", ""));

  for(connmap::iterator i=conn_table.begin();
      i!=conn_table.end();
      i++)
    if(Exists(i->first) ||
       old_user_cfg->Exists(i->first) ||
       old_system_cfg->Exists(i->first) ||
       (!themeroot.empty() && old_theme_cfg->Exists(i->first)))
      i->second->emit();
}

void signalling_config::Dump(ostream &out)
{
  user_config->Dump(out);
}
