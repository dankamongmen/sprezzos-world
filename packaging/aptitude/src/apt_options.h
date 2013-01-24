// apt_options.h
//
//  Copyright 2000 Daniel Burrows
//

#ifndef APT_OPTIONS_H
#define APT_OPTIONS_H

/** \brief The main options dialog
 * 
 *  \file apt_options.h
 */

namespace cwidget
{
  namespace util
  {
    template<class T> class ref_ptr;
  }

  namespace widgets
  {
    class widget;

    typedef util::ref_ptr<widget> widget_ref;
  }
}

// hmm, maybe just make this a global variable that gets shown and hidden?
cwidget::widgets::widget_ref make_ui_options_dialog();
cwidget::widgets::widget_ref make_misc_options_dialog();
cwidget::widgets::widget_ref make_dependency_options_dialog();


namespace aptitude
{
  namespace ui
  {
    namespace config
    {
      cwidget::widgets::widget_ref make_options_tree();
    }
  }
}

#endif
