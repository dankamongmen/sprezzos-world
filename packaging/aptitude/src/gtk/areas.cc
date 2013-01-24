/** \file areas.cc */    // -*-c++-*-

#include "areas.h"

#include <aptitude.h>

#include <gtk/toplevel/model.h>

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <gtkmm/stock.h>

using boost::make_shared;
using boost::shared_ptr;
using gui::toplevel::area_info;
using gui::toplevel::area_list;
using gui::toplevel::create_area_info;
using gui::toplevel::create_area_list;

namespace gui
{
  areas::~areas()
  {
  }

  namespace
  {
    class areas_impl : public areas
    {
      shared_ptr<area_info> upgrade;
      shared_ptr<area_info> browse;
      shared_ptr<area_info> search;
      shared_ptr<area_info> go;
      shared_ptr<area_info> preferences;

      shared_ptr<area_list> all_areas;

      // Handle the wonky protocol for getting a pixbuf from a stock
      // ID.
      static Glib::RefPtr<Gdk::Pixbuf>
      get_stock_icon(const Gtk::BuiltinStockID &stock)
      {
        shared_ptr<Gtk::Image>
          img(new Gtk::Image(stock,
                             Gtk::ICON_SIZE_BUTTON));

        return img->get_pixbuf();
      }

      // Support constructing all_areas from the rest of the members.
      static shared_ptr<area_list>
      make_all_areas(const shared_ptr<area_info> &upgrade,
                     const shared_ptr<area_info> &browse,
                     const shared_ptr<area_info> &search,
                     const shared_ptr<area_info> &go,
                     const shared_ptr<area_info> &preferences)
      {
        std::vector<shared_ptr<area_info> > all_areas;

        all_areas.push_back(upgrade);
        all_areas.push_back(browse);
        all_areas.push_back(search);
        all_areas.push_back(go);
        all_areas.push_back(preferences);

        return create_area_list(all_areas);
      }

    public:
      areas_impl()
        : upgrade(create_area_info(_("Upgrade"),
                                   _("Keep your computer up-to-date."),
                                   get_stock_icon(Gtk::Stock::GO_UP))),
          browse(create_area_info(_("Browse"),
                                  _("Explore the available packages."),
                                  get_stock_icon(Gtk::Stock::INDEX))),
          search(create_area_info(_("Find"),
                                  _("Search for packages."),
                                  get_stock_icon(Gtk::Stock::FIND))),
          go(create_area_info(_("Go"),
                              _("Finalize and apply your changes to the system."),
                              get_stock_icon(Gtk::Stock::APPLY))),
          preferences(create_area_info(_("Preferences"),
                                    _("Configure aptitude."),
                                    get_stock_icon(Gtk::Stock::PREFERENCES))),
          all_areas(make_all_areas(upgrade, browse, search, go,
                                   preferences))
      {
      }

      boost::shared_ptr<area_list> get_areas() { return all_areas; }
      boost::shared_ptr<area_info> get_browse() { return browse; }
      boost::shared_ptr<area_info> get_go() { return go; }
      boost::shared_ptr<area_info> get_preferences() { return preferences; }
      boost::shared_ptr<area_info> get_search() { return search; }
      boost::shared_ptr<area_info> get_upgrade() { return upgrade; }
    };
  }

  shared_ptr<areas> create_areas()
  {
    return make_shared<areas_impl>();
  }
}
