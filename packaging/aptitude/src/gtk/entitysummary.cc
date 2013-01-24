/** \file entitysummary.cc */         // -*-c++-*-


// Copyright (C) 2009 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "entitysummary.h"

#include <aptitude.h>

#include <cwidget/generic/util/ssprintf.h>

#include <generic/util/undo.h>

#include <gtkmm.h>

#include <gtk/hyperlink.h>
#include <gtk/info.h>
#include <gtk/packageinformation.h>
#include <gtk/pkgview.h>
#include <gtk/screenshot.h>

namespace cw = cwidget;

namespace gui
{
  namespace
  {
    class InfoTabButtons : public Gtk::VButtonBox
    {
      cw::util::ref_ptr<PkgEntity> e;

      Gtk::Button installButton;
      Gtk::Button removeButton;
      Gtk::Button purgeButton;
      Gtk::Button keepButton;
      Gtk::Button holdButton;
      Gtk::Button autoButton;
      Gtk::Button manualButton;

      void do_dispatch_action(PackagesAction action)
      {
	std::auto_ptr<undo_group> undo(new undo_group);
	{
	  aptitudeDepCache::action_group group(*apt_cache_file, undo.get());
	  e->dispatch_action(action, true);
	  e->dispatch_action(action, false);
	}
	if(!undo.get()->empty())
	  apt_undos->add_item(undo.release());
      }

      void insert_button(Gtk::Button *button,
			 const Glib::ustring &buttonText,
			 Gtk::StockID stockId,
			 PackagesAction action)
      {
	button->set_image(*manage(new Gtk::Image(stockId, Gtk::ICON_SIZE_BUTTON)));
	button->signal_clicked().connect(sigc::bind(sigc::mem_fun(*this, &InfoTabButtons::do_dispatch_action),
						    action));
	button->set_label(buttonText);

	add(*button);
	button->show();
      }

      void update_package_button_states(const std::set<pkgCache::PkgIterator> *changed_packages)
      {
	using cw::util::ssprintf;
	pkgCache::PkgIterator pkg;
	pkgCache::VerIterator ver;

	if(e.valid())
	  {
	    pkg = e->get_pkg();
	    ver = e->get_ver();
	  }

	if(!e.valid() || pkg.end())
	  return;

	if(changed_packages->find(pkg) == changed_packages->end())
	  return;

	std::set<PackagesAction> actions;
	e->add_actions(actions);

	pkgDepCache::StateCache &state = (*apt_cache_file)[pkg];
	pkgCache::VerIterator candver = state.CandidateVerIter(*apt_cache_file);

	// Don't show the user the option to install the package unless
	// it might at some point be installable.
	if(candver.end() || state.Status == 0)
	  installButton.hide();
	else
	  {
	    installButton.show();

	    if(state.Status == 1)
	      {
		installButton.set_label(ssprintf(_("Upgrade to %s version %s"),
						 pkg.Name(),
						 candver.VerStr()));
		installButton.set_image(*manage(new Gtk::Image(Gtk::Stock::GO_UP,
							       Gtk::ICON_SIZE_BUTTON)));
	      }
	    else if(state.Status == 2)
	      {
		installButton.set_label(ssprintf(_("Install %s version %s"),
						 pkg.Name(),
						 candver.VerStr()));
		installButton.set_image(*manage(new Gtk::Image(Gtk::Stock::ADD,
							       Gtk::ICON_SIZE_BUTTON)));
	      }
	    else if(state.Status == -1)
	      {
		installButton.set_label(ssprintf(_("Downgrade to %s version %s"),
						 pkg.Name(),
						 candver.VerStr()));
		installButton.set_image(*manage(new Gtk::Image(Gtk::Stock::GO_DOWN,
							       Gtk::ICON_SIZE_BUTTON)));
	      }
	  }

	if(state.Keep())
	  {
	    if((*apt_cache_file)->get_ext_state(pkg).selection_state == pkgCache::State::Hold)
	      keepButton.set_label(ssprintf(_("Don't hold %s at its current version."),
					    pkg.Name()));
	    else
	      keepButton.set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));
	  }
	else if(state.Delete())
	  {
	    if(state.iFlags & pkgDepCache::Purge)
	      keepButton.set_label(ssprintf(_("Cancel the purge of %s."), pkg.Name()));
	    else
	      keepButton.set_label(ssprintf(_("Cancel the removal of %s."), pkg.Name()));
	  }
	else if(state.Install())
	  {
	    if(state.Status == 1)
	      keepButton.set_label(ssprintf(_("Cancel the upgrade of %s."), pkg.Name()));
	    else if(state.Status == 2)
	      keepButton.set_label(ssprintf(_("Cancel the installation of %s."), pkg.Name()));
	    else if(state.Status == -1)
	      keepButton.set_label(ssprintf(_("Cancel the downgrade of %s."), pkg.Name()));
	    else if(state.Status == 1 && (state.iFlags & pkgDepCache::ReInstall))
	      keepButton.set_label(ssprintf(_("Cancel the reinstallation of %s."), pkg.Name()));
	    else
	      keepButton.set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));
	  }
	else
	  keepButton.set_label(ssprintf(_("Cancel any actions on %s."), pkg.Name()));

	// If the package isn't installed, the "remove" and "purge"
	// buttons shouldn't appear, since they're never going to be
	// available.
	removeButton.property_visible() = (state.Status != 2);
	purgeButton.property_visible() = (state.Status != 2);

	installButton.property_sensitive() =
	  (actions.find(Upgrade) != actions.end() ||
	   actions.find(Downgrade) != actions.end() ||
	   actions.find(Install) != actions.end());

	removeButton.property_sensitive() = (actions.find(Remove) != actions.end());
	purgeButton.property_sensitive() = (actions.find(Purge) != actions.end());
	keepButton.property_sensitive() = (actions.find(Keep) != actions.end());
	holdButton.property_sensitive() = (actions.find(Hold) != actions.end());

	autoButton.property_visible() = (actions.find(MakeAutomatic) != actions.end());
	manualButton.property_visible() = (actions.find(MakeManual) != actions.end());
      }

    public:
      InfoTabButtons(const cw::util::ref_ptr<PkgEntity> &_e)
	: e(_e)
      {
	using cw::util::ssprintf;

	(*apt_cache_file)->package_states_changed.connect(sigc::mem_fun(*this, &InfoTabButtons::update_package_button_states));

	std::string name(!e->get_pkg().end()
			 ? e->get_pkg().Name()
			 : "??");

	insert_button(&installButton,
		      "The user should never see this text.", Gtk::Stock::DIALOG_ERROR,
		      Install);
	insert_button(&removeButton,
		      ssprintf(_("Remove %s"), name.c_str()),
		      Gtk::Stock::REMOVE,
		      Remove);
	insert_button(&purgeButton,
		      ssprintf(_("Purge %s"), name.c_str()),
		      Gtk::Stock::CLEAR,
		      Purge);
	insert_button(&keepButton,
		      "The user should not see this text.",
		      Gtk::Stock::MEDIA_REWIND,
		      Keep);
	insert_button(&holdButton,
		      ssprintf(_("Hold %s at its current version."),
			       name.c_str()),
		      Gtk::Stock::MEDIA_PAUSE,
		      Hold);
	insert_button(&autoButton,
		      ssprintf(_("Mark %s as automatically installed."),
			       name.c_str()),
		      Gtk::StockID(),
		      MakeAutomatic);
	insert_button(&manualButton,
		      ssprintf(_("Mark %s as manually installed."),
			       name.c_str()),
		      Gtk::StockID(),
		      MakeManual);

	std::set<pkgCache::PkgIterator> dummy;
	dummy.insert(e->get_pkg());
	update_package_button_states(&dummy);
      }
    };

    struct compare_provider_lists_by_name
    {
      pkg_name_lt base;
    public:
      bool operator()(const std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > &p1,
		      const std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > &p2) const
      {
	return base(p1.first, p2.first);
      }
    };
  }

  void show_entity_summary(const cw::util::ref_ptr<Entity> &ent,
			   Gtk::TextView *textView)
  {
    using cw::util::ssprintf;

    Glib::RefPtr<Gtk::TextBuffer> textBuffer = Gtk::TextBuffer::create();

    cw::util::ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();
    // Set to where the screenshot should be inserted, if one should
    // be inserted.
    Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> screenshotAnchor;
    pkgCache::PkgIterator pkg;
    pkgCache::VerIterator ver;

    if(pkg_ent.valid())
      {
	pkg = pkg_ent->get_pkg();
	ver = pkg_ent->get_ver();
      }

    // \todo We should gracefully handle missing versions.
    if(!pkg_ent.valid() || pkg.end() || ver.end())
      {
	// \todo This should be a generic function in gui.h.  In fact,
	// maybe the collation should be a generic function in
	// generic/apt/apt.h.
	if(pkg_ent.valid() && !pkg.end() && !pkg.ProvidesList().end())
	  {
	    textBuffer->insert(textBuffer->end(), ssprintf(_("%s is a virtual package provided by:\n"),
							   pkg.Name()));
	    // Collate the providers by the providing package.
	    std::map<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > providers;
	    for(pkgCache::PrvIterator prv = pkg.ProvidesList(); !prv.end(); ++prv)
	      {
		providers[prv.OwnerPkg()].push_back(prv.OwnerVer());
	      }

	    // Put them in alphabetical order.
	    std::vector<std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > >
	      provider_pairs(providers.begin(), providers.end());

	    std::sort(provider_pairs.begin(), provider_pairs.end(),
		      compare_provider_lists_by_name());

	    for(std::vector<std::pair<pkgCache::PkgIterator, std::vector<pkgCache::VerIterator> > >::const_iterator
		  it = provider_pairs.begin(); it != provider_pairs.end(); ++it)
	      {
		using cw::util::transcode;
		textBuffer->insert(textBuffer->end(), transcode(L"\t\x2022 "));
		add_hyperlink(textBuffer, textBuffer->end(),
			      it->first.Name(),
			      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					 it->first,
					 PkgEntity::get_ver(it->first)));

		textBuffer->insert(textBuffer->end(), "  (");
		bool first = true;
		for(std::vector<pkgCache::VerIterator>::const_iterator verIt = it->second.begin();
		    verIt != it->second.end(); ++verIt)
		  {
		    if(first)
		      first = false;
		    else
		      textBuffer->insert(textBuffer->end(), ", ");

		    add_hyperlink(textBuffer, textBuffer->end(),
				  verIt->VerStr(),
				  sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
					     verIt->ParentPkg(),
					     *verIt));
		  }

		textBuffer->insert(textBuffer->end(), ")\n");
	      }
	  }
	else
	  textBuffer->set_text("");
      }
    else
      {
        PackageInformation info(pkg, ver);

	Glib::RefPtr<Gtk::TextBuffer::Tag> nameTag = textBuffer->create_tag();
	nameTag->property_size() = 20 * Pango::SCALE;

	Glib::RefPtr<Gtk::TextBuffer::Tag> fieldNameTag = textBuffer->create_tag();
	fieldNameTag->property_weight() = 2 * Pango::SCALE;

	textBuffer->insert_with_tag(textBuffer->end(),
				    info.Name(),
				    nameTag);
	textBuffer->insert(textBuffer->end(), " ");
	add_hyperlink(textBuffer, textBuffer->end(), _("(more info...)"),
		      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
				 pkg, ver));
	textBuffer->insert(textBuffer->end(), "\n");
        textBuffer->insert(textBuffer->end(), info.ShortDescription());
        textBuffer->insert(textBuffer->end(), "\n");


	screenshotAnchor = textBuffer->create_child_anchor(textBuffer->end());

	// TODO: insert a horizontal rule here (how?)

	textBuffer->insert(textBuffer->end(), "\n");

	if (ver)
	  {
	    //pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

	    textBuffer->insert_with_tag(textBuffer->end(), _("Version: "), fieldNameTag);
	    textBuffer->insert(textBuffer->end(), info.Version());

	    textBuffer->insert(textBuffer->end(), "\n");
	    textBuffer->insert(textBuffer->end(), "\n");

	    std::wstring longdesc = get_long_description(ver, apt_package_records);

	    textBuffer->insert_with_tag(textBuffer->end(), _("Description: "), fieldNameTag);

	    textBuffer->insert(textBuffer->end(), info.LongDescription());

            textBuffer->insert(textBuffer->end(), "\n");

            textBuffer->insert_with_tag(textBuffer->end(), _("Source: "), fieldNameTag);

            add_hyperlink(textBuffer, textBuffer->end(),
			  info.SourcePackage(),
			  sigc::bind(sigc::mem_fun(*pMainWindow, &AptitudeWindow::add_packages_tab),
				     "?source-package(^" + backslash_escape_nonalnum(info.SourcePackage()) + "$)"));

	    textBuffer->insert(textBuffer->end(), "\n");
	    textBuffer->insert(textBuffer->end(), "\n");
	    add_debtags(textBuffer, textBuffer->end(),
			pkg, fieldNameTag);
	  }
	else
	  {
	  }
      }

    textView->set_buffer(textBuffer);

    // This all has to be done after we set the buffer anyway.
    //
    // TODO: better layout is probably good.  Is it?
    if(pkg_ent.valid())
      {
	textBuffer->insert(textBuffer->end(), "\n\n");
	Glib::RefPtr<Gtk::TextBuffer::ChildAnchor> button_box_anchor =
	  textBuffer->create_child_anchor(textBuffer->end());

	Gtk::ButtonBox *button_box = manage(new InfoTabButtons(pkg_ent));

	button_box->show();

	textView->add_child_at_anchor(*button_box, button_box_anchor);

	// Note the use of sigc::ref / ent.weak_ref() to ensure this
	// connection is removed when the buttons are destroyed.
      }

    if(!pkg.end() && screenshotAnchor)
      {
	screenshot_image *thumbnail =
	  manage(new screenshot_image(pkg.Name(), aptitude::screenshot_thumbnail));

	textView->add_child_at_anchor(*thumbnail, screenshotAnchor);
	thumbnail->show();
      }
  }
}
