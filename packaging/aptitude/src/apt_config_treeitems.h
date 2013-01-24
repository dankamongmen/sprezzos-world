// apt_config_treeitems.h          -*-c++-*-
//
//   Copyright (C) 2007-2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef APT_CONFIG_TREEITEMS_H
#define APT_CONFIG_TREEITEMS_H

#include <cwidget/widgets/treeitem.h>

namespace cwidget
{
  class fragment;
}

namespace aptitude
{
  namespace ui
  {
    namespace config
    {
      /** \brief The interface for configuration list-items.
       */
      class config_treeitem : virtual public cwidget::widgets::treeitem
      {
      public:
	/** \brief A signal emitted when the description of this tree-item
	 *  needs to be refreshed.
	 */
	sigc::signal0<void> description_changed;

	/** \brief Option information
	 *
	 *  These methods retrieve information about the option for
	 *  presentation to the user.
	 */
	// @{

	/** \brief Retrieve a long description of this configuration item.
	 */
	virtual cwidget::fragment *get_long_description() const = 0;

	// @}
      };

      /** \brief Generate a configuration item for the given Boolean option.
       *
       *  \param description       A textual description of the configuration item.
       *  \param long_description  A longer description of the configuration item,
       *                           as a fragf-style pattern with no arguments.
       *  \param item    The configuration item that is to be managed.
       *  \param dflt    The default value if the option is not set.
       *
       *  \return a structure describing the new configuration item.
       *  The caller is responsible for destroying the treeitem
       *  object.
       *
       *  Selecting the option and pressing "Confirm" will toggle it on
       *  or off.
       */
      cwidget::widgets::treeitem *make_boolean_item(const std::wstring &description,
				     const std::string   &long_description,
				     const std::string   &item,
				     const bool          dflt);

      /** \brief Generate a tree item for the given string option.
       *
       *  \param description       A textual description of the configuration item.
       *  \param long_description  A longer description of the configuration item,
       *                           as a fragf-style pattern with no arguments.
       *  \param item    The configuration item that is to be managed.
       *  \param dflt    The default value if the option is not set.
       *
       *  \return a structure describing the new configuration item.
       *  The caller is responsible for destroying the treeitem
       *  object.
       *
       *  Selecting the option and pressing "Confirm" will display a
       *  prompt to enter a new value for the option.
       */
      cwidget::widgets::treeitem *make_string_item(const std::wstring &description,
				    const std::string  &long_description,
				    const std::string  &item,
				    const std::string  &dflt);

      /** \brief Information about a single option in a multiple-choice setting. */
      class radio_choice
      {
	std::string value;
	std::string untranslated_description;
	std::string untranslated_long_description;

      public:
	radio_choice()
	{
	}

	radio_choice(const std::string &_value,
		     const std::string &_untranslated_description,
		     const std::string &_untranslated_long_description)
	  : value(_value),
	    untranslated_description(_untranslated_description),
	    untranslated_long_description(_untranslated_long_description)
	{
	}

	const std::string &get_value() const
	{
	  return value;
	}

	const std::string &get_untranslated_description() const
	{
	  return untranslated_description;
	}

	const std::string &get_untranslated_long_description() const
	{
	  return untranslated_long_description;
	}
      };

      /** \brief Generate a tree item for the given multi-choice option.
       *
       *  \param description       A textual description of the configuration item.
       *  \param long_description  A longer description of the configuration item,
       *                           as a fragf-style pattern with no arguments.
       *  \param item    The configuration item that is to be managed.
       *  \param choices The permitted choices for the radio item.
       *  \param dflt    The default value if the option is not set.
       *
       *  \return a structure describing the new configuration item.
       *  The caller is responsible for destroying the treeitem
       *  object.
       *
       *  Selecting the option and pressing "Confirm" will display a
       *  prompt to choose a new value for the option.
       */
      cwidget::widgets::treeitem *make_radio_item(const std::wstring &description,
				   const std::string  &long_description,
				   const std::string  &item,
				   const std::vector<radio_choice> &choices,
				   const std::string  &dflt);
    }
  }
}

#endif // APT_CONFIG_TREEITEMS_H
