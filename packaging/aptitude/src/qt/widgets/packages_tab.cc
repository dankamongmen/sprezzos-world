/** \file packages_tab.cc */
//
// Copyright (C) 2010 Piotr Galiszewski
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

// Local includes:
#include "packages_tab.h"

#include "aptitude.h"

// System includes:
#include <QtGui/QComboBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QListView>
#include <QtGui/QPushButton>
#include <QtGui/QTreeView>
#include <QtGui/QVBoxLayout>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      packages_tab::packages_tab(QWidget *parent)
	: tab(tab::tab_packages, tab::closing_policy_destroy, parent)
      {
	create_gui();
      }

      packages_tab::~packages_tab()
      {
      }

      void packages_tab::create_gui()
      {
	QHBoxLayout *layout = new QHBoxLayout(this);
	layout->setMargin(0);
	layout->setSpacing(0);

	layout->addWidget(create_left_widget());
	layout->addWidget(create_right_widget());
	layout->setStretch(1, 100);
      }

      QWidget * packages_tab::create_left_widget()
      {
	QWidget *left_widget = new QWidget();
	QVBoxLayout *left_layout = new QVBoxLayout(left_widget);

	QWidget *show_widget = new QWidget();
	QHBoxLayout *show_layout = new QHBoxLayout(show_widget);

	// TODO: this should be ShowOptionsModel
	QStringList show_tems;
	show_tems << _("all") << _("installed") << _("installed locally") << _("new")
		  << _("not installed") << _("virtual");

	show_combobox = new QComboBox();
	show_combobox->addItems(show_tems);

	show_layout->addWidget(new QLabel(_("Show:")));
	show_layout->addWidget(show_combobox);

	filters_view = new QListView();
	filters_view->setSizePolicy(QSizePolicy(QSizePolicy::Ignored, QSizePolicy::Expanding));

	manage_filters_button = new QPushButton(_("Manage Filters"));
	connect(manage_filters_button, SIGNAL(clicked()), this, SLOT(manage_filters_clicked()));

	left_layout->addWidget(show_widget);
	left_layout->addWidget(filters_view);
	left_layout->addWidget(manage_filters_button);

	return left_widget;
      }

      QWidget * packages_tab::create_right_widget()
      {
	QWidget *right_widget = new QWidget();
	QVBoxLayout *right_layout = new QVBoxLayout(right_widget);

	QWidget *search_widget = new QWidget();
	QHBoxLayout *search_layout = new QHBoxLayout(search_widget);

	search_edit = new QLineEdit();

	QStringList search_items;
	search_items << _("Name") << _("Name and Description") << _("Maintainer") << _("Version");

	search_by_combobox = new QComboBox();
	search_by_combobox->addItems(search_items);

	search_layout->addStretch();
	search_layout->addWidget(new QLabel(_("Find:")));
	search_layout->addWidget(search_edit);
	search_layout->addWidget(new QLabel(_("by")));
	search_layout->addWidget(search_by_combobox);

	packages_view = new QTreeView();
	packages_view->setRootIsDecorated(false);

	right_layout->addWidget(search_widget);
	right_layout->addWidget(packages_view);

	return right_widget;
      }

      void packages_tab::manage_filters_clicked()
      {

      }
    }
  }
}

#include "packages_tab.moc"
