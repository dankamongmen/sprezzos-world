/** \file status_widget.cc */
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

// Local includes
#include "status_widget.h"

#include "../tabs_manager.h"

// System includes
#include <QtGui/QApplication>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QStackedWidget>
#include <QtGui/QStyle>
#include <QtGui/QVBoxLayout>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      status_widget::status_widget(QWidget *parent)
        : QWidget(parent)
      {
        create_gui();

        update_changes_summary();
      }

      status_widget::~status_widget()
      {
      }

      void status_widget::create_gui()
      {
        stacked_widget = new QStackedWidget();

        QVBoxLayout *layout = new QVBoxLayout(this);
        layout->addWidget(stacked_widget);

        create_changes_widget();
        create_progress_widget();
      }

      void status_widget::create_changes_widget()
      {
        changes_widget = new QWidget();

        QHBoxLayout *main_layout = new QHBoxLayout(changes_widget);

        changes_summary = new QLabel();
        main_layout->addWidget(changes_summary);

        QDialogButtonBox *buttons = new QDialogButtonBox(Qt::Horizontal, this);
        main_layout->addWidget(buttons);

        // TODO: find proper icons
        show_changes_button = new QPushButton(qApp->style()->standardIcon(QStyle::SP_DialogApplyButton), "Show Changes", this);
        apply_changes_button = new QPushButton(qApp->style()->standardIcon(QStyle::SP_DialogApplyButton), "Apply Changes", this);
        resolve_dependencies_button = new QPushButton(qApp->style()->standardIcon(QStyle::SP_DialogApplyButton), "Resolve Dependeincies", this);
        cancel_button = new QPushButton(qApp->style()->standardIcon(QStyle::SP_DialogCancelButton), "Cancel", this);

        connect(show_changes_button, SIGNAL(clicked()), this, SLOT(show_changes_button_clicked()));
        connect(apply_changes_button, SIGNAL(clicked()), this, SLOT(apply_changes_button_clicked()));
        connect(resolve_dependencies_button, SIGNAL(clicked()), this, SLOT(resolve_dependencies_button_clicked()));
        connect(cancel_button, SIGNAL(clicked()), this, SLOT(cancel_button_clicked()));

        buttons->addButton(show_changes_button, QDialogButtonBox::ApplyRole);
        buttons->addButton(apply_changes_button, QDialogButtonBox::ApplyRole);
        buttons->addButton(resolve_dependencies_button, QDialogButtonBox::ApplyRole);
        buttons->addButton(cancel_button, QDialogButtonBox::RejectRole);

        stacked_widget->addWidget(changes_widget);
      }

      void status_widget::create_progress_widget()
      {
        progress_widget = new QWidget();

        stacked_widget->addWidget(progress_widget);
      }

      void status_widget::update_changes_summary()
      {
        changes_summary->setText("Short changes summary");

        resolve_dependencies_button->hide();
        show_changes_button->setEnabled(false);
        apply_changes_button->setEnabled(false);

      }

      void status_widget::apply_changes_button_clicked()
      {
	tabs_manager::get_instance()->open_perform_changes_tab(this);
      }

      void status_widget::cancel_button_clicked()
      {
      }

      void status_widget::resolve_dependencies_button_clicked()
      {
	tabs_manager::get_instance()->open_resolver_tab(this);
      }

      void status_widget::show_changes_button_clicked()
      {
	tabs_manager::get_instance()->open_changes_preview_tab(this);
      }

      // TODO: add proper logic hear
      void status_widget::current_tab_changed(tab::tab_type type)
      {
        if(type == tab::tab_resolver)
        {
          apply_changes_button->show();
          resolve_dependencies_button->hide();
        }
        else if(type == tab::tab_preview)
        {
          show_changes_button->setEnabled(false);
          apply_changes_button->hide();
          resolve_dependencies_button->show();
        }
        else
        {
          apply_changes_button->hide();
          resolve_dependencies_button->show();
        }
      }
    }
  }
}

#include "status_widget.moc"
