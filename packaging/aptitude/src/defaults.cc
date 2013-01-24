// defaults.cc
//
//  Copyright 1999 Daniel Burrows
//
//  Defaults for Aptitude.

#include <cwidget/config/keybindings.h>
#include <cwidget/config/colors.h>

#include "mine/cmine.h"

#include "pkg_tree.h"
#include "pkg_node.h"

namespace cw = cwidget;

static void init_keybindings()
{
  cw::config::global_bindings.set("CycleNext", cw::config::key(KEY_F(6), true));
  cw::config::global_bindings.set("CyclePrev", cw::config::key(KEY_F(7), true));

  cw::config::global_bindings.set("QuitProgram", cw::config::key(L'Q', false));

  cw::config::global_bindings.set("UpdatePackageList", cw::config::key(L'u', false));
  cw::config::global_bindings.set("MarkUpgradable", cw::config::key(L'U', false));
  cw::config::global_bindings.set("ForgetNewPackages", cw::config::key(L'f', false));
  cw::config::global_bindings.set("ChangePkgTreeLimit", cw::config::key(L'l', false));
  cw::config::global_bindings.set("ChangePkgTreeGrouping", cw::config::key(L'G', false));
  cw::config::global_bindings.set("ChangePkgTreeSorting", cw::config::key(L'S', false));

  cw::config::global_bindings.set("CycleOrder", cw::config::key(L'o', false));

  cw::config::global_bindings.set("Install", cw::config::key(L'+', false));
  cw::config::global_bindings.set("Remove", cw::config::key(L'-', false));
  cw::config::global_bindings.set("Hold", cw::config::key(L'=', false));
  cw::config::global_bindings.set("Purge", cw::config::key(L'_', false));
  cw::config::global_bindings.set("Keep", cw::config::key(L':', false));
  cw::config::global_bindings.set("SetAuto", cw::config::key(L'M', false));
  cw::config::global_bindings.set("ClearAuto", cw::config::key(L'm', false));
  cw::config::global_bindings.set("ForbidUpgrade", cw::config::key(L'F', false));
  cw::config::global_bindings.set("Reinstall", cw::config::key(L'L', false));

  cw::config::global_bindings.set("Dependencies", cw::config::key(L'd', false));
  cw::config::global_bindings.set("ReverseDependencies", cw::config::key(L'r', false));
  cw::config::global_bindings.set("InfoScreen", cw::config::key(KEY_ENTER, true));
  cw::config::global_bindings.set("Versions", cw::config::key(L'v', false));
  cw::config::global_bindings.set("Changelog", cw::config::key(L'C', false));

  cw::config::global_bindings.set("DoInstallRun", cw::config::key(L'g', false));
  cw::config::global_bindings.set("InstallSingle", cw::config::key(L'I', false));
  cw::config::global_bindings.set("ChangePkgDisplayFormat", cw::config::key(L'p', false));
  cw::config::global_bindings.set("ChangePkgStatusFormat", cw::config::key(L's', false));

  cw::config::global_bindings.set("ToggleColumnHeaders", cw::config::key(L'h', false));

  cw::config::global_bindings.set("ShowHideDescription", cw::config::key(L'D', false));

  cw::config::global_bindings.set("DescriptionUp", cw::config::key(L'a', false));
  cw::config::global_bindings.set("DescriptionDown", cw::config::key(L'z', false));
  cw::config::global_bindings.set("DescriptionCycle", cw::config::key(L'i', false));

  cw::config::global_bindings.set("DpkgReconfigure", cw::config::key(L'R', false));
  cw::config::global_bindings.set("BugReport", cw::config::key(L'B', false));

  // Hierarchy editor
  cw::config::global_bindings.set("Commit", cw::config::key(L'N', false));
  cw::config::global_bindings.set("SaveHier", cw::config::key(L'S', false));
  cw::config::global_bindings.set("EditHier", cw::config::key(L'E', false));

  cw::config::global_bindings.set("SearchBroken", cw::config::key(L'b', false));

  cw::config::global_bindings.set("NextSolution", cw::config::key(L'.', false));
  cw::config::global_bindings.set("PrevSolution", cw::config::key(L',', false));
  cw::config::global_bindings.set("FirstSolution", cw::config::key(L'<', false));
  cw::config::global_bindings.set("LastSolution", cw::config::key(L'>', false));
  cw::config::global_bindings.set("ExamineSolution", cw::config::key(L'e', false));
  cw::config::global_bindings.set("ApplySolution", cw::config::key(L'!', false));
  cw::config::global_bindings.set("DumpResolver", cw::config::key(L'*', false));

  cw::config::global_bindings.set("SolutionActionReject", cw::config::key(L'r', false));
  cw::config::global_bindings.set("SolutionActionApprove", cw::config::key(L'a', false));

  pkg_tree::init_bindings();
  pkg_tree_node::init_bindings();
  cmine::init_bindings();
}

static void init_styles()
{
  cw::set_style("PkgNotInstalled", cw::style());
  cw::set_style("PkgIsInstalled", cw::style_attrs_on(A_BOLD));
  cw::set_style("PkgToHold", cw::style_fg(COLOR_WHITE) + cw::style_attrs_on(A_REVERSE));
  cw::set_style("PkgToRemove", cw::style_fg(COLOR_MAGENTA) + cw::style_attrs_on(A_REVERSE));
  cw::set_style("PkgBroken", cw::style_fg(COLOR_RED) + cw::style_attrs_on(A_REVERSE));
  cw::set_style("PkgToInstall", cw::style_fg(COLOR_GREEN) + cw::style_attrs_on(A_REVERSE));
  cw::set_style("PkgToUpgrade", cw::style_fg(COLOR_CYAN) + cw::style_attrs_on(A_REVERSE));
  cw::set_style("PkgToDowngrade", cw::style_attrs_on(A_BOLD));


  cw::set_style("PkgNotInstalledHighlighted",
	    cw::style_attrs_flip(A_REVERSE));
  cw::set_style("PkgIsInstalledHighlighted",
	    cw::style_attrs_on(A_BOLD) + cw::style_attrs_flip(A_REVERSE));
  cw::set_style("PkgToHoldHighlighted",
	    cw::style_fg(COLOR_WHITE));
  cw::set_style("PkgToRemoveHighlighted",
	    cw::style_fg(COLOR_MAGENTA));
  cw::set_style("PkgBrokenHighlighted",
	    cw::style_fg(COLOR_RED));
  cw::set_style("PkgToInstallHighlighted",
	    cw::style_fg(COLOR_GREEN));
  cw::set_style("PkgToUpgradeHighlighted",
	    cw::style_fg(COLOR_CYAN));
  cw::set_style("PkgToDowngradeHighlighted",
	    cw::style_attrs_on(A_BOLD) + cw::style_attrs_flip(A_REVERSE));



  cw::set_style("DepBroken", cw::style_fg(COLOR_BLACK)+cw::style_bg(COLOR_RED));

  cw::set_style("MediaChange", cw::style_fg(COLOR_YELLOW)+cw::style_bg(COLOR_RED)+cw::style_attrs_on(A_BOLD));
  cw::set_style("Progress", cw::style_fg(COLOR_BLUE)+cw::style_bg(COLOR_YELLOW));
  cw::set_style("DownloadProgress", cw::style_fg(COLOR_BLUE)+cw::style_bg(COLOR_YELLOW));
  cw::set_style("DownloadHit", cw::style_fg(COLOR_BLACK)+cw::style_bg(COLOR_GREEN));

  cw::set_style("ChangelogNewerVersion", cw::style_attrs_on(A_BOLD));
  cw::set_style("Bullet", cw::style_fg(COLOR_YELLOW)+cw::style_attrs_on(A_BOLD));
  cw::set_style("TrustWarning", cw::style_fg(COLOR_RED)+cw::style_bg(COLOR_BLACK)+cw::style_attrs_on(A_BOLD));

  cw::set_style("SolutionActionRejected", cw::style_bg(COLOR_RED));
  cw::set_style("SolutionActionApproved", cw::style_bg(COLOR_GREEN));
}

void init_defaults()
{
  init_keybindings();
  init_styles();
}
