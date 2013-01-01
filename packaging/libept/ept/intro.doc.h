/** \mainpage EPT library documentation

\section Introduction to the EPT library

The EPT library is a collection of <em>data providers</em>, that are interface
to access various kind of information about \em packages.

All data providers are independent from each other and can be instantiated
alone.  This allows to instantiate only the amount of infrastructure that is
needed for a job, and to use alternative data provider implementations where
appropriate.

Because of this, for example, libept can be used in applications that already
use libapt-pkg directly: if the Apt object is not instantiated, then libept
will not use the apt-pkg library at all.

Package names (and, where used, versions) are what brings all the data
providers together: all the data providers access information about Debian
packages, which have a well defined namespace.  libept represents a package
simply with a std::string containing its name: to get various kinds of
information about a package, one just queries it by name to many data
providers:

\code
	Apt apt;
	Debtags debtags;
	Popcon popcon;

	// Get the Apt record for the debtags package
	PackageRecord record(apt.rawRecord("debtags"));

	// Get the tags of the debtags package
	std::set<Tag> tags = debtags.getTagsOfItem("debtags");

	// Get the popcon score of the debtags package
	float score = popcon.score("debtags");

	// Print the records of all image editors
	std::set<Tag> imageEditors;
	imageEditors.insert(debtags.vocabulary().tagByName("use::editing"));
	imageEditors.insert(debtags.vocabulary().tagByName("works-with::image"));
	imageEditors.insert(debtags.vocabulary().tagByName("role::program"));
	std::set<std::string> packages = debtags.getItemsHavingTags(imageEditors);
	for (std::set<std::string>::const_iterator i = packages.begin();
		i != packages.end(); ++i)
	{
		PackageRecord rec(apt.rawRecord(*i));
		PackageState state = apt.state(*i);
		// Also show whether a package is installed
		if (state.isInstalled())
			std::cout << rec.package() << " i " << rec.shortDescription();
		else
			std::cout << rec.package() << " - " << rec.shortDescription();
	}
\endcode

A data provider does not need to know about the others, nor it needs to
implement a specific interface: the only requirement on a data provider is that
it can be queried using package names.

The only methods that are found in all data providers are hasData() and
timestamp(), that can be used to query if a data provider is working and how up
to date it is.

The data providers currently implemented are:

\li ept::apt::Apt: access the APT database.
\li ept::debtags::Debtags: access the Debtags tag information.
\li ept::popcon::Popcon: access Popcon package scores.
\li ept::textsearch::TextSearch: fast Xapian-based full text search on package description, and more.

It is easy to implement more data providers.  It is also easy to implement new
data providers outside of this library, as long as they can be queried using
package names.
*/
