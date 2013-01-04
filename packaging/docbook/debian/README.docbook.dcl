From: Adam Di Carlo <adam@onshore.com>
Subject: DOCBOOK: objection to docbook.dcl
To: docbook@lists.oasis-open.org
Cc: docbook-tc@oasis-open.org
Date: Wed, 21 Mar 2001 02:35:31 -0500


Shipped with the DocBook DTDs from 2.4.1 and up is 'docbook.dcl', an
SGML declaration for use with DocBook documents.  However, this
declartion is unnecessarily restrictive, to the level where it is
rather cumbersome to implement.

My argument is that the DocBook declaration should diverge from the SP
(and OpenSP) implied declarations only where the divergance expresses
a real necessity to diverge.  This is based on the principle that
software (including SGML parsers) should be tolerant of what they
accept.  The unnecessarily broad divergance of the shipped Docbook
declaration puts a burden on document engineers using DocBook.

I am considering here only the DocBook SGML DTD, since I presume the
Declaration is rather irrelevant for XML files, since all XML files
have the same XML declaration applied to them.

I consider here 'docbook.dcl' as shipped with DocBook 4.1.

Major problems:

 OMITTAG is turned off (why?)

 NAMELEN is too short

 Document Character set is too restrictive

 SUBDOC is turned off (why?)


Description:

* OMITTAG is turned off

'OMITTAG' is turned off in 'docbook.dcl', disallowing markup
minimization of any sort.  This is on in the implied declaration of
both Jade and OpenJade. This creates problems because documents using
the default declaration for their parser will have a valid document,
but if the user decides to be more fasidious and user the docbook SGML
declaration, sudden their document will not be valid.

The major problem is that trying to turn this on will make a large
number of existing SGML DocBook instances invalid.


* NAMELEN is too short

The NAMELEN quantity set in docbook.dcl is set to 45, rather than the
default SP NAMELEN of 99999999.

A number of users have complained of problems due to this limitation
(do a google search on 'docbook namelen' to see what I mean) in any
cases (such as the SUSE Linux distribution) where the declaration is
enforced.

Quoting <URL:http://xml.coverpages.org/wlw14.html>:

   Care should be used when changing these since creating a variant
   syntax may make it difficult for some SGML systems to process
   documents created with that syntax.  The best means of guaranteeing
   portability between different SGML systems and applications is to
   use the reference concrete syntax as much as possible.

One wonders why we need to diverge from the reference concrete syntax
here.


* Document Character set it too restrictive

As an example, to workaround limitations in the support of KOI-R SDATA
entities in Jade and OpenJade, KOI-R users have to use unicode
entities.  With the docbook.dcl file, these entities are disallowed,
although they are perfectly valid with the implied SP declaration.
Example of being disallowed:

  jade:/usr/share/sgml/entities/sgml-iso-entities-8879.1986/ISOcyr1.ent:1:16:E: \
  "1072" is not a character number in the document character set


* SUBDOC is turned off

Why is it necessary to disallow SUBDOC in DocBook SGML documents?
Seems like some authors may wish to use this, even if its not fully
supported by existing stylesheets.



I hope I got my facts correct, and that this commentary is useful.

-- 
.....Adam Di Carlo....adam@onshore.com.....<URL:http://www.onshored.com/>

