# $Id: Makefile.PL,v 1.4 2003/03/10 14:00:45 pajas Exp $

use strict;

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'XML::XUpdate::LibXML',
    'VERSION_FROM'	=> 'lib/XML/XUpdate/LibXML.pm', # finds $VERSION
    'PREREQ_PM'		=> {
			    'XML::LibXML' => 1.53,
			    'XML::LibXML::Iterator' => 0
			   },
    'AUTHOR'            => 'Petr Pajas (pajas@matfyz.cz)',
    'ABSTRACT'          => 'update XML documents according to XUpdate file',
    'EXE_FILES'         => ['xupdate'],
);

