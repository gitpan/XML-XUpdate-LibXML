# $Id: Makefile.PL,v 1.3 2002/10/29 10:49:52 pajas Exp $

use strict;

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'XML::XUpdate::LibXML',
    'VERSION_FROM'	=> 'lib/XML/XUpdate/LibXML.pm', # finds $VERSION
    'PREREQ_PM'		=> {
			    'XML::LibXML' => 1.52
			   },
    'AUTHOR'            => 'Petr Pajas (pajas@matfyz.cz)',
    'ABSTRACT'          => 'update XML documents according to XUpdate file',
    'EXE_FILES'         => ['xupdate'],
);

