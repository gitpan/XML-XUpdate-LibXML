# $Id: Makefile.PL,v 1.2 2002/03/06 13:38:11 pajas Exp $

use strict;

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'XML::XUpdate::LibXML',
    'VERSION_FROM'	=> 'lib/XML/XUpdate/LibXML.pm', # finds $VERSION
    'PREREQ_PM'		=> {
			    'XML::LibXML' => 1.31
			   }, 
    'AUTHOR'            => 'Petr Pajas (pajas@matfyz.cz)',
    'ABSTRACT'          => 'update XML documents according to XUpdate file',
    'EXE_FILES'         => ['xupdate'],
);

