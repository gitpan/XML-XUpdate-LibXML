# $Id: LibXML.pm,v 1.6 2002/06/24 14:39:41 pajas Exp $

package XML::XUpdate::LibXML;

use XML::LibXML;
use strict;
use vars qw(@ISA $debug $VERSION);

BEGIN {
  $debug=0;
  $VERSION = '0.2.3';
}

sub strip_space {
  my ($text)=@_;
  $text=~s/^\s*//;
  $text=~s/\s*$//;
  return $text;
}

sub new {
  my $class=(ref($_[0]) || $_[0]);
  return bless [{},"http://www.xmldb.org/xupdate"], $class;
}

sub _set_var {
  my ($self,$name,$value)=@_;
  $self->[0]->{$name}=$value;
}

sub _get_var {
  my ($self,$name)=@_;
  return $self->[0]->{$name};
}

sub set_namespace {
  my ($self,$URI)=@_;
  $self->[1]=$URI;
}

sub namespace {
  my ($self)=@_;
  return $self->[1];
}

sub process {
  my ($self,$dom,$updoc)=@_;
  return unless ref($self);

  print STDERR "Updating $dom\n" if $debug;
  foreach my $command ($updoc->getDocumentElement()->childNodes()) {
    if ($command->nodeType == XML_ELEMENT_NODE) {
      if ($command->getNamespaceURI() eq $self->namespace()) {
	print STDERR "applying ",$command->toString(),"\n" if $debug;
	$self->xupdate_command($dom,$command);
      } else {
	print STDERR "Ignorint element ",$command->toString(),"\n" if $debug;
      }
    }
  }
}

sub get_text {
  my ($self,$node)=@_;
  my $text="";
  foreach ($node->childNodes()) {
    if ($_->nodeType() == XML_TEXT_NODE ||
	$_->nodeType() == XML_CDATA_SECTION_NODE) {
      $text.=$_->getData();
    }
  }
  return strip_space($text);
}

sub append {
  my ($self,$node,$results)=@_;
  foreach (@$results) {
    if ($_->nodeType == XML_ATTRIBUTE_NODE) {
      $node->setAttributeNS($_->getNamespaceURI,$_->getName(),$_->getValue);
    } else {
      $node->appendChild($_);
    }
  }
}

sub insert_after {
  my ($self,$node,$results)=@_;
  foreach (reverse @$results) {
    $node->parentNode()->insertAfter($_,$node);
  }
}

sub insert_before {
  my ($self,$node,$results)=@_;
  foreach (@$results) {
    $node->parentNode()->insertBefore($_,$node);
  }
}

sub append_child {
  my ($self,$node,$results,$child)=@_;
  if ($child ne "") {
    $child=$node->findvalue($child) unless $child =~/^\s*\D+\s*$/;
    my @children=$node->childNodes();
    my $after=$children[$child-1];
    if ($child>1 and $after) {
      $self->insert_after($after,$results);
    } elsif (@children and $after) {
      $self->insert_before($children[0],$results);
    } else {
      $self->append($node,$results);
    }
  } else {
    $self->append($node,$results);
  }
}

sub update {
  my ($self,$node,$results)=@_;

  if ($node->nodeType == XML_TEXT_NODE ||
      $node->nodeType == XML_CDATA_SECTION_NODE) {
    $self->insert_after($node,$results);
    $node->unbindNode();
  } elsif ($node->nodeType == XML_ATTRIBUTE_NODE ||
	   $node->nodeType == XML_PI_NODE) {
    $node->setValue(strip_space(join "", map { $_->toString() } @$results));
  } elsif ($node->nodeType == XML_ELEMENT_NODE) {
    foreach ($node->childNodes()){
      $_->unbindNode();
    }
    $self->append($node,$results);
  }
}

sub remove {
  my ($self, $node)=@_;
  $node->unbindNode();
}

sub rename {
  my ($self,$node,$name)=@_;
  $node->setName($name);
}

sub process_instructions {
  my ($self, $dom, $command)=@_;

  my @result=();
  
  foreach my $inst ($command->childNodes()) {
    print STDERR "instruction ",$command->toString(),"\n" if $debug;
    if ( $inst->nodeType == XML_ELEMENT_NODE ) {
      if ( $inst->getLocalName() eq 'element' ) {
	my $new;
	if ($inst->hasAttribute('namespace') and
	    $inst->getAttribute('name')=~/:/) {
	  $new=$dom->getOwnerDocument()->createElementNS(
							 $inst->getAttribute('namespace'),
							 $inst->getAttribute('name')
							);
	} else {
	  $new=$dom->getOwnerDocument()->createElement($inst->getAttribute('name'));
	}
	$self->append($new,$self->process_instructions($dom,$inst));
	push @result,$new;
      } elsif ( $inst->getLocalName() eq 'attribute' ) {
	if ($inst->hasAttribute('namespace') and
	    $inst->getAttribute('name')=~/:/) {
	  push @result,
	    $dom->getOwnerDocument()->
	      createAttributeNS(
				$inst->getAttribute('namespace'),
				$inst->getAttribute('name'),
				$self->get_text($inst)
			       );
	} else {
	  push @result,
	    $dom->getOwnerDocument()->
	      createAttribute(
			      $inst->getAttribute('name'),
			      $self->get_text($inst)
			     );
	}
      } elsif (  $inst->getLocalName() eq 'text' ) {
	push @result,$dom->getOwnerDocument()->createTextNode($self->get_text($inst));
      } elsif ( $inst->getLocalName() eq 'processing-instruction' ) {
	push @result,$dom->getOwnerDocument()->createProcessingInstruction(
						       $inst->getAttribute('name'),
						       $self->get_text($inst)
						      );
      } elsif ( $inst->getLocalName() eq 'value-of' ) {
	my $value=$dom->getOwnerDocument()->findvalue($self->get_select($inst));
	push @result,$dom->getOwnerDocument()->createTextNode($value);
      } else {
	# not in XUpdate DTD but in examples of XUpdate WD
	push @result,$dom->getOwnerDocument()->importNode($inst) 
	  unless ($inst->getNamespaceURI eq $self->namespace());
      }
    } elsif ( $inst->nodeType == XML_CDATA_SECTION_NODE ||
	      $inst->nodeType == XML_TEXT_NODE) {
      push @result,$dom->getOwnerDocument()->importNode($inst);
    }
  }
  return \@result;
}

sub get_select {
  my ($self,$node)=@_;
  my $select=$node->getAttribute('select');
  $select=~s{\$([A-Za-z0-9._\-:]+)}{_get_var($1)}gesx; # TODO: complete NMTOKEN match
  return $select;
}

sub xupdate_command {
  my ($self,$dom,$command)=@_;
  my $child;
  return unless ($command->getType == XML_ELEMENT_NODE);
  my $select=$self->get_select($command);
  if ($command->getLocalName() eq 'variable') {
    if ($select ne "") {
      $self->_set_var($command->getAttribute('name'),$select);
    } else {
      $self->_set_var($self->get_text($command));
    }
  } else {
    if ($select ne "") {

      my @refnodes=$dom->findnodes($select);
      if (@refnodes) {
	if ($command->getLocalName eq 'insert-after') {
	  my $results=$self->process_instructions($dom,$command);
	  $self->insert_after($refnodes[0],$results);
	} elsif ($command->getLocalName eq 'insert-before') {
	  my $results=$self->process_instructions($dom,$command);
	  $self->insert_before($refnodes[0],$results);
	  my $results=$self->process_instructions($dom,$command);
	} elsif ($command->getLocalName eq 'append') {
	  my $results=$self->process_instructions($dom,$command);
	  $child=$command->getAttribute('child');
	  $self->append_child($refnodes[0],$results,$child);
	} elsif ($command->getLocalName eq 'update') {
	  my $results=$self->process_instructions($dom,$command);
	  # Well, XUpdate WD is not very specific about this.
          # The content of this element should be PCDATA only.
          # I'm extending WD by allowing instruction list.
	  $self->update($refnodes[0],$results);
	} elsif ($command->getLocalName eq 'remove') {
	  $self->remove($refnodes[0]);
	} elsif ($command->getLocalName eq 'rename') {
	  $self->rename($refnodes[0],$self->get_text($command));
	}
      }
    }
  }
}


1;

__END__

=pod

=head1 NAME

XML::XUpdate::LibXML - Simple implementation of XUpdate format

=head1 SYNOPSIS

use XML::LibXML;
use XML::XUpdate::LibXML;

$parser  = XML::LibXML->new();
$dom     = $parser->parse_file("mydoc.xml");
$actions = $parser->parse_file("update.xml");

$xupdate = XML::LibXML::XUpdate->new();
$xupdate->process($dom->getDocumentElement(), $actions);
print $dom->toString(),"\n";

=head1 DESCRIPTION

This module implements the XUpdate format described in XUpdate Working
Draft from 2000-09-14 (http://www.xmldb.org/xupdate/xupdate-wd.html).
The implementation is based on XML::LibXML DOM API.


=head2 C<new>

    my $xupdate = XML::LibXML::XUpdate->new();

Creates a new XUpdate object. You may use this object to update
several different DOM trees using several different XUpdate
descriptions. The advantage of it is that an xupdate object remembers
values all variables declared in XUpdate documents.

=head2 C<$xupdate->process($document_dom,$xupdate_dom)>

This function takes two DOM trees as its arguments. It works by
updating the first tree according to all XUpdate commands included in
the second one. All XUpdate commands must be children of the root
element of the second tree and must all belong to XUpdate namespace
"http://www.xmldb.org/xupdate". The namespace URI may be changed
with set_namespace method.

=head2 C<$xupdate->set_namespace($URI)>

You may use this method to change the namespace of XUpdate elements.
The default namespace is "http://www.xmldb.org/xupdate".

=head2 C<$xupdate->namespace()>

Returns XUpdate namespace URI used by XUpdate processor to identify
XUpdate commands.

=head2 EXPORT

None.

=head1 AUTHOR


Petr Pajas, pajas@matfyz.cz

=head1 SEE ALSO

L<XML::LibXML>

=cut
