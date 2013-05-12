#!/usr/bin/perl 
#===============================================================================
#
#         FILE: static_page_builder.pl
#
#        USAGE: ./static_page_builder.pl [Templates directory] [Output directory]  
#
#  DESCRIPTION: generate static html pages
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: mijkenator 
# ORGANIZATION: 
#      VERSION: 1.0
#      CREATED: 08.05.2013 18:43:08
#     REVISION: ---
#===============================================================================

use strict;
use warnings;
use HTML::Template;

my ($template_dir, $pub_dir) = ($ARGV[0] || '/home/ashim/work/mijkweb/tmpl', $ARGV[1] || '/opt/mijkweb_pub/html');
for(glob($template_dir."/*.tmpl")){
    print "processing -> ",$_,"\n";
    my $template = HTML::Template->new(filename => $_, path => [$template_dir]);
    open(my $fh, ">".$pub_dir."/".(/([^\/]+)\.tmpl$/)[0].".html");
    print $fh $template->output;
    close $fh;
}

