#!/usr/bin/perl 
#===============================================================================
#
#         FILE: reg_many_appusers.pl
#
#        USAGE: ./reg_many_appusers.pl  
#
#  DESCRIPTION: 
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: YOUR NAME (), 
# ORGANIZATION: 
#      VERSION: 1.0
#      CREATED: 27.03.2013 20:07:59
#     REVISION: ---
#===============================================================================

use strict;
use warnings;
use LWP;
use JSON;
use HTTP::Request::Common qw(POST GET);
use LWP::UserAgent;
use Data::Dumper;
use HTTP::Cookies;

my $dev_key = $ARGV[0] || "7664b6b6-2fb1-4e56-bb65-eeb852d0d7c3";
my $extra = $ARGV[1] || 20;

my $ua = LWP::UserAgent->new;
$ua->timeout(45);
$ua->cookie_jar({});

foreach my $i (1..$extra){
    my $req = POST 'http://localhost:3033/app/user/create',
                 [ request => '{"type":"createappuser", "applogin":"applogin1'.$i.'","apppassword":"apppassword", "developer_guid":"'.$dev_key.'"}'];
    print $ua->request($req)->as_string;
}

