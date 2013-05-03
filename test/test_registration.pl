#!/usr/bin/perl 
#===============================================================================
#
#         FILE: test_registration.pl
#
#        USAGE: ./test_registration.pl  
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
#      CREATED: 16.01.2013 07:58:35
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

my $ua = LWP::UserAgent->new;
$ua->timeout(45);
#$ua->cookie_jar(new HTTP::Cookies(file=>'/tmp/cookies.dat', autosave=>1));
$ua->cookie_jar({});


print "----------------------------------------------------------------------------\n";
#my $req = POST 'http://localhost:3033/user/create',
my $req = POST 'http://localhost/_mijkweb/user/create',
             [ request => '{"type":"createsysuser", "login":"syslogin1", "password":"syspassword", "email":"sysemail"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

print "----------------------------------------------------------------------------\n";

