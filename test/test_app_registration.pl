#!/usr/bin/perl 
#===============================================================================
#
#         FILE: test_app_registration.pl
#
#        USAGE: ./test_app_registration.pl  
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
#      CREATED: 17.01.2013 18:17:24
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
my $extra = $ARGV[1] || '';

my $ua = LWP::UserAgent->new;
$ua->timeout(45);
#$ua->cookie_jar(new HTTP::Cookies(file=>'/tmp/cookies.dat', autosave=>1));
$ua->cookie_jar({});

print "----------------------------------------------------------------------------\n";
my $req = POST 'http://localhost:3033/app/user/create',
             [ request => '{"type":"createappuser", "applogin":"applogin1'.$extra.'","apppassword":"apppassword", "developer_guid":"'.$dev_key.'"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

#exit(0);
#sleep(2);

print "----------------------------------------------------------------------------\n";
$req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"testsysuser'.$extra.'", "password":"syspassword"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);
print "----------------------------------------------------------------------------\n";

#exit(0);
#sleep(2);

$req = POST 'http://localhost:3033/auth/user/create',
             [ request => '{"type":"createappuser", "applogin":"applogin2'.$extra.'","apppassword":"apppassword"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

print "----------------------------------------------------------------------------\n";

#exit(0);
#sleep(2);

$req = POST 'http://localhost:3033/auth/user/create',
             [ request => '{"type":"createappuser", "applogin":"applogin3'.$extra.'","apppassword":"apppassword"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

print "----------------------------------------------------------------------------\n";

