#!/usr/bin/perl 
#===============================================================================
#
#         FILE: test_auth.pl
#
#        USAGE: ./test_auth.pl  
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
#      CREATED: 03.01.2013 13:55:38
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
my $req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"admin", "password":"lalalala"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

print "----------------------------------------------------------------------------\n";
$req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"admin", "password":"superpass"}'];
print $ua->request($req)->as_string;
print "----------------------------------------------------------------------------\n";
print "Sleep for a while... \n";
sleep(10);
$req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"admin", "password":"superpass"}'];
print $ua->request($req)->as_string;
print "----------------------------------------------------------------------------\n";

my $i = 12;
while($i){
    sleep(10);    
    $req = POST 'http://localhost:3033/auth/testc/testm',
                 [ request => '{"type":"ikpuktralala"}'];
    print $ua->request($req)->as_string;
    print "----------------------------------------------------------------------------\n";
    $i--
}

print "Super sleep \n";
sleep(120);
$req = POST 'http://localhost:3033/auth/testc/testm',
             [ request => '{"type":"ikpuktralala"}'];
print $ua->request($req)->as_string;
print "----------------------------------------------------------------------------\n";
