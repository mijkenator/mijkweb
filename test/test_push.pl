#!/usr/bin/perl 
#===============================================================================
#
#         FILE: test_push.pl
#
#        USAGE: ./test_push.pl  
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
#      CREATED: 16.12.2012 18:02:55
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

my $response = $ua->request(GET 'http://localhost:3030/test/elli/cookie');
print Data::Dumper::Dumper($response);
print "----------------------------------------------------------------------------\n";

my $message = $ARGV[0] || 'mimimimi';

my $req = POST 'http://localhost:3030/push',
              [ request => '{"type":"push", "message":"'.$message.'", "channel":"ch1"}'];
 
print $ua->request($req)->as_string;
print "----------------------------------------------------------------------------\n";
