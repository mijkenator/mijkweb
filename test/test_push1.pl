#!/usr/bin/perl 
#===============================================================================
#
#         FILE: test_push1.pl
#
#        USAGE: ./test_push1.pl  
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

print "----------------------------------------------------------------------------\n";
my $req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"applogin1", "password":"apppassword"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

print "----------------------------------------------------------------------------\n";

while(my $message = <STDIN>){
    chomp($message);
    last if $message eq 'quit';
    my $req = POST 'http://localhost:3033/auth/push',
                 [ request => '{"type":"push", "message":"'.$message.'"}'];
    print $ua->request($req)->as_string;
    print "----------------------------------------------------------------------------\n";
}
