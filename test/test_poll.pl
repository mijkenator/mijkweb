#!/usr/bin/perl 
#===============================================================================
#
#         FILE: test_poll.pl
#
#        USAGE: ./test_poll.pl  
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

my $login = $ARGV[0] || "applogin2";
my $password = $ARGV[1] || "apppassword";

print "--------------------------------------------------------------\n";
my $req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"'.$login.'", "password":"'.$password.'"}'];
print $ua->request($req)->as_string;
#print Data::Dumper::Dumper($response);

print "-------------------------------------------------------------\n";
my $mseq = 0;
while(1){
    $mseq = poll_func($ua, $mseq)
}


sub poll_func
{
    my ($ua, $seq) = @_;
    my $req = POST 'http://localhost:3033/auth/poll',
                  [ request => '{"type":"poll","seq":'.$seq.'}'];
     
    my $resp = $ua->request($req);
    #print "Resp: ".$resp->decoded_content." \n";
    print $resp->as_string;
    print "-----------------------------------------------------\n";
    return JSON->new->decode($resp->decoded_content)->{"seq"};
}

