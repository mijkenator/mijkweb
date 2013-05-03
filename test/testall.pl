#!/usr/bin/perl 
#===============================================================================
#
#         FILE: testall.pl
#
#        USAGE: ./testall.pl  
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
#      CREATED: 29.01.2013 10:15:14
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
$ua->cookie_jar({});


#clean database
`mysql -u root mijkweb < /home/ashim/work/mijkweb/mysql.sql`;

print "AFTER DB RECREATION \n";

# create new sysuser testsysuser

print "----------------------------------------------------------------------------\n";
my $dev_guid;
my $req = POST 'http://localhost:3033/user/create',
             [ request => '{"type":"createsysuser", "login":"testsysuser", "password":"syspassword", "email":"sysemail"}'];
#print $ua->request($req)->as_string;
my $res = $ua->request($req);
if ($res->is_success) {
    print $res->content."\n";
    my $json = JSON->new()->decode($res->content);
    print Data::Dumper::Dumper($json);
    $dev_guid = $json->{developer_guid};
}else{
    print $res->status_line, "\n";
}
print "----------------------------------------------------------------------------\n";

$req = POST 'http://localhost:3033/user/create',
             [ request => '{"type":"createsysuser", "login":"testsysuser", "password":"syspassword", "email":"sysemail"}'];
print $ua->request($req)->as_string;
print "----------------------------------------------------------------------------\n";


my $ret = `./test_app_registration.pl $dev_guid`;
print "TEST_APP_REG: \n".$ret."\n";
`./reg_many_appusers.pl $dev_guid`;

$req = POST 'http://localhost:3033/user/create',
             [ request => '{"type":"createsysuser", "login":"testsysuser2", "password":"syspassword", "email":"sysemail2"}'];
#print $ua->request($req)->as_string;
$res = $ua->request($req);
if ($res->is_success) {
    print $res->content."\n";
    my $json = JSON->new()->decode($res->content);
    print Data::Dumper::Dumper($json);
    $dev_guid = $json->{developer_guid};
}else{
    print $res->status_line, "\n";
}
print "----------------------------------------------------------------------------\n";

$ret = `./test_app_registration.pl $dev_guid 2`;
print "TEST_APP_REG: \n".$ret."\n";


#`tmux send-keys -t mijkweb:polling.0 './test_poll.pl applogin1 apppassword' Enter`;
#`tmux send-keys -t mijkweb:polling.1 './test_poll.pl applogin2 apppassword' Enter`;
#`tmux send-keys -t mijkweb:polling.2 './test_poll.pl applogin3 apppassword' Enter`;
#`tmux send-keys -t mijkweb:polling.3 './test_push.pl ' Enter`;


print "-------------------------------------------------------------\n";
$req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"testsysuser", "password":"syspassword"}'];
print $ua->request($req)->as_string;
print "-------------------------------------------------------------\n";
$req = POST 'http://localhost:3033/auth/user/profile',
             [ request => '{"type":"get_profile"}'];
print $ua->request($req)->as_string;
print "-------------------------------------------------------------\n";
$req = POST 'http://localhost:3033/auth/check/session',
             [ request => '{"type":"check_session"}'];
print $ua->request($req)->as_string;
print "-------------------------------------------------------------\n";
$req = POST 'http://localhost:3033/auth/user/set',
             [ request => '{"type":"set_key", "key":"key1", "value":"value11111"}'];
print $ua->request($req)->as_string;
print "-------------------------------------------------------------\n";

$req = POST 'http://localhost:3033/logout', [ request => '{"type":"logout"}'];
print $ua->request($req)->as_string;

#exit(0);

$req = POST 'http://localhost:3033/auth',
             [ request => '{"type":"login", "login":"testsysuser2", "password":"syspassword"}'];
print $ua->request($req)->as_string;

$req = POST 'http://localhost:3033/auth/user/set',
             [ request => '{"type":"set_key", "key":"key1", "value":"value22222"}'];
print $ua->request($req)->as_string;

$req = POST 'http://localhost:3033/auth/user/get',
             [ request => '{"type":"get_key", "key":"key1"}'];
print $ua->request($req)->as_string;
$req = POST 'http://localhost:3033/auth/user/set',
             [ request => '{"type":"set_key", "key":"key2", "value":"value22222"}'];
print $ua->request($req)->as_string;
$req = POST 'http://localhost:3033/auth/user/set',
             [ request => '{"type":"set_key", "key":"key3", "value":"value22222"}'];
print $ua->request($req)->as_string;
$req = POST 'http://localhost:3033/auth/user/del',
             [ request => '{"type":"del_key", "key":"key2"}'];
print $ua->request($req)->as_string;
$req = POST 'http://localhost:3033/auth/user/get',
             [ request => '{"type":"get_key", "key":"key2"}'];
print $ua->request($req)->as_string;

print "-------------------------------------------------------------\n";

$req = POST 'http://localhost:3033/logout', [ request => '{"type":"logout"}'];
print $ua->request($req)->as_string;

