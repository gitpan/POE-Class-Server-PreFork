#!/usr/bin/perl

use lib '../lib';
use strict;

# A simple preforked echo server
package My::EchoServer;

sub POE::Kernel::ASSERT_DEFAULT () { 1 }
#sub POE::Kernel::TRACE_REFCNT () { 1 }


use POE;

use base 'POE::Class::Conn::TCPStream';

sub handler_input {
    my ($self, $input) = @_[OBJECT, ARG0];
    return if $self->get_shutdown;
    print "<< $input\n";
    $self->get_wheel->put($input);
}

sub handler_start {
    $_[OBJECT]->SUPER::handler_start(@_[1 .. $#_]);
    printf "Conn session %d started\n", $_[SESSION]->ID;
}

sub handler_stop {
    $_[OBJECT]->SUPER::handler_stop(@_[1 .. $#_]);
    printf "Conn session %d stopped\n", $_[SESSION]->ID;
}

package main;

sub POE::Class::Server::PreFork::DEBUG () { 1 }

use POE qw/
    Class::Server::PreFork
    Class::Server::PreFork::TCP
/;

my $server = new POE::Class::Server::PreFork::TCP(
    port         => 'echo',
    conn_class   => 'My::EchoServer',
    max_requests => 10,
);
printf "Created server with ID %d\n", $server->ID;

my $preforker = new POE::Class::Server::PreFork(
    server            => $server,
    max_clients       => 15,
    min_spare_servers => 3,
    max_spare_servers => 10,
    start_servers     => 5,
    timeout           => 10,
);
printf "Created prefork server with ID %d\n", $preforker->ID;

my $session = $preforker->start;
printf "Created prefork session with ID %d\n", $session->ID;

$poe_kernel->run;



