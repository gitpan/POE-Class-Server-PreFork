#!/usr/bin/perl

my %Called;
use Test::More qw(no_plan);

BEGIN { use_ok('POE::Class::Server::PreFork') }
require_ok('POE::Class::Server::PreFork');
import POE::Class::Server::PreFork(':all');

ok(defined &COMM_ACK, 'COMM_ACK exported');
ok(defined &COMM_BUSY, 'COMM_BUSY exported');
ok(defined &COMM_NOT_BUSY, 'COMM_NOT_BUSY exported');

package My::PreFork::Thingy;

use POE;
use strict;

@My::PreFork::Thingy::ISA = qw(POE::Class);

sub create_states {
    my $self = shift;
    $poe_kernel->state(resume_accept => $self, 'handler_resume_accept');
}

*get_send_ack = sub {};
*set_send_ack = sub {};

sub send_ack {
    my $self = shift;
    $self->{send_ack} = shift if @_;
    $Called{send_ack}++;
    return $self->{send_ack};
}
*get_send_ack = *set_send_ack = *send_ack;

sub handler_start {
    my $self = $_[OBJECT];
    $Called{handler_start}++;
    $self->SUPER::handler_start(@_[1 .. $#_]);
}

sub handler_resume_accept {
    my $self = $_[OBJECT];
    $Called{handler_resume_accept}++;
}

package My::PreFork;

use strict;

use POE;
use POE::Class::Server::PreFork;

use Test::More;

@My::PreFork::ISA = qw(POE::Class::Server::PreFork);

sub create_states {
    my $self = shift;
    $self->SUPER::create_states;
    $poe_kernel->state(check_start_yield => $self, 'handler_start_check_yield');
}

sub handler_start {
    my $self = $_[OBJECT];
    $self->SUPER::handler_start(@_[1 .. $#_]);
    is($Called{send_ack}, 2, 'handler_start() called send ack');
    my $sendack = $self->get_server->send_ack;
    is($sendack, int($self->get_timeout / 2), 'handler_start() set send_ack to the proper value');
    is($self->get_server_check_ack, $self->get_timeout * 1.5, 'handler_start() set server_check_ack to the proper value');
    is($Called{handler_start}, 1, 'handler_start() started child session');
    $_[KERNEL]->yield('check_start_yield');
}

sub handler_forkoff {
    my $self = $_[OBJECT];
    $self->SUPER::handler_forkoff(@_[1 .. $#_]);
    $Called{handler_forkoff}++;
}

sub handler_start_check_yield {
    my $self = $_[OBJECT];
    is($Called{handler_forkoff}, 1, 'handler_start() yielded forkoff');
    $self->yield('shutdown');
}

package main;

use POE;

use strict;

my $server = My::PreFork->new;

can_ok($server, qw(
    busy_servers
    free_servers
    wheel_id_to_pid
    shutdown_on_error
    server
    min_spare_servers
    max_spare_servers
    start_servers
    max_clients
    timeout
    server_check_ack
    is_child

    handler_start
    handler_child
    handler_run_error
    handler_shutdown
    handler_server_closed
    handler_server_stderr
    handler_server_ack
    handler_server_timeout_check
    handler_forkoff
    handler_server_fork

    new
    DESTROY
    create_states
    server_timeout
    server_timeout_busy
    server_timeout_free
    server_start
    server_ack
    server_not_busy
    server_busy
    server_stdout
    server_stderr
));

isa_ok($server, 'POE::Class::Server::PreFork', 'new() returned a PreFork object');
isa_ok($server, 'POE::Class', 'POE::Class::Server::PreFork is derived from POE::Class');

my $busy_children = $server->get_busy_servers;
ok(defined $busy_children, 'busy_servers returned a defined value');
ok(ref($busy_children) eq 'HASH', 'busy_servers defaults to a hash');

my $free_children = $server->get_free_servers;
ok(defined $free_children, 'free_servers returned a defined value');
ok(ref($free_children) eq 'HASH', 'free_servers defaults to a hash');

my $map_children = $server->get_wheel_id_to_pid;
ok(defined $map_children, 'wheel_id_to_pid returned a defined value');
ok(ref($map_children) eq 'HASH', 'wheel_id_to_pid defaults to a hash');

my $shutdown_on_error = $server->get_shutdown_on_error;
ok(defined $shutdown_on_error, 'shutdown_on_error() returned a defined value');
is($shutdown_on_error, 1, 'shutdown_on_error defaults to 1');

my $child_object = $server->get_server;
ok(!defined $child_object, 'server defaults to undefined');

my $min_spare_servers = $server->get_min_spare_servers;
is($min_spare_servers, 1, 'min_spare_servers() defaults to 1');

my $max_spare_servers = $server->get_max_spare_servers;
is($max_spare_servers, 10, 'max_spare_servers() defaults to 10');

my $start_servers = $server->get_start_servers;
is($start_servers, 5, 'start_servers() defaults to 5');

my $max_clients = $server->get_max_clients;
is($max_clients, 50, 'max_clients() defaults to 50');

my $timeout = $server->get_timeout;
is($timeout, 0, 'timeout() defaults to 0');

my $server_check_ack = $server->get_server_check_ack;
is($server_check_ack, undef, 'server_check_ack() defaults to undef');

my $is_child = $server->get_is_child;
is($is_child, undef, 'is_child() defaults to undef');

my $thingy = My::PreFork::Thingy->new;

$server->set_timeout(10);

$server->set_server($thingy);
$server->start;

$poe_kernel->run;

