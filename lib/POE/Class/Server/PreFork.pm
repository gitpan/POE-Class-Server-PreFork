package POE::Class::Server::PreFork;

use constant COMM_ACK      => 1;
use constant COMM_BUSY     => 2;
use constant COMM_NOT_BUSY => 3;

BEGIN {
    unless (defined &DEBUG) {
        eval 'sub DEBUG () { 0 }';
    }
}


use vars qw/$VERSION @ISA @EXPORT_OK %EXPORT_TAGS/;
use strict;

use Carp;
use POE qw/
    Wheel::Run
    Filter::Line
    Class
/;

@ISA = qw(POE::Class);

$VERSION = '0.01-beta';

@EXPORT_OK = qw(
    COMM_ACK
    COMM_BUSY
    COMM_NOT_BUSY
);

%EXPORT_TAGS = (all => \@EXPORT_OK);

require Exporter;
*import = \&Exporter::import;

use POE::Class::Attribs
    busy_servers      => {},
    free_servers      => {},
    wheel_id_to_pid   => {},
    shutdown_on_error => 1,
    server            => undef,
    min_spare_servers => 1,
    max_spare_servers => 10,
    start_servers     => 5,
    max_clients       => 50,
    timeout           => 0,
    server_check_ack  => undef,
    is_child          => undef;


# Handlers

sub handler_start {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    $self->SUPER::handler_start(@_[1 .. $#_]);

    if (!$self->get_min_spare_servers or $self->get_min_spare_servers < 1) {
        $self->set_min_spare_servers(1);
    }
    if (!$self->get_max_spare_servers or $self->get_max_spare_servers <= $self->get_min_spare_servers) {
        $self->set_max_spare_servers($self->get_min_spare_servers + 1);
    }
    if (!$self->get_max_clients) {
        $self->set_max_clients(50);
    }
    if (!$self->get_start_servers) {
        $self->set_start_servers(5);
    }
    if ($self->get_start_servers > $self->get_max_clients) {
        $self->get_start_servers($self->get_max_clients); 
    }
    
    if ($self->get_timeout) {
        $self->get_server->set_send_ack(int($self->get_timeout / 2))
            if !$self->get_server->get_send_ack;
        $self->set_server_check_ack($self->get_timeout * 1.5)
            if !$self->get_server_check_ack;
    }

    $self->get_server->start;

    $self->yield('forkoff');
    $kernel->delay_set(server_timeout_check => $self->get_server_check_ack)
        if $self->get_server_check_ack;
}

sub handler_child {
    my ($self, $what, $server_session) = @_[OBJECT, ARG0, ARG1];
    $self->SUPER::handler_child(@_[1 .. $#_]);
    return if $self->get_shutdown;

    if ($what eq 'lose') {
        my $server_object = $self->resolve_session($server_session);
        if ($server_object and $server_object == $self->get_server) {
            $self->yield('shutdown');
        }
    }
}

sub handler_run_error {
    my ($self, $kernel, $operation,
            $errnum, $errstr, $wheel_id) =
        @_[OBJECT, KERNEL, ARG0..ARG3];
    return unless $errnum;

    my $pid = $self->get_wheel_id_to_pid->{$wheel_id};
    warn "$pid got $operation error $errnum ($errstr)\n";
    if ($self->get_shutdown_on_error) {
        $self->yield('shutdown');
    }
}

sub handler_shutdown {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    $self->SUPER::handler_shutdown(@_[1 .. $#_]);


    $kernel->alarm_remove_all;
    if ($self->get_is_child) {
        DEBUG and warn "$$ Shutting down in child\n";
        $self->set_server(undef);
    }
    else {
        DEBUG and warn "$$ Shutting down\n";
        my $busy_children = $self->get_busy_servers;
        my $free_children = $self->get_free_servers;
        for (values %$free_children, values %$busy_children) {
            $_->{wheel}->kill('INT');
        }
    }
}

sub handler_server_closed {
    my ($self, $kernel, $id) = @_[OBJECT, KERNEL, ARG0];
    return if $self->get_is_child;

    my $pid = delete $self->get_wheel_id_to_pid->{$id};
    if (delete $self->get_free_servers->{$pid} or delete $self->get_busy_servers->{$pid}) {
        unless ($self->get_shutdown) {
            DEBUG and warn "$pid stopped\n";
            $self->yield('forkoff');
        }
    }
}

sub handler_server_stderr {
    my ($self, $input, $id) = @_[OBJECT, ARG0, ARG1];
    return if $self->get_shutdown;

    my $busy_children = $self->get_busy_servers;
    my $free_children = $self->get_free_servers;

    my $pid = $self->get_wheel_id_to_pid->{$id};
    my $child = $free_children->{$pid} || $busy_children->{$pid};

    $self->server_stderr($child, $input);
}

sub handler_server_ack {
    my ($self, $kernel, $input, $id) = @_[OBJECT, KERNEL, ARG0, ARG1];
    return if $self->get_shutdown;

    my $busy_children = $self->get_busy_servers;
    my $free_children = $self->get_free_servers;

    my $pid = $self->get_wheel_id_to_pid->{$id};
    my $child = $free_children->{$pid} || $busy_children->{$pid};
    if ($input eq COMM_ACK) {
        $self->server_ack($child);
    }
    elsif ($input eq COMM_BUSY) {
        $self->server_busy($child);
    }
    elsif ($input eq COMM_NOT_BUSY) {
        $self->server_not_busy($child);
    }
    else {
        $self->server_stdout($child, $input);
    }
}

sub handler_server_timeout_check {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    return if $self->get_shutdown;

    my $free_children = $self->get_free_servers;
    my $busy_children = $self->get_busy_servers;

    my $fork;
    for my $pid (keys %$free_children) {
        my $child = $free_children->{$pid};
        my $t = time - $child->{last_ack};
        if ($t > $self->get_timeout) {
            DEBUG and warn "Free child " , $child->{wheel}->ID, ":", $child->{wheel}->PID, " timed out";
            $self->server_timeout_free($child);
            $fork++;
        }
    }
    for my $pid (keys %$busy_children) {
        my $child = $busy_children->{$pid};
        my $t = time - $child->{last_ack};
        if ($t > $self->get_timeout) {
            DEBUG and warn "Busy child " , $child->{wheel}->ID, ":", $child->{wheel}->PID, " timed out";
            $self->server_timeout_busy($child);
            $fork++;
        }
    }
    $self->yield('forkoff') if $fork;
    $kernel->delay_set(server_timeout_check => $self->get_server_check_ack)
        if $self->get_server_check_ack;
}

sub handler_forkoff {
    my ($self, $kernel, $session) = @_[OBJECT, KERNEL, SESSION];
    return if $self->get_shutdown;

    my $free_children = $self->get_free_servers;
    my $busy_children = $self->get_busy_servers;

    my $tofork;
    my $idle = keys %$free_children;
    my $used = keys %$busy_children;
    my $total = $idle + $used;

    DEBUG and do {
        warn "Idle: $idle\n";
        warn "Used: $used\n";
        warn "Total: $total\n";
        warn "MaxSpare: ", $self->get_max_spare_servers, "\n";
        warn "MinSpare: ", $self->get_min_spare_servers, "\n";
    };
    if ($idle > $self->get_max_spare_servers) {
        my %copy = %$free_children;
        for ($self->get_max_spare_servers + 1 .. $idle) {
            my ($tokill, $pid);
            for (keys %copy) {
                if (!$tokill or $copy{$_}{starttime} < $tokill->{starttime}) {
                    $pid = $_;
                    $tokill = $copy{$_};
                }
            }
            if ($tokill) {
                delete $copy{$pid};
                DEBUG and warn "Killing child: ", $tokill->{wheel}->ID, ":", $tokill->{wheel}->PID, "\n";
                $tokill->{wheel}->kill('INT');
            }
        }
    }
    elsif ($total == 0) {
        for (1 .. $self->get_start_servers) {
            last if $total >= $self->get_max_clients;
            $total++;
            $self->yield('server_fork');
        }
    }
    elsif ($idle < $self->get_min_spare_servers) {
        for ($idle + 1 .. $self->get_min_spare_servers) {
            last if $total >= $self->get_max_clients;
            $total++;
            $self->yield('server_fork');
        }
    }
}

sub handler_server_fork {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    return if $self->get_shutdown;

    my $children_map  = $self->get_wheel_id_to_pid;
    my $free_children = $self->get_free_servers;

    my $wheel = POE::Wheel::Run->new(
        Program => sub {
            $self->server_start;
            DEBUG and warn "Program started\n";
            $kernel->run;
        },
        CloseEvent  => "server_closed",
        StdoutEvent => "server_ack",
        StderrEvent => "server_stderr",
        StdioFilter => POE::Filter::Line->new,
        ErrorEvent  => "run_error",
    );
    $free_children->{$wheel->PID} = {
        wheel     => $wheel,
        last_ack  => time,
        starttime => time,
    };
    DEBUG and printf "Started child %d:%d\n", $wheel->ID, $wheel->PID;
    $children_map->{$wheel->ID} = $wheel->PID;
}

# Instance methods

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    _init_internal_data($self);
    return $self;
}

sub DESTROY {
    my $self = shift;
    $self->SUPER::DESTROY;
    _destroy_internal_data($self);
}

sub create_states {
    my $self = shift;
    croak "Unknown arguments to create_states()" if @_;
    $self->SUPER::create_states;

    for (qw/
        run_error server_closed server_stderr server_ack
        server_timeout_check server_fork forkoff
    /
    )
    {
        $poe_kernel->state($_ => $self, "handler_$_");
    }
}

# Yes, this is strange but some people may want to do something
# different depending on if the child is busy or not.
sub server_timeout {
    my ($self, $child) = @_;
    $child->{wheel}->kill('INT');
}

sub server_timeout_busy {
    my ($self, $child) = @_;

    my $busy_children = $self->get_busy_servers;
    my $children_map  = $self->get_wheel_id_to_pid;

    DEBUG and warn "Busy child timed out: ", $child->{wheel}->ID, ":", $child->{wheel}->PID, "\n";

    $self->server_timeout($child);
    delete $busy_children->{$child->{wheel}->PID};
    delete $children_map->{$child->{wheel}->ID};
}

sub server_timeout_free {
    my ($self, $child) = @_;

    DEBUG and warn "Free child timed out: ", $child->{wheel}->ID, ":", $child->{wheel}->PID, "\n";

    my $free_children = $self->get_free_servers;
    my $children_map  = $self->get_wheel_id_to_pid;
    $self->server_timeout($child);
    delete $free_children->{$child->{wheel}->PID};
    delete $children_map->{$child->{wheel}->ID};
}

# After the fork
sub server_start {
    my $self = shift;

    croak "Too many arguments to server_start" if @_;

    # So this session will shutdown
    $poe_kernel->detach_child($self->get_server->get_session)
        or die "Could not detach child: $!";

    # I do not know why I need this but the wheel does not
    # seem to get destroyed fast enough
    my $busy_children = $self->get_busy_servers;
    my $free_children = $self->get_free_servers;
    for (values %$busy_children, values %$free_children) {
        $_->{wheel}->pause_stdout;
        $_->{wheel}->pause_stderr;
    }

    $self->set_is_child(1);
    $self->get_server->post('resume_accept');
    $self->call('shutdown');
}

sub server_ack {
    my $self = shift;

    my $child = shift;
    croak "No child specified to server_ack"
        unless defined $child;

    DEBUG and warn "Ack: ", $child->{wheel}->ID, ":", $child->{wheel}->PID, "\n";

    croak "Too many arguments to server_ack" if @_;

    $child->{last_ack} = time;
}

sub server_not_busy {
    my $self = shift;

    my $child = shift;
    croak "No child specified to server_not_busy"
        unless defined $child;

    croak "Too many arguments to server_not_busy" if @_;

    DEBUG and warn "not busy: ", $child->{wheel}->ID, ":", $child->{wheel}->PID, "\n";

    my $free_children = $self->get_free_servers;
    my $busy_children = $self->get_busy_servers;

    my $pid = $child->{wheel}->PID;
    unless (exists $free_children->{$pid}) {
        $free_children->{$pid} = delete $busy_children->{$pid};
        $self->yield('forkoff');
    }
}

sub server_busy {
    my $self = shift;

    my $child = shift;
    croak "No child specified to server_busy"
        unless defined $child;

    croak "Too many arguments to server_busy" if @_;

    DEBUG and warn "busy: ", $child->{wheel}->ID, ":", $child->{wheel}->PID, "\n";

    my $free_children = $self->get_free_servers;
    my $busy_children = $self->get_busy_servers;

    my $pid = $child->{wheel}->PID;
    unless (exists $busy_children->{$pid}) {
        $busy_children->{$pid} = delete $free_children->{$pid};
        $self->yield('forkoff');
    }
}

sub server_stdout {
    my ($self, $child, $output) = @_;
    croak "No child specified to server_stdout"
        unless defined $child;
    my $pid = $child->{wheel}->PID;
    print "$pid STDOUT: $output\n";
}

sub server_stderr {
    my ($self, $child, $output) = @_;
    croak "No child specified to server_stderr"
        unless defined $child;
    my $pid = $child->{wheel}->PID;
    print "$pid STDERR: $output\n";
}

1;

__END__

=head1 DESCRIPTION

POE::Class::Server::PreFork - Class for preforking and monitoring server
sessions.

=head1 SYNOPSIS

    use POE qw(Class::Server::PreFork);

    my $server = new POE::Class::Server::PreFork::Something;

    my $preforked = new POE::Class::Server::PreFork(
        server            => $server,
        # optional and default
        shutdown_on_error => 1,
        min_spare_servers => 1,
        max_spare_servers => 2,
        start_servers     => 5,
        max_clients       => 50,
        timeout           => 0,
    );
    printf "Created prefork server with ID %d\n", $preforker->ID;

    # Create the session
    my $session = $preforked->start;
    printf "Created prefork session with ID %d\n", $session->ID;

    # don't forget this
    $poe_kernel->run;

=head1 DESCRIPTION

POE::Class::Server::PreFork is more or less a monitor. It tracks forked
sessions, reforkes them when they timeout and preforks them to the given
specifications. POE::Class::Server::PreFork isa L<POE::Class>.


=head1 ACCESSOR METHODS

All accessors have three methods ala L<POE::Class::Attribs>. set_ATTRIB,
get_ATTRIB and ATTRIB. Set and get are obvious. ATTRIB is simply a set/get
method. See L<POE::Class::Attribs> for more details.

=over

=item server

This is the object used for the server session.  POE::Class::Server::PreFork
comes with two such classes.  L<POE::Class::Server::PreFork::TCP> and
L<POE::Class::Server::PreFork::UNIX>.

=item shutdown_on_error

shutdown_on_error says whether the server should yield shutdown if an error occurs
with the child server. See L<POE::Wheel::Run> for possible errors.

=item min_spare_servers

=item max_spare_servers

These are the maximum and minimum spare (non-busy) servers PreFork should keep
around.

=item start_servers

The number of servers PreFork should initially start with. This will be set to
max_clients if it is greater than max_clients.

=item max_clients

This is the maximum number of child servers that will be launched.

=item timeout

This is the approximate time in seconds a child is allowed to block.

=back

=head1 INTERNALS

This section is for people who wish to subclass POE::Class::Server::PreFork. If you are
not such a person you may stop reading now.


=head2 The Server Object

The C<server> object's session is started from within this class so that it
becomes a child of this class. It is started with the method C<start()> which
is probably inherited from L<POE::Class>. When it is started it is expected to
setup a socket of some kind. It should B<NOT> start accepting connections yet,
that comes later.

When PreFork initially starts it calles the child server's method send_ack with
the proper interval PreFork needs ACKs for set_timeouts. After that, PreFork
calles the child server's C<start()> method.

An ACK is the constant COMM_ACK, defined in this class, printed to STDOUT by the child
server.

PreFork forks child servers using L<POE::Wheel::Run>. When PreFork forks it
detaches the child session so that the session used by PreFork can exit. The
child server's session object is accessed with the C<get_session()> method (also
inherited from L<POE::Class>). Then PreFork posts a C<resume_accept> event to
the child server's session. This is the event that tells the child server
session to start accepting connections now.

In addition to ACKs the server child is responsible for letting us know when it
is busy and when it is free. It does this by printing the COMM_BUSY or COMM_NOT_BUSY
constants, defined in this package, to STDOUT.

This constants COMM_BUSY, COMM_NOT_BUSY and COMM_ACK can be exported from this
class

    use POE::Class::Server::PreFork qw(COMM_BUSY COMM_NOT_BUSY COMM_ACK);
    # -or-
    use POE::Class::Server::PreFork qw(:all);

=head2 Server Tracking

Child Servers are tracked using three hash references which can be accessed by
methods:

=over

=item busy_servers

=item free_servers

These methods store a hash that maps the PID of the child process to a hash
containing

=over

=item wheel

The L<POE::Wheel::Run> object used to fork off the child. See
L<POE::Wheel::Run> for details on methods.

=item last_ack

The last time PreFork recieved an ACK. This is used to timeout the process. It
is unix time returned by time().

=item starttime 

This stores the time() this process started. PreFork uses this time to decide
which process to kill off when it is going back down to C<max_spare_servers>.
PreFork kill the oldest non-busy server.

=back

Both of these combined are all the child servers.

=item wheel_id_to_pid

This hash simply maps L<POE::Wheel::Run> ID to PID. It is needed in places
PreFork only has the Wheel ID.

=back

=head2 Handlers

These are the handlers this session defines. Handlers are all object states
defined in the method C<create_states()> which is called from the base class
L<POE::Class>.

The following show the name of the state on the left and the name of the method
on the right.

=over

=item _start => handler_start

This handler is called when the PreFork session starts. It does some
normalization on the attributes that are not set and or set to things that do
not make sense.  PreFork then creates the server child session and yields to
forkoff.

=item _child => handler_child

Used to know if the server child session has ended for some reason (possibly an
error).  If it has ended PreFork yields shutdown.

=item _stop => handler_stop

Used to cleanup internal data structures.

=item run_error => handler_run_error

C<handler_run_error()> is called by L<POE::Wheel::Run> when an error occurs
with the processes PeFork forked. See the StderrEvent in L<POE::Wheel::Run>
for details on arguments.

This handler warns the error and yields shutdown if C<shutdown_on_error()> is
true.

=item shutdown => handler_shutdown

Puts us in the shutdown state and removes all alarms.

In the forked child deletes all internal references to wheels so the PreFork
session ends. In the parent it sends a SIGINT to all of the children.

=item server_closed => handler_server_closed

Called by L<POE::Wheel::Run> when a child closes all it's output channels
(PreFork assumes this means it exited). See the CloseEvent in
L<POE::Wheel::Run> for details on arguments. 

PreFork uses this handler to remove tracked L<POE::Wheel::Run> wheels and to
yield forkoff unless it is in a C<Shutdown()> state.

=item server_stderr => handler_server_stderr

Called from L<POE::Wheel::Run> when a child produces STDERR. See
L<POE::Wheel::Run> for details on this handler's arguments. This handler looks
up the child hash and calles the C<server_stderr()> method with the child's
hash reference as the first argument and the input as the second.

=item server_ack => handler_server_ack

StdoutEvent from L<POE::Wheel::Run>. If the output is the constant COMM_ACK
calles the method C<server_ack()>, if the output is the constant COMM_BUSY
calles the method C<server_busy()>, if the output is the constant COMM_NOT_BUSY
calles the method C<server_not_busy()> else calles the method
C<server_stdout()>.

=item server_timeout_check => handler_server_timeout_check

Called on a timer of C<get_timeout()> * 1.5. Checks all children to see that
they have sent an ACK within C<get_timeout()> seconds. If a free server is
timeing out calles the method C<server_timeout_free()>, if a busy server is
timeing out calles C<server_timeout_busy()>. If any servers have timed out
yield forkoff.

=item forkoff => handler_forkoff

This handler is the main code for handling child server monitoring. It kills
off the oldest processes if PreFork is over C<get_max_spare_servers()>, starts
the initial C<get_start_servers()> if total servers is zero, or yields
server_fork for every child server PreFork is under C<get_min_spare_servers()>.

=item server_fork => handler_server_fork

Uses L<POE::Wheel::Run> to fork of a child process. Calles the method C<server_start()>
from within the child process. Stores the child hash in C<free_servers()> and the wheel
ID to PID in C<wheel_id_to_pid()>.

=back

=head2 Methods

Many of these methods were created to provide a simpler subclass interface.

=over

=item create_states

Creates the following states

    run_error            => handler_run_error
    server_closed        => handler_server_closed
    server_stderr        => handler_server_stderr
    server_ack           => handler_server_ack
    server_timeout_check => handler_server_timeout_check
    server_fork          => handler_server_fork
    forkoff              => handler_forkoff

See L</"Handlers"> for details.

=item server_timeout

This method is called when a server has timed out. It sends a SIGINT
to the process.

=item server_timeout_busy

=item server_timeout_free


Server objects send information back to the monitor via STDOUT. This
information lets the monitor know if the server is busy or not.
C<server_timeout_busy()> or C<server_timeout_free()> are called when a free or
busy server times out respectivly. The only argument to these methods is the
child's hash.

Both of these methods do the same thing by default (with slightly different
debug information in debug mode). They call C<server_timeout()> and removes the
references to the childs wheel.

=item server_start

Called from within the forked process. Does the following:

Detaches the child session.

Pauses all wheels to keep them from getting input before they are destroyed.

Posts a resume_accept event to the server child session, set.

Calles the shutdown state.

=item server_ack

Takes the child hash reference as the first argument. Sets the last_ack
attribute in the child hash to the current time.

=item server_not_busy

Takes the child hash reference as the first arugment. Moves the reference for
this child from the C<busy_servers()> hash to the C<free_servers()> hash.

=item server_busy

Takes the child hash reference as the first arugment. Moves the reference for
this child from the C<free_servers()> hash to the C<busy_servers()> hash.

=item server_stdout

Called when the child server produces stdout that is not recognized. Takes the
child hash reference as the first argument and the output as the second. Prints
the output to STDOUT prefixed with C<$pid STDOUT: >.

=item server_stderr

Called when the child server produces stderr that is not recognized. Takes the
child hash reference as the first argument and the output as the second. Prints
the output to STDERR prefixed with C<$pid STDERR: >.

=item busy_servers

Returns the hash reference of busy server childs.

=item free_servers

Returns the hash reference of non-busy server childs.

=item wheel_id_to_pid

Returns the hash reference that maps server childs wheel ID to PID.

=item server_check_ack

The interval we check for timed out children.

=item is_child

Used to know if we are the forked child. This is set in C<server_start()>.

=back

=head1 BUGS

The documentation is not complete.

Tests are far from complete.

=head1 AUTHOR

Scott Beck E<lt>sbeck@gossamer-threads.comE<gt>

=head1 SEE ALSO

L<POE>

L<POE::Class>

L<POE::Class::Server::PreFork::TCP>

L<POE::Class::Server::PreFork::UNIX>

=cut

