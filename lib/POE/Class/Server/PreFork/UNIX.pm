package POE::Class::Server::PreFork::UNIX;

use strict;
use vars qw/@ISA/;

use Carp;
use POE qw/
    Class::Server::UNIX
/;
use POE::Class::Server::PreFork qw(:all);

@ISA = qw(POE::Class::Server::UNIX);

use POE::Class::Attribs
    alarm_id         => undef,
    max_requests     => 25,
    requests_handled => 0,
    send_ack         => 0;

sub handler_resume_accept {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    return if $self->get_shutdown;

    print COMM_ACK, "\n";
    $self->get_wheel->resume_accept;
    $self->set_alarm_id(
        $kernel->delay_set(send_ack => $self->get_send_ack)
    ) if $self->get_send_ack;
}

sub handler_send_ack {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    return if $self->get_shutdown;

    print COMM_ACK, "\n";
    $self->set_alarm_id(
        $kernel->delay_set(send_ack => $self->get_send_ack)
    ) if $self->get_send_ack;
}

sub handler_connection {
    my $self = $_[OBJECT];
    return if $self->get_shutdown;

    $self->SUPER::handler_connection(@_[1 .. $#_]);

    $self->get_wheel->pause_accept;
    print COMM_BUSY, "\n";

    if ($self->get_max_requests) {
        my $handled = $self->get_requests_handled;
        $self->set_requests_handled(++$handled);
        if ($handled >= $self->get_max_requests) {
            $self->set_shutdown(1);
        }
    }
}

sub handler_child {
    my ($self, $what) = @_[OBJECT, ARG0];
    $self->SUPER::handler_child(@_[1 .. $#_]);

    if ($what eq 'lose') {
        if ($self->get_shutdown) {
            $self->set_wheel(undef);
            print COMM_BUSY, "\n";
        }
        else {
            print COMM_NOT_BUSY, "\n";
            $self->get_wheel->resume_accept;
        }
    }
}

sub signal_int {
    my ($self, $kernel) = @_[OBJECT, KERNEL];
    $self->yield('shutdown');
    $kernel->sig_handled;
}

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
    croak "Unknown arguments passed to create_states()" if @_;
    $self->SUPER::create_states;

    for (qw/resume_accept send_ack/) {
        $poe_kernel->state($_ => $self, "handler_$_");
    }
    $poe_kernel->sig(INT => 'int');
}

sub connect {
    my $self = shift;
    $self->SUPER::connect;
    $self->get_wheel->pause_accept;
    $poe_kernel->alarm_remove($self->get_alarm_id)
        if defined $self->get_alarm_id;
}

1;

__END__

=head1 DESCRIPTION

POE::Class::Server::PreFork::UNIX - Subclass of POE::Class::Server::UNIX which
handles talking back to POE::Class::Server::PreFork.

=head1 SYNOPSIS

    package My::Conn;

    use POE qw(Class::Conn::UNIXStream);

    @My::Server::ISA = qw(POE::Class::Conn::UNIXStream);

    sub handler_input {
        my ($self, $input) = @_[OBJECT, ARG0];
        return if $self->get_shutdown;
        print "<< $input\n";
        $self->get_wheel->put($input);
    }

    package main;

    use POE qw(
        Class::Server::PreFork
        Class::Server::PreFork::UNIX
    );

    my $server = new POE::Class::Server::PreFork::UNIX(
        # Option added by this subclass
        max_requests => 10,

        # Same options as POE::Class::Server::UNIX
        path         => '/tmp/foo.sock'
        conn_class   => 'My::Conn',
    );

    my $preforked = new POE::Class::Server::PreFork(
        server => $server,
    );

    # Create the session
    $preforked->start;

    # don't forget this
    $poe_kernel->run;

=head1 DESCRIPTION

POE::Class::Server::PreFork::UNIX is a subclass of L<POE::Class::Server::UNIX>.
It implements what is needed to play nice with L<POE::Class::Server::PreFork>,
i.e. pause accept when the connection starts, tell
L<POE::Class::Server::PreFork> when we are busy and not busy.

You should see L<POE::Class::Server::UNIX> for details on the inherited
interface.

=head1 ACCESSOR METHODS

All accessors have three methods ala L<POE::Class::Attribs>. set_ATTRIB,
get_ATTRIB and ATTRIB. Set and get are obvious. ATTRIB is simply a set/get
method. See L<POE::Class::Attribs> for more details.

=over

=item max_requests

This is the only really public method you should be calling. It sets the total
number of requests this process will handle before shutting down.

=back

=head1 INTERNALS

If you have no plans on subclassing this module, stop reading now.

=head2 ACCESSOR METHODS

=over

=item alarm_id

This stores the alarm ID which is set in C<handler_resume_accept()>. This alarm
is used to print COMM_ACK to STDOUT. The alarm is set to C<send_ack()> seconds.

=item requests_handled

Keeps track of the number of requests we have handled. It is incremented in the
overriden method C<handler_connection()>.

=item send_ack

This is set by L<POE::Class::Server::PreFork> to how often we need to tell our
parent we are still alive. It is used to set the alarm time in
C<handler_resume_accept()> and C<handler_send_ack()>.

=back

=head2 HANDLERS

=over

=item resume_accept => handler_resume_accept

L<POE::Class::Server::PreFork> posts this state once it has forked. In this
handler PreFork::UNIX resumes acception on this wheel and sets the alarm used to
tell L<POE::Class::Server::PreFork> we are still alive. It also prints an
initial COMM_ACK.

=item send_ack => handler_send_ack

This handler simply prints COMM_ACK followed by a new line to STDOUT and sets
another alarm of C<send_ack()> seconds to post a send_ack event.

=item connection => handler_connection

Overrides the connection handler in L<POE::Class::Server::UNIX>, returns if the
C<shutdown()> flag is set and calles C<SUPER::handler_connection()>.

PreForked servers only accept one connection at a time, so in this handler we
pause the wheel handling accepts. It is unpaused once this connection has
finished.

This handler also checks for C<max_requests()>. If PreFork::UNIX reaches
C<max_requests()> the C<shutdown()> flag is set.

=item _child => handler_child

This handler is used to find out when the connection child session has finished.
If the C<shutdown()> flag is set this handler sets C<wheel()> to undef and prints
COMM_BUSY. This will cause this process to exit shortly. Else COMM_NOT_BUSY is
printed and C<resume_accept()> is called on the accept C<wheel()>.

=back

=head2 SIGNALS

=over

=item INT => int => signal_int

In the C<create_states()> method PreFork::UNIX creates a state called int with a
handler C<signal_int()>. L<POE::Class::Server::PreFork> sends a SIGINT to tell
us it is time to shutdown. This int handler simply yields the shutdown state.

=back

=head2 METHODS

=over

=item new

Calles C<SUPER::new()> and sets up internal instance storage ala
L<POE::Class::Attribs>.

=item DESTROY

Calles C<SUPER::DESTROY> and cleans up internal instance data.

=item create_states

Used to setup three states. resume_accept, send_ack and signal_int. Also sets
up the SIGINT handler.

=item connect

Overrides the C<connect()> method in L<POE::Class::Server::UNIX>. Calles
C<SUPER::connect> and calles C<pause_accept()> on the accept wheel. This is
needed because L<POE::Class::Server::PreFork> starts this session before it
forks. C<resume_accept> is posted within each child by
L<POE::Class::Server::PreFork> to resume accepts.

=back

=head1 TODO

Write better documentation.

=head1 AUTHOR

Scott Beck E<lt>sbeck@gossamer-threads.comE<gt>

=head1 SEE ALSO

L<POE>

L<POE::Class>

L<POE::Class::Server::UNIX>

L<POE::Class::Server::PreFork>

=cut


