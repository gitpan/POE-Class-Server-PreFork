#!/usr/bin/perl

use strict;

use Test::More qw(no_plan);

BEGIN { use_ok('POE::Class::Server::PreFork::UNIX') }
require_ok('POE::Class::Server::PreFork::UNIX');

