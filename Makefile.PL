#!/usr/bin/perl

use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME         => 'POE::Class::Server::PreFork',
    VERSION_FROM => 'lib/POE/Class/Server/PreFork.pm',
    PMLIBDIRS    => [ qw(lib) ],
    PREREQ_PM    => {
        POE => '0.27'
    },
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (AUTHOR     => 'Scott Beck <sbeck@gossamer-threads.com>') : ()),
);


