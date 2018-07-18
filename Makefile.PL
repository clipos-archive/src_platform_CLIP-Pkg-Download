# SPDX-License-Identifier: LGPL-2.1-or-later
# Copyright © 2008-2018 ANSSI. All Rights Reserved.
use 5.008008;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'CLIP::Pkg::Download',
    VERSION_FROM      => 'lib/CLIP/Pkg/Download.pm', # finds $VERSION
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/CLIP/Pkg/Download.pm', # retrieve abstract from module
       AUTHOR         => 'A. U. Thor <vincent@(none)>') : ()),
);
