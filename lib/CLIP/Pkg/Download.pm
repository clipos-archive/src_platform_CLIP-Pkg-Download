# SPDX-License-Identifier: LGPL-2.1-or-later
# Copyright © 2008-2018 ANSSI. All Rights Reserved.
package CLIP::Pkg::Download;

use 5.008008;
use strict;
use warnings;
use File::Basename;
use File::Path;
use File::Copy;
use CLIP::Pkg::Base ':all';
use CLIP::Logger ':all';
use Sort::Versions;
use Compress::Zlib;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
clippkg_list_cache_upgrade
clippkg_download
clippkg_download_all
clippkg_gen_packages_gz
clippkg_gen_mirror
clippkg_update_mirror
clippkg_download_error_recovery
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '1.2.6';


=head1 NAME

CLIP::Pkg::Download - Perl extension to manage downloads of CLIP packages

=head1 VERSION

Version 1.2.6

=head1 SYNOPSIS

  use CLIP::Pkg::Download;
  use CLIP::Pkg::Download ':all';

=head1 DESCRIPTION

CLIP::Pkg::Download provides a set of helper functions for local package mirror
management under CLIP. This includes both the synchronisation of local mirrors 
against remote ones - through a download-specific cache used for signature and
package field checks - and the synchronisation of local mirrors one against 
another, e.g. when upgrading an alternate CLIP install.

CLIP::Pkg::Download uses the basic helpers from CLIP::Pkg::Install for, among 
others, package parsing and checking, and the CLIP::Logger module to log its
outputs.

=head1 EXPORT

No functions or variables are exported by default. The module defines a single
Exporter tag, ':all', which exports the following functions:

=over 4

=item * 

B<clippkg_list_cache_upgrade()>

=item *

B<clippkg_download()>

=item *

B<clippkg_download_all()>

=item * 

B<clippkg_gen_packages_gz()>

=item * 

B<clippkg_gen_mirror()>

=item * 

B<clippkg_update_mirror()>

=item * 

B<clippkg_download_error_recovery()>

=back

=cut

###############################################################
#                          VARIABLES                          #
###############################################################

=head1 VARIABLES

CLIP::Pkg::Download can be configured through the following variables :

=over 4

=item B<CLIP::Pkg::Download::$g_apt_cache_dir>

Full path to the current download cache. This should be the parent 
directory of the C<archives> directory where new packages will be
downloaded, e.g. C</var/cache/apt>.

=cut
our $g_apt_cache_dir;

=item B<CLIP::Pkg::Download::$g_dl_retries>

Number of retries when downloading a package fails (e.g. because the signature
of the downloaded package cannot be verified). Default: 2.

=cut

our $g_dl_retries = 2;

=item B<CLIP::Pkg::Download::$g_sourcelist_file>

Sourcelist file used by APT to make URL.

=cut

our $g_sourcelist_file;

=item B<CLIP::Pkg::Download::$g_sourcelist_admin_dir>

Directory where to get sourcefile templates.

=cut

our $g_sourcelist_admin_dir;

=item B<CLIP::Pkg::Download::$g_sourcelist_var_eth0>

Replace "@ETH0" in sourcefile templates.

=cut

our $g_sourcelist_var_eth0 = "00:00:00:00:00:00";

=item B<CLIP::Pkg::Download::$g_sourcelist_var_hostname>

Replace "@HOSTNAME" in sourcefile templates.

=cut

our $g_sourcelist_var_hostname = "nobody";

=back

In addition to those module-specific variables, CLIP::Pkg::Download is also 
indirectly affected by the configuration variables supported by the 
CLIP::Pkg::Base and CLIP::Logger modules, which it includes.

=cut

# APT options shortcut
my $g_apt_opts = $CLIP::Pkg::Base::g_apt_opts;

###############################################################
#                          SUBS                               #
###############################################################

=head1 FUNCTIONS

=head2 Download

=over 4

=cut

		       #######################
		       #      Download       #
		       #######################

=item B<CLIP::Pkg::Download::parse_upgrades($output)>

Internal use only : parse an apt-get <command> --print-uris output 
(passed as a list referenced by $output) to list upgrade candidates 
(full package names).

=cut

sub parse_upgrades($) {
	my $output = shift;

	my @tbi = grep { /https:\/\// or /copy:\// } @{$output};

	# Possible outputs:
	# '<URL>' <pkg> (normal)
	#  ?] '<URL>' <pkg> (first URI line when prompted to type 
	#     "Yes, do as I say")
	# ^ note whitespace...
	foreach my $p (@tbi) {
		chomp $p;
		$p =~ s/^(?: \?\] )?\S+\s+(\S+)\s+.*/$1/;
	}
	return \@tbi;
}

=item B<clippkg_list_cache_upgrade($pkg)>

List the names (full names) of all packages needed to upgrade package $pkg
(which can be either a selected optional package or a configuration), and 
which are not yet present in cache.
Returns a reference to a list of upgrade candidates (full package names), 
or C<undef> in case of error.

=cut

sub clippkg_list_cache_upgrade($) {
	my $pkg = shift;

	open PIPE, "echo \"Yes, do as I say!\" | "
		."apt-get install \'$pkg\' $g_apt_opts --print-uris 2>&1 |";
	my @output = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "failed to list cache upgrade URIs for $pkg";
		foreach (@output) {
			clip_warn "apt output: $_";
		}
		return undef;
	}

	return parse_upgrades(\@output);
}

=item I<CLIP::Pkg::Download::_download_nodeps($pname, $remote_only)>

Internal use only. Downloads the latest version of a package named $pname into 
the local cache, without dependency checks, then checks the package's 
signature and, if it is a configuration, release date.
If $remote_only is non-null, the download will be performed only from
those sources in sources.list that are not tagged 'local'. Concretely, this
means, in the case of a package download for a given RM jail, that the other
RM jail's mirror will be ignored.
Returns 1 on success, 0 on error (no retries). In case of error, any 
downloaded files are automatically removed from the local cache.

=cut

sub _download_nodeps($$) {
	my ($pname, $remote) = @_;

	clip_log "Downloading $pname";
	my $ppath = "$g_apt_cache_dir/archives/$pname";

	my $pfull = $pname;
	$pname =~ s/_\S+.deb$//;

	my $apt_opts = "";
	$apt_opts = "-r" if ($remote); # Use only remote sources

	if (! -f $ppath) {
		open PIPE, "apt-get -o apt::get:fix-policy-broken=true "
			."install \'$pname\' $g_apt_opts $apt_opts -d 2>&1 |";
		my @output = <PIPE>;
		close PIPE;

		if ($?) {
			clip_warn "error downloading $pname "
				."without dependencies";
			foreach my $line (@output) {
				clip_warn "apt output: $line";
			}
			return 0
		}
	}

	unless (clippkg_check_sig($ppath)) {
		unlink $ppath;
		return 0;
	}

	if (clippkg_is_conf($pname)) {
		# Configuration : check release date
		my $cinfo;
		unless (defined ($cinfo = 
				clippkg_get_fields($ppath, "Release-Date"))) {
			clip_warn "failed to retrieve Release-Date for $pfull";
			unlink $ppath;
			return 0;
		}

		unless (clippkg_check_release_date($ppath, $cinfo)) {
			clip_warn "removing $pfull : wrong release date";
			unlink $ppath;
			return 0;
		}
	}

	clip_log "Package $pfull downloaded successfully";
	
	return 1;
}

=item B<clippkg_download($pkg, $hash)>

Tries at most $g_dl_retries times to download package $pkg into the local 
cache and check its signatures (and release date if it is a configuration).
Returns 1 if package was successfully downloaded and checked, 0 if all 
$g_dl_retries tries to download and check it failed.

=cut

sub clippkg_download($$) {
	my ($pfull, $hash) = @_;

	my $tries = 0;

	if (defined($hash) and not clippkg_check_optional($pfull, $hash)) {
			clip_warn "$pfull is not an allowed optional package";
			return 0;
	}

	# First download attempt is performed with local mirrors included 
	# (i.e. the other RM jail in the case of a RM download). The following 
	# attempts exclude local mirrors, to force downloading from a remote 
	# (supposedly more up-to-date) mirror.
	while ($tries < $g_dl_retries) {
		if (_download_nodeps($pfull, $tries)) {
			return 1;
		} else {
			clip_warn "try ".($tries+1)
				."/$g_dl_retries at downloading $pfull failed";
			if (-f "$g_apt_cache_dir/archives/partial/$pfull" ) {
				unlink "$g_apt_cache_dir/archives/"
					."partial/$pfull" 
					or clip_warn "Failed to remove "
						."partial $pfull";
			}
			$tries = $tries + 1;
		}
	}

	clip_warn "all tries to download $pfull failed";
	return 0;
}

=item I<CLIP::Pkg::Download::write_pending($file, $list)>

Internal use only. This writes out, one per line, all entries in the list
referenced by $list, into the file named $file, which is created as needed.
This is typically used to write a temporary list of packages being downloaded,
which can than be read by the error recovery functions on the next run, if 
the first one was interrupted.
Returns 1 on success, 0 on error.

=cut

sub write_pending($$) {
	my ($outfile, $lref) = @_;

	unless (open OUT, ">>", "$outfile") {
		clip_warn "failed to open $outfile for writing";
		return 0;
	}

	# N.B. : do not miss newline at the end of the pending
	# file.
	# Otherwise two sucessive writes to the file will 
	# bork the last line from the first write and first
	# one from the second, and those two packages
	# will fail to be recovered...
	foreach my $pkg (@{$lref}) {
		print OUT "$pkg\n" if ($pkg);
	}
	close OUT;
	return 1;
}

=item B<clippkg_download_with_deps($pname, $plist, $clist, $hash)>

Download a package $pname's (short name) latest version in the local cache, 
along with all its dependencies that are not in the cache yet. Check all 
signatures (and release dates on configurations). Packages can optionally be 
checked against an allowed optional package hash passed as $hash. If $hash is 
passed as C<undef>, no such check is performed (as expected when downloading a 
configuration).

When $plist is defined, it is assumed to be a reference to a list, into which 
are pushed the full names of all new packages that were downloaded by this call.

When $clist is defined, it is assumed to be a reference to a list. When a new 
version of $pname is downloaded, the full name for that version is pushed into 
that list.

In case of error ($pname or any one of its dependencies cannot be downloaded, 
or fail to pass all checks), all newly downloaded packages are removed before 
returning.

Returns 1 on success, 0 on failure.

=cut

sub clippkg_download_with_deps($$$$) {
	my ($pname, $plist, $clist, $hash) = @_;

	my $deplist;

	return 0 
		unless (defined($deplist = clippkg_list_cache_upgrade($pname)));
	
	if ($#{$deplist} == -1) {
		clip_log "No upgrade to download for $pname";
		return 1;
	}

	# Write/append a list of packages not fully downloaded / checked, 
	# to be removed on next run if this run gets interrupted 
	# before completion.
	unless (write_pending ("$g_apt_cache_dir/pending", $deplist)) {
		clip_warn "failed to write list of packages to be downloaded";
		return 0;
	}

	my $pfull = ""; # Full package name.
	foreach my $pkg (@{$deplist}) {
		unless (clippkg_download($pkg, $hash)) {
			clip_warn "failed to download all "
				."dependencies of $pname";
			# Remove all downloaded dependencies
			foreach my $to_rm (@{$deplist}) {
				# XXX What do we do if this fails ? 
				unlink "$g_apt_cache_dir/archives/$to_rm"
					if (-f "$g_apt_cache_dir/archives"
						."/$to_rm" );
			}
			return 0;
		} else {
			if (defined($clist) and $pkg =~ /^$pname/) {
				$pfull = $pkg;
			}
		}
	}

	clip_log "all dependencies of $pname downloaded successfully";
	if (defined($plist)) {
		push @{$plist}, @{$deplist};
	}
	if (defined($clist) and $pfull) {
		push @{$clist}, ($pfull);
	}

	# Note: pending file not removed on purpose, will be removed only 
	# after migration of packages into the mirror.
	return 1;
}

=item B<clippkg_download_all($dist, $conflist, $plist, $mirpath)

Download all new packages for distribution $dist, including both new confs and
new optional packages. 

This is is done as follows :

=over 8

=item 1.

Upgrade candidates (configuration and optional package names for the current 
dist) are listed, and sorted into configurations on the one hand and optional 
packages on the other hand.

=item 2.

Upgrades are downloaded for all configuration candidates, and a list of full 
names for those configurations that have been downloaded is built.

=item 3.

That list is used, along with configurations currently available in the
mirror at $mirpath, to build a hash of allowed optional packages.

=item 4.

Upgrades are attempted for all optional packages, using that hash to perform 
optional checks on them.

=back

New configuration full names are appended to the list referenced by $conflist 
if it is defined. New packages full names are similarly appended to the list 
referenced by $plist, if it is defined.

Returns 1 on success, 0 on error.

=cut

sub clippkg_download_all($$$$) {
	my ($dist, $conflist, $plist, $mirpath) = @_;

	my $candidates;

	if (not defined ($candidates = clippkg_list_upgrade_candidates(0))) {
		clip_warn "could not get list of upgrade candidates";
		return 0;
	}

	clip_log "upgrade candidates: $candidates"; 

	my @tbi_confs = (); # New configurations
	my @tbi_pkgs = (); # New optional packages
	
	foreach my $pkg (split " ", $candidates) {
		if (clippkg_is_conf($pkg)) {
			push @tbi_confs, ($pkg);
		} else {
			push @tbi_pkgs, ($pkg);
		}
	}

	foreach my $part (<$g_apt_cache_dir/archives/partial/*>) {
		clip_warn "Removing partial ".(basename $part);
		unlink "$part" or clip_warn "Failed to remove $part";
	}


	# First download all configurations, to build a list of fresh 
	# configurations

	my @new_confs = (); # Freshly downloaded configurations
	foreach my $cname (@tbi_confs) {
		# No error check : one configuration failing to download does
		# not block other configurations.
		clippkg_download_with_deps($cname, $plist, \@new_confs, undef);
	}
	push @{$conflist}, @new_confs;

	# We're done if there are no optional packages
	return 1 unless (@tbi_pkgs);

	# Then use that list of fresh configurations to build the current 
	# allowed optional hash.

	my $hash; 
	unless (defined($hash = clippkg_list_mirror_optional(\@new_confs, 
					$mirpath, "$g_apt_cache_dir/archives"))) {
		clip_warn "could not get allowed optional packages for $dist";
		return 0;
	}

	# Then finally download optional packages, using the 
	foreach my $pname (@tbi_pkgs) {
		clippkg_download_with_deps($pname, $plist, undef, $hash);
	}

	return 1;
}

=back

=head2 Mirror management

=over 4

=cut
                       ################################
		       #      Mirror management       #
		       ################################

# Field priorities for package fields, used to decide in which order to 
# include those fields in the per-mirror Packages.gz file.
my %fprios;

my @fields= qw(Package Source Version Priority Section Essential Maintainer Depends ConfDepends Recommends Suggests Conflicts Provides Replaces Architecture Filename Size Installed-Size MD5sum SHA1 SHA256 Description Homepage CLIP-Jails Category-fr Description-fr Impact Urgency Distribution Built-By Build-Date Release-Date License Urgency);

my $i = 100;
grep( $fprios{$_} = $i--, @fields);

=item B<CLIP::Pkg::Download::get_hash($pkg, $href, $cmd, $key)>

Internal use only. Hash package $pkg using $cmd (e.g. MD5sum), and 
insert the resulting value in the hash referenced by $href, with key
$key.

Returns 1 on success, 0 on error.

=cut

sub get_hash($$$$) {
	my ($pkg, $href, $cmd, $key) = @_;


	open PIPE, "$cmd $pkg 2>&1|"; 
	my $val = <PIPE>;
	close PIPE;
	if ($?) {
		clip_warn "$cmd $pkg failed";
		clip_warn "$cmd output: $val";
		return 0;
	}
	chomp $val;
	$val =~ s/ .*//;

	$href->{$key} = $val;

	return 1;
}

=item B<CLIP::Pkg::Download::get_pkginfo($pkg, $href, $sha)>

Internal use only. Get all information on package (path available in 
$pkg, relative to the base of the mirror) needed to represent this
package in a Packages.gz mirror description file, and insert that
info in a global mirror hash referenced by $href. $href is keyed by
package names (without version), and its values are references to 
hashes matching field names for field values, for each package in the
mirror.

SHA1 and SHA256 hashes are only included in the output if $sha is non-zero.

The function returns 1 on success, 0 on error.

=cut

sub get_pkginfo($$$) {
	my ($pkg, $href, $sha) = @_;
	my $phash;
	$phash = clippkg_get_fields($pkg, "");

	return 0 unless (get_hash($pkg, $phash, "md5sum", "MD5sum"));
	if ($sha) {
		return 0 unless (get_hash($pkg, $phash, "sha1sum", "SHA1"));
		return 0 unless (get_hash($pkg, $phash, "sha256sum", "SHA256"));
	}

	$phash->{'Filename'} = $pkg;

	my @stat = stat($pkg); 
	if ($?) {
		clip_warn "could not stat $pkg";
		return 0;
	}
	
	$phash->{'Size'} = $stat[7];

	delete $phash->{' Homepage'};

	my $pname = $phash->{'Package'};

	my $oldhash;
	if (defined ($oldhash = $href->{$pname})) {
		my $oldver = $oldhash->{'Version'};
		my $newver = $phash->{'Version'};

		if (versioncmp($newver, $oldver) > 0) {
			clip_warn "Duplicate entry for $pname, "
				."keeping version $newver over $oldver";
			$href->{$pname} = $phash;
		} else {
			clip_warn "Duplicate entry for $pname, keeping version "
				."$oldver over $newver";
		}
	} else {
		$href->{$pname} = $phash;
	}

	return 1;		
}

=item B<CLIP::Pkg::Download::read_packages_gz($path, $in, $hash)>

Internal use only. Reads a gzip-compressed Packages.gz mirror description file 
in $in, and populates the hash referenced by $hash with those packages referenced in $in
which are effectively present in $path.

$hash is built as a hash keyed by package names, with each value being itself a reference
to a hash matching field names to field values for that package.

Returns 1 on success, 0 on failure.

=cut

sub read_packages_gz($$$) {
	my ($path, $in, $hash) = @_;

	unless (open IN, "<", $in) {
		clip_warn "Failed to open $in for reading";
		return 1;
	}
	my $gz;
	unless (defined ($gz = gzopen(\*IN, "rb"))) {
		clip_warn "Failed to open gz stream to read $in";
		return 0;
	}

	my $pname;
	my $phash;
	my $line;
	while ($gz->gzreadline($line)) {
		next unless ($line =~ /^([^:]+): (.*)/);
		my $key = $1;
		my $val = $2;

		next if ($key eq " Homepage");

		if ($key eq "Package") {
			# check previous package - delete it from global hash
			# if it is no longer present in the mirror
			if ($pname) {
				my $pkg = $pname."_".$phash->{'Version'};
				$pkg .= "_".$phash->{'Architecture'}.".deb";
				delete $hash->{$pname} unless (-f "$path/$pkg");
			}
			$pname = $val;
			$phash = {};
			$hash->{$pname} = $phash;
		}

		unless ($pname) {
			clip_warn "No package for field $key ($val)";
			next;
		}

		$phash->{$key} = $val;
	}
	# check last package
	if ($pname) {
		my $pkg = $pname."_".$phash->{'Version'};
		$pkg .= "_".$phash->{'Architecture'}.".deb";
		delete $hash->{$pname} unless (-f "$path/$pkg");
	}
	if ($gz->gzclose()) {	# Returns 0 on success, don't ask...
		clip_warn "Failed to close input gz stream";
		return 0;
	} # closes IN as well

	return 1;
}


=item B<clippkg_gen_packages_gz($path, $in, $out, $list)>

Create a gzip-compressed Packages.gz mirror description file, output to file $out, 
based on existing Packages.gz file $in (if $in is not empty) and the new packages 
in $list (reference to a list of packages), which must be present in directory $path.
Packages referenced in $in but which are no longer present in $path are not kept
in the new $out file.
$path must be a path relative to the root of the mirror tree, which also needs to be 
the current working directory.

SHA1 and SHA256 hashes are only calculated for new packages if $sha is non-zero.

Returns 1 on success, 0 on failure.

=cut

sub clippkg_gen_packages_gz($$$$$) {
	my ($path, $in, $out, $list, $sha) = @_;

	my %hash = ();

	if ($in) {
		unless (read_packages_gz($path, $in, \%hash)) {
			clip_warn "Failed to read Packages.gz at $in";
			return 0;
		}
	}

	foreach my $pkg (@{$list}) {
		next unless (get_pkginfo("$path/$pkg", \%hash, $sha));
	}

	unless (open OUT, ">", $out) {
		clip_warn "Failed to open $out for writing";
		return 0;
	}

	my $gz;
	unless (defined ($gz = gzopen(\*OUT, "wb"))) {
		clip_warn "Failed to open gz stream to $out";
		return 0;
	}

	for my $key (sort keys %hash) {
		my $phash = $hash{$key};
		foreach (sort { $fprios{$b} <=> $fprios{$a} } keys %{$phash}) {
			unless ($gz->gzwrite("$_: $phash->{$_}\n")) {
				clip_warn "Failed to write to gz stream";
				$gz->gzclose();
				return 0;
			}
		}	
		unless ($gz->gzwrite("\n")) {
			clip_warn "Failed to write newline to gz stream";
			$gz->gzclose();
			return 0;
		}
	}

	if ($gz->gzclose()) {	# Returns 0 on success, don't ask...
		clip_warn "Failed to close gz stream";
		return 0;
	} # closes OUT as well

	return 1;
}

=item B<CLIP::Pkg::Download::write_flags($core, $apps, $base, $dist)>

Internal use only. Create flags $base/mirrors/flags to signal the availability
of updates. Creates a $dist_core flag file if $core is 1, and a $dist_apps flag
if $flag is 1. The 'flags' directory is created as needed. If both $core and 
$apps are 0, returns immediately (without creating the flags directory.

Returns 1 on success (including when nothing was done), 0 otherwise.

=cut

sub write_flags($$$$) {
	my ($core, $apps, $base, $dist) = @_;

	my $ret = 1;

	return 1 unless ($core or $apps);

	my $flagdir = "$base/mirrors/flags";

	unless (-d $flagdir) {
		unless (mkdir $flagdir) {
			clip_warn "failed to create directory: $flagdir";
			return 0;
		}
	}

	if ($core) {
		if (open OUT, ">", "$flagdir/".$dist."_core") {
			print OUT "New $dist core present";
			close OUT;
		} else {
			clip_warn "failed to write $dist core flag";
			$ret = 0;
		}
	}

	if ($apps) {
		if (open OUT, ">", "$flagdir/".$dist."_apps") {
			print OUT "New $dist apps present";
			close OUT;
		} else {
			clip_warn "failed to write $dist apps flag";
			$ret = 0;
		}
	}

	return $ret;
}

=item B<clippkg_gen_mirror($base, $dist, $source, $nolink, $list)>

Creates or updates the $dist mirror in the $base directory (i.e. the mirror 
contained in the C<$base/mirrors/$dist> directory) using new packages found 
in the $source directory. Only those packages that are present in $source, and 
in the list referenced by $list, if that is defined, and which are not present
in the mirror, are injected into it. The injection method varies depending on
the value of the $nolink parameter: if that is zero, the packages are 
hard-linked from $source into the mirror (which is only possible if both 
directories belong to the same VFS mount), otherwise they are copied from 
$source into the mirror.

Before the actual copy or linking takes place, the list of packages to be
injected in the mirror is written, one per line, in the 
C<$base/mirrors/$dist/pending> file, which is only removed once all packages 
have been injected, and the new mirror index has been generated. This allows 
the error recovery functions to remove those partially copied packages on the 
next run in case this call gets interrupted. That removal (of both packages 
and "pending" file) is also automatically performed by the function before 
it returns any error.

Note that once the new packages have been injected, the function prunes the 
mirror of its now useless packages, by calling the clippkg_prune() function 
from CLIP::Pkg::Base, before generating the new index for the mirror. That 
index is updated atomically once generated, to make sure a complete index 
(possibly not accurate) is present at all times, and thus avoid apt errors 
caused by a corrupted mirror.

The function returns 1 on success, and 0 on error.

=cut

sub clippkg_gen_mirror($$$$$) {
	my ($mirror_base, $dist, $source, $nolink, $list) = @_;
	my $mirpath = "$mirror_base/mirrors/$dist";

	# 1 if we inject new core packages, 0 otherwise
	my $new_core = 0;
	# 1 if we inject new apps packages, 0 otherwise
	my $new_apps = 0;

	my $arch;

	unless (defined($arch = clippkg_get_arch())) {
		clip_warn "Could not read architecture";
		return 0;
	}
	
	unless (chdir "$source") {
		clip_warn "could not chdir to $source";
		return 0;
	}
	
	my @debs;

	if (defined($list)) {
		@debs = @{$list};
	} else {
		@debs = <*.deb>;
	}

	my @copy = ();
	LOOP:
	foreach my $pkg (@debs) {
		if (-e $pkg and not -e "$mirpath/$pkg") {
			unless ($new_core and $new_apps) {
				my $prio;
				unless (defined($prio = 
						clippkg_get_fields($pkg, 'Priority'))) {
					clip_warn "failed to read Priority of package $pkg";
					next LOOP;
				}
				my $priority = lc($prio->{"Priority"});
				chomp($priority);
				if (not $new_core and $priority eq "required") {
					$new_core = 1;
				}
				if (not $new_apps and $priority eq "important") {
					$new_apps = 1;
				}
			}
			push @copy, ($pkg) 
		}
	}

	if ($#copy == -1) {
		clip_debug "no new packages to be copied into the mirror";
		return 1;
	}

	# Write out a copy of packages being copied, to be removed in case 
	# of interruption
	unless (write_pending ("$mirpath/pending", \@copy)) {
		clip_warn "failed to write list of packages to be "
			."copied into local mirror";
		return 0;
	}
	
	clip_log "updating mirror for $dist, adding packages: "
						.(join " ", @copy);
	if ($nolink) {
		foreach my $pkg (@copy) {
			unless (copy("$source/$pkg", "$mirpath/$pkg")) {
				clip_warn "could not copy $pkg into $mirpath";
				goto ERR;
			}
		}
	} else {
		foreach my $pkg (@copy) {
			if (-e "$mirpath/$pkg") {
				clip_warn "ignored existing link $pkg into $mirpath";
				next;
			}
			unless (link("$source/$pkg", "$mirpath/$pkg")) {
				clip_warn "could not link $pkg into $mirpath";
				goto ERR;
			}
		}
	}
		

	unless (clippkg_prune("$mirpath")) {
		clip_warn "failed to prune the local mirror";
		# do not error out in that case, let the update go on
	}

	unless (chdir "$mirror_base/mirrors") {
		clip_warn "could not chdir to $mirror_base/mirrors";
		goto ERR;
	}

	clip_debug "updating mirror metadata for $dist";

	unless (clippkg_gen_packages_gz($dist, 
			"dists/main/$dist/binary-$arch/Packages.gz",
			"dists/main/$dist/binary-$arch/Packages.gz.new",
			\@copy, 0)) {
		clip_warn "Failed to create mirror";
		goto ERR;
	}

	unless (move ("dists/main/$dist/binary-$arch/Packages.gz.new", 
				"dists/main/$dist/binary-$arch/Packages.gz")) {
		clip_warn "failed to update Packages.gz for $dist";
		goto ERR;
	}

	unless (write_flags($new_core, $new_apps, $mirror_base, $dist)) {
		clip_warn "failed to write mirror flags for $dist";
		goto ERR;
	}

	unlink "$mirpath/pending"
		or clip_warn "could not remove $mirpath/pending";

	clip_log "mirror generated for $dist";

	return 1;

ERR:
	
	foreach my $pkg (@copy) {
		next unless (-e "$mirpath/$pkg");

		unlink "$mirpath/$pkg" 
			or clip_warn "could not remove $mirpath/$pkg";
	}
	unlink "$mirpath/pending"
		or clip_warn "could not remove $mirpath/pending";

	return 0;
}

sub replace_source($$@) {
	my $file_src=shift;
	my $file_dst=shift;

	my @pattern_match=('@ETH0@', '@HOSTNAME@');
	my @pattern_replace=@_;

	unless (open IN, "<", "$file_src") {
		clip_warn "failed to open $file_src";
		return 0;
	}
	#my @sourcelist = <IN>;
	#clip_warn("XXX; src: @sourcelist");

	unless (open OUT, ">", "$file_dst") {
		close(IN);
		clip_warn "failed to open $file_dst";
		return 0;
	}

	my $i;
	while(<IN>) {
		for($i=0; $i <= $#pattern_replace; $i++) {
			s/$pattern_match[$i]/$pattern_replace[$i]/g;
		}
		print OUT;
	}
	close(OUT);
	close(IN);

	return 1;
}


sub clippkg_prepare_sourcelist() {
	unless (-d "$g_sourcelist_admin_dir") {
		clip_warn "cannot find directory '$g_sourcelist_admin_dir'";
		return 0;
	}

	my $jail;
	if ($g_sourcelist_file =~ /.*\/sources\.list\.([a-z_]{4})$/) {
		$jail = $1;
	} else {
		clip_warn "wrong sources.list.* filename";
		return 0;
	}

	my $sourcelist = "$g_sourcelist_admin_dir/sources.list.$jail";
	if ($sourcelist eq $g_sourcelist_file) {
		clip_warn "sourcelist files are the same";
		return 1;
	}
	unless (replace_source("$sourcelist", "$g_sourcelist_file",
			"$g_sourcelist_var_eth0", "$g_sourcelist_var_hostname")) {
		clip_warn "failed to prepare sources.list.$jail";
		return 0;
	}
	return 1;
}

=item B<clippkg_update_mirror($base, $dist, $recovered, $conflist, $err)>

Full run update of the local mirror for the $dist distribution in the 
$dist directory. This function first updates the local apt index, then
calls clippkg_download_all() to fetch all new configurations, and their
packages, into the local cache, and check them. Then it updates the local
mirror using those downloaded packages, to which can be optionally added
a set of packages from a previous incomplete run, that are passed as a list
referenced by $recovered. The caller is responsible for checking the 
signatures on those packages (e.g. with the clippkg_download_error_recovery()
function) before passing them to clippkg_update_mirror(). Once the local
mirror has been updated, the download cache is pruned of any superfluous 
packages before the function returns. The function also manages the 
C<$g_apt_cache_dir/pending> file left by clippkg_download_all(), removing
it once the new packages have all been injected in the local mirror.

The list of newly downloaded configurations is added to the list referenced
by $conflist, if that is defined.

The function returns 1 on success, and 0 on error, in which case an error
string is placed in the ${$err}. Possible error codes:
 "sourcelist" => preparing sources.list.* file failed
 "update" => apt-get update failed
 "download" => downloading all packages failed
 "mirror" => failed to update local mirror
=cut

sub clippkg_update_mirror($$$$$) {
	my ($mirror_base, $dist, $recovered, $conflist, $err) = @_;

	unless (clippkg_prepare_sourcelist()) {
		${$err} = "sourcelist";
		goto ERR;
	}

	unless (clippkg_update_db(0)) {
		${$err} = "update";
		goto ERR;
	}

	my @newdebs = ();

	unless (clippkg_download_all($dist, $conflist, 
				\@newdebs, "$mirror_base/mirrors/$dist")) {
		${$err} = "download";
		goto ERR;
	}

	# We remove the cache pending copy ourselves, and only after
	# updating the mirror, to make sure we redownload and/or re-check
	# any packages that were downloaded but not injected in the 
	# local mirror.

	if (defined($recovered)) {
		push @newdebs, @{$recovered};
	}

	goto OUT if ($#newdebs == -1); 

	# Filter new confs list, keeping only those that are not yet 
	# in the local mirror
	my $len = $#{$conflist};
	my $idx = 0;
	while ($idx <= $len) {
		my $conf = $conflist->[$idx];
		if (-e "$mirror_base/mirrors/$dist/$conf") {
			splice @{$conflist}, $idx, 1;
			$len--;
		} else {
			$idx++;
		}
	}

	unless (clippkg_gen_mirror($mirror_base, $dist, 
				"$g_apt_cache_dir/archives", 0, \@newdebs)) {
		${$err} = "mirror";
		goto ERR;
	}
		

	foreach (@${conflist}) {
		clip_log "Configuration $_ added to the local mirror";
	}

	unlink "$g_apt_cache_dir/pending" 
		or clip_warn "failed to remove $g_apt_cache_dir/pending";

OUT:
	unless (clippkg_prune("$g_apt_cache_dir/archives")) {
		clip_warn "failed to prune the download cache";
		# do not error out in that case, let the update go on
	}
	return 1;

ERR:
	clip_warn "aborting mirror update for $dist";
	return 0;
}

=back

=head2 Error recovery

=over 4

=cut

                       #############################
		       #      Error recovery       #
		       #############################


=item I<CLIP::Pkg::Download::remove_from_list($list, $path)>

Internal use only. Unlinks the packages listed in file $list (one per line, 
full names) from the $path directory, if they exist.
Returns 1 on success, and 0 on failure. 

=cut

sub remove_from_list($$) {
	my ($list, $path) = @_;

	unless (open IN, "<", "$list") {
		clip_warn "failed to open $list for reading";
		return 0;
	}

	my @pending = <IN>;
	close IN;

	my $ret = 1;

    PKGLOOP:
	foreach my $pkg (@pending) {
		chomp $pkg;
		next PKGLOOP unless ($pkg);
		if (-e "$path/$pkg") {
			clip_warn "removing pending $pkg";
			unless (unlink "$path/$pkg") {
				clip_warn "failed to remove $path/$pkg";
				$ret = 0;
			}
		} else {
			clip_warn "$path/$pkg does not exist";
		}
	}

	return $ret;
}

=item I<CLIP::Pkg::Download::check_from_list($list, $path)>

Internal use only. Checks the signatures of the packages listed in file $list 
(one per line, full names), when they exist in the $path directory. 
Packages that fail their signature check are removed from $path, those that 
match their signatures are left as is, and added to a list of "recovered"
packages which is returned by the function. Configuration packages are 
systematically removed, without any signature check, to force their re-download.

Returns a reference to a list of recovered packages on success, 
and C<undef> on failure. 

=cut

sub check_from_list($$) {
	my ($list, $path) = @_;
	my @checked = ();
	my $ret = 1;
	unless (open IN, "<", "$list") {
		clip_warn "failed to open $list for reading";
		return undef;
	}

	my @pending = <IN>;
	close IN;

   PKGLOOP:
	foreach my $pkg (@pending) {
		chomp $pkg;
		next PKGLOOP unless ($pkg);
		if (-e "$path/$pkg") {
			# Note: we remove configuration packages, to force 
			# apt to re-evaluate their dependencies.
			if (clippkg_is_conf($pkg)) {
				clip_warn "Removing previously downloaded $pkg "
						."to force dependency check";
				unlink "$path/$pkg" 
					or clip_warn "Failed to remove "
						."leftover $pkg, update might "
						."not be complete";
				next;
			}

			clip_log "Checking $pkg, leftover from previous download";
			unless (clippkg_check_sig("$path/$pkg")) {
				clip_warn "leftover $pkg failed "
					."signature check";
				unless (unlink "$path/$pkg") {
					clip_warn "Failed to remove leftover "
						."$pkg, aborting";
					return 0;
				}
			} else {
				clip_log "Package $pkg successfully recovered from "
						."previous download";
				push @checked, ($pkg);
			}
		} 
	}
	return \@checked;
}

=item B<clippkg_download_error_recovery($base, $dist, $recovered)>

Recovers from a previous interrupted run. This removes any pending packages 
(leftover from an interrupted clippkg_gen_mirror() call) in the $dist mirror 
in C<$base/mirrors/$dist>, then looks for leftover new packages in the 
download cache, checking the signatures of those that are present, and keeping 
only those that pass this check. Those recovered packages are added to the 
list referenced by $recovered, if defined, so that they may then be added to 
the local mirror by the caller.

Note that leftover configurations in the download cache are systematically 
removed, to force their re-download during this run, and thus force a 
re-evaluation of their dependencies.

This function also takes care of removing the C<pending> files left in the 
local mirror and download cache by the previous interrupted run.

Returns 1 on success, 0 on failure.

=cut

sub clippkg_download_error_recovery($$$) {
	my ($mirror_base, $dist, $recovered) = @_;

	my $ret = 1;
	my $mirrorpath = "$mirror_base/mirrors/$dist";
	if ( -e "$mirrorpath/pending") {
		clip_warn "error during previous update - cleaning local "
				."mirror";
		unless (remove_from_list("$mirrorpath/pending", "$mirrorpath")){
			clip_warn "error recovery failed";
			return 0;
		}
		unlink "$mirrorpath/pending"
			or clip_warn "error removing $mirrorpath/pending";
	}


	if ( -f "$g_apt_cache_dir/pending.new" ) {
		# We were interrupted while creating the new pending file
		# Theoretically there should be an old pending file of which
		# the - possibly incomplete - new one is only a subset. Thus
		# keeping the old one might incur some useless checks, but is
		# the safe way to go.
		if ( -f "$g_apt_cache_dir/pending") {
			unlink "$g_apt_cache_dir/pending.new"
				or clip_warn "error removing "
					."$g_apt_cache_dir/pending.new";
		} else {
			# OK, we've been taken to weirdo land. Let's not pay
			# attention to all the flying pigs and sheep outside,
			# and do as best we can.
			clip_warn "$g_apt_cache_dir/pending.new found "
				."without matching old file. "
				."Don't look outside...";
			move("$g_apt_cache_dir/pending.new", 
					"$g_apt_cache_dir/pending")
				or clip_warn "could not recreate old "
					."pending file";
				# At this point we're quite possibly in deep 
				# shit, and raising an error here won't save 
				# our arses. Let's pretend all is well...
		}
	}

	if ( -e "$g_apt_cache_dir/pending") {
		clip_warn "error during previous update - checking local cache";
		my $checked;
		
		unless (defined ($checked = 
				check_from_list("$g_apt_cache_dir/pending", 	
						"$g_apt_cache_dir/archives"))) {
			clip_warn "error recovery failed";
			return 0;
		}
		# Create - as atomically as possible - a new 'pending' file 
		# containing those files that were checked OK. Otherwise those 
		# files will be lost if a new interruption occurs before we 
		# can complete this run.
		unless (write_pending("$g_apt_cache_dir/pending.new", 
								$checked)) {
			clip_warn "error creating new pending file";
			return 0;
		}
		unless (move("$g_apt_cache_dir/pending.new", 
						"$g_apt_cache_dir/pending")) {
			clip_warn "could not replace pending file";
			return 0;
		}

		push @{$recovered}, @{$checked}
				if (defined($recovered));
		return 1;
	}
	
	return 1;
}

			

1;
__END__

=back

=head1 SEE ALSO

CLIP::Pkg::Base(3),CLIP::Pkg::Install(3),CLIP-Logger(3),dpkg(1),apt-get(8)

=head1 AUTHOR

Vincent Strubel, E<lt>clip@ssi.gouv.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008-2009 SGDN/DCSSI
Copyright (C) 2010-2013 SGDSN/ANSSI

All rights reserved.

=cut
