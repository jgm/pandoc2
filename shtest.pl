#!/usr/bin/env perl -w

use IPC::Open2;
use Text::Diff;
use Term::ANSIColor;
use Getopt::Std;
use Pod::Usage;
use Benchmark;
use warnings;

my %options = ();
getopts("cw:", \%options) or pod2usage(-verbose => 1) && exit;

$cmdsub = $options{w} || undef;
$colors = $options{c};

my $time_start = new Benchmark;

my $failures = 0;
my $passes   = 0;

foreach (@ARGV) {
  my $dir = $_;
  my @testfiles = `find "$dir" -name '*.test'`;

  foreach (@testfiles) {
    my $fn = $_;
    my $result = runtest($fn);
    my $ok = ($result =~ /^$/);
    if ($ok) {
      $passes += 1;
      if ($colors) {
        print colored ("[OK]     ", "yellow");
      } else {
        print "[OK]     ";
      }
      print $fn;
    } else {
      $failures += 1;
      if ($colors) {
        print colored ("[FAILED] ", "red");
      } else {
        print "[FAILED] ";
      }
      print $fn;
      if ($colors) {
        print colored ($result, "cyan");
      } else {
        print $result;
      }
    }
  }

}

my $time_end = new Benchmark;
my $time_diff = timediff($time_end, $time_start);

print "Passed:  ", $passes, " tests\n";
print "Failed:  ", $failures, " tests\n";
print "Time:   ", timestr($time_diff), "\n";

exit($failures);

# end of main program

sub runtest {
  my $fn = $_[0];
  open(FILE, $fn) or die "Can't read file 'filename' [$fn]\n";

  my $cmd = <FILE>;
  $cmd =~ s/[^ ]*/$cmdsub/ if defined $cmdsub;
  while (<FILE>) {
    if ($_ =~ /^<<< *$/) { last; }
  }

  my($outstream, $instream);
  open2($outstream, $instream, $cmd);

  while (<FILE>) {
    if ($_ =~ /^>>> *$/) {
      close $instream;
      last;
    } else {
      print $instream $_;
    }
  }

  my $expected = "";
  while (<FILE>) {
    $expected .= $_;
  }

  my $actual = "";
  while (<$outstream>) {
    $actual .= $_;
  }
  close $outstream;

  my $diff = diff \$expected, \$actual;

  return $diff;
}

__END__

=pod

=head1 NAME

B<shtest>


=head1 SYNOPSIS

B<shtest.pl> [ B<options> ]  [ I<directory> ... ]

=head1 DESCRIPTION


=head1 OPTIONS

=over 4

=item B<-w>

Specify the path to the program to test.  The first word of
the command line specified in each test will be replaced by this path.

=item B<-c>

Use ANSI colors in output.

=back

=head1 BUGS

=head1 VERSION HISTORY

1.0	Tue 06 Jun 2011

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2011 John MacFarlane

This is free software; you may redistribute it and/or modify it under
the same terms as Perl itself.

=cut
