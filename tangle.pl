#!/usr/bin/perl

$tangle_on = undef;
while (<>) {
  if (/#\+begin_src emacs-lisp(.*)/i) {
	 if ($1 !~ /:tangle no/) {
		$tangle_on = 1;
	 }
  }
  elsif (/#\+end_src/i) {
	 $tangle_on = undef;
  }
  elsif ($tangle_on) {
	 print;
  }
}
