# The Computer Language Benchmarks Game
# # http://benchmarksgame.alioth.debian.org/
# # initial fannkuch port from C by Steve Clark
# #   rewrite by Kalev Soikonen
# #   modified by Kuang-che Wu
# #   modified by David Golden
# # updated for fannkuch-redux by Jonathan DePeri
# #   permutations generated using Mike Pall's approach
# # modified by Haisi Yi

use integer;

sub fannkuchredux {
    my ($m, $checksum, $maxflips, $flips, $sign) = ((shift)-1, 0, 0, 0, 1);
    my @count = 0 .. $m; 
    my $p = pack "c*", @count;
    my ($q, $f, $i);
    while(1) {
        if (ord($p)) {
            $q = $p;
            $flips = 0;
            while (($f = ord($q))++) {
                $flips++;
                substr($q, 0, $f, reverse(substr($q,0,$f)));
            }
            $maxflips = $flips if ($flips > $maxflips);
            $checksum += ($sign * $flips);
        }
        if ($sign == 1) {
            $sign = -1;
            substr $p, 1, 0, (substr($p,0,1,""));
        } else {
            $sign = 1;
            substr $p, 1, 0, (substr($p,2,1,""));
            for $i (2 .. $m) {
				if ($count[$i]) { $count[$i]--; last; }
	            return ($checksum, $maxflips) if ($i == $m);
	            $count[$i] = $i;
	            substr $p, $i+1, 0, (substr($p,0,1,""));
            }
        }
    }
}

exit -1 if not defined $ARGV[0] or $ARGV[0] <= 2;
my ($checksum, $maxflips) = &fannkuchredux($ARGV[0]);
print "$checksum\nPfannkuchen($ARGV[0]) = $maxflips\n";
