# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
#
# contributed by Emanuele Zeppieri
# modified by Christian Walde (threads)

use threads;

sub bottomup_tree {
    my ( $value, $depth ) = @_;
    return $value unless $depth;
    my $value2 = $value * 2;
    $depth--;
    return [ bottomup_tree( $value2 - 1, $depth ), bottomup_tree( $value2, $depth ), $value ];
}

sub check_tree {
    my ( $left, $right, $value ) = @{ $_[0] };
    return $value + ( ref $left ? check_tree( $left ) - check_tree( $right ) : $left - $right );
}

sub depth_iteration {
    my ( $depth, $max_depth, $min_depth ) = @_;

    my $iterations = 2 << $max_depth - $depth + $min_depth - 1;
    my $check      = 0;

    foreach ( 1 .. $iterations ) {
        $check += check_tree( bottomup_tree( $_,  $depth ) );
        $check += check_tree( bottomup_tree( -$_, $depth ) );
    }

    return ( $depth => [ $iterations, $depth, $check ] );
}

my $max_depth = shift @ARGV;
my $min_depth = 4;

$max_depth = $min_depth + 2 if $min_depth + 2 > $max_depth;

my $stretch_depth = $max_depth + 1;
my $stretch_tree = bottomup_tree( 0, $stretch_depth );
print "stretch tree of depth $stretch_depth\t check: ", check_tree( $stretch_tree ), "\n";
undef $stretch_tree;

my $longlived_tree = bottomup_tree( 0, $max_depth );

my @results;
for ( my $depth = $min_depth ; $depth <= $max_depth ; $depth += 2 ) {
    while ( 1 ) {
        last if threads->list < 4;
        push @results, $_->join for threads->list( threads::joinable );
    }

    threads->create(
        { 'context' => 'list', 'stack_size' => 64 },    #
        sub { depth_iteration( $depth, $max_depth, $min_depth ) }
    );
}

while ( threads->list ) {
    push @results, $_->join for threads->list( threads::joinable );
}

my %results = @results;
for my $key ( sort { $a <=> $b } keys %results ) {
    my ( $iterations, $depth, $check ) = @{ $results{$key} };
    print 2 * $iterations, "\t trees of depth $depth\t check: ", $check, "\n";
}

print "long lived tree of depth $max_depth\t check: ", check_tree( $longlived_tree ), "\n";
