#!/usr/bin/env perl
use strict;
use warnings;
use feature qw(say);
use Data::Dumper;

sub input1 {
	open(my $in,  "<",  "../inputs/input-16.1.txt")  or die "Can't open input.txt: $!";
	my @lines = <$in>;

        my @VAR;
        push @VAR, [ splice @lines, 0, 4 ] while @lines;

        foreach $a (@VAR){
            my ($op_code_ref, $before_ref, $after_ref) = parseInputSection(join('', @$a));
        }
}

my $sample_input = 'Before: [3, 1, 2, 3]\n5 3 1 1\nAfter:  [3, 0, 2, 3]';

sub parseInputSection {
	my ($input) = @_;
	my ($before, $op, $after) = split(/\\n/, $input);

        # trim 
        $before =~ tr/ Before:\[\]//d;
        $after =~ tr/ After:\[\]//d;



        my @befArr = split(/,/, $before);
        my ($opNum, $a, $b, $c) = split(/ /, $op);
        my @aftArr = split(/,/, $after);

        my %op_code;
        $op_code{'num'} = $opNum;
        $op_code{'a'} = $a;
        $op_code{'b'} = $b;
        $op_code{'c'} = $c;

        return (\%op_code, \@befArr, \@aftArr)
}

# sub addr {
# 	my ($inst, $bef, $aft) = @_;
# 	my @spt = split(', ', $aft);

# 	foreach my $i (@spt) {

# 	}
# }



input1();
