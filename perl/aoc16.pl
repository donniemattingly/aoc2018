#!/usr/bin/env perl
use strict;
use warnings;
use feature qw(say);
use Data::Dumper;

sub parsePart2Input {
    my $filename = "../inputs/input-16.2.txt";

    open(my $in,  "<", $filename)  or die "Can't open input.txt: $!";
    my @lines = <$in>;

    my @inputs = ();

    foreach my $line (@lines) {
       my @spl = split(/ /, $line);
       push @inputs, \@spl;
    }

    return \@inputs;
}

sub parseInputSection {
    # my $filename = "../inputs/input-16.1.txt";
    my $filename = "../inputs/input-16.1.txt";

    open(my $in,  "<", $filename)  or die "Can't open input.txt: $!";
    my @lines = <$in>;

    my @VAR;
    push @VAR, [ splice @lines, 0, 4 ] while @lines;


    my @inputs = ();

    foreach $a (@VAR){
        my ($before, $op, $after) = @$a;

        # trim 
        $before =~ tr/ \t\nBefore:\[\]//d;
        $after =~ tr/ \t\nAfter:\[\]//d;



        my @befArr = split(/,/, $before);
        my ($opNum, $a, $b, $c) = split(/ /, $op);
        my @aftArr = split(/,/, $after);

        my %op_code;
        $op_code{'num'} = $opNum;
        $op_code{'a'} = $a;
        $op_code{'b'} = $b;
        $op_code{'c'} = $c;

        my @input = (\%op_code, \@befArr, \@aftArr);

        push @inputs, \@input;
        
    }

    return \@inputs;
}

our @r = (0, 0, 0, 0);

my %opcodes = (
    addr => sub { $r[$_[2]] = $r[$_[0]] + $r[$_[1]] },
    addi => sub { $r[$_[2]] = $r[$_[0]] + $_[1] },
    mulr => sub { $r[$_[2]] = $r[$_[0]] * $r[$_[1]] },
    muli => sub { $r[$_[2]] = $r[$_[0]] * $_[1] },
    banr => sub { $r[$_[2]] = $r[$_[0]] & $r[$_[1]] },
    bani => sub { $r[$_[2]] = $r[$_[0]] & $_[1] },
    borr => sub { $r[$_[2]] = $r[$_[0]] | $r[$_[1]] },
    bori => sub { $r[$_[2]] = $r[$_[0]] | $_[1] },
    setr => sub { $r[$_[2]] = $r[$_[0]] },
    seti => sub { $r[$_[2]] = $_[0] },
    gtir => sub { $r[$_[2]] = $_[0] > $r[$_[1]] },
    gtri => sub { $r[$_[2]] = $r[$_[0]] > $_[1] },
    gtrr => sub { $r[$_[2]] = $r[$_[0]] > $r[$_[1]] },
    eqir => sub { $r[$_[2]] = $_[0] == $r[$_[1]] },
    eqri => sub { $r[$_[2]] = $r[$_[0]] == $_[1] },
    eqrr => sub { $r[$_[2]] = $r[$_[0]] == $r[$_[1]] }
    );



my $inputs_ref = parseInputSection();

my $matchThree = 0;
my %numToFn;

foreach my $sample (@$inputs_ref) {
    my ($opr, $bfr, $afr) = @$sample;
    my $match_count = 0;
    my $matched_name;
    while (my ($key, $value) = each (%opcodes)) {

        # set local registers
        local @r = ($bfr->[0], $bfr->[1], $bfr->[2], $bfr->[3]);

        # apply op code function to registers
        $value->($opr->{a}, $opr->{b}, $opr->{c});
        
        # compare against input
        if($r[0] == $afr->[0] 
           && $r[1] == $afr->[1] 
           && $r[2] == $afr->[2] 
           && $r[3] == $afr->[3]){
            $match_count += 1;
            $matched_name = $key;
        }
    }

    if($match_count >= 3){
        $matchThree += 1;
    }

    if($match_count == 1){

        # save our number to op code mapping
        my $opNum = $opr->{'num'};
        $numToFn{$opNum} = $opcodes{$matched_name};

        # remove it as an option
        delete $opcodes{$matched_name};
    }
}

# print Dumper(%numToFn);

# print "Matches: $matchThree \n";

my $part2Inputs = parsePart2Input();

foreach my $s (@$part2Inputs) {
    $numToFn{$s->[0]}->($s->[1], $s->[2], $s->[3]);
}

print $r[0];
