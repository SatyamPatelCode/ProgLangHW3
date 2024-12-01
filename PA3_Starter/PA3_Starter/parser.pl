use strict;
use warnings;

# Parse command
sub parse_command {
    my ($tokens) = @_;
    my ($table_info, $remaining1) = parse_table_column_info($tokens);
    return unless $table_info;

    my ($command_op, $remaining2) = parse_command_operation($remaining1);
    return unless $command_op;

    if ($remaining2->[0] eq '.') {
        return (["command", $table_info, $command_op], [@{$remaining2}[1 .. $#$remaining2]]);
    }
    return;
}

# Parse table column info
sub parse_table_column_info {
    my ($tokens) = @_;

    if ($tokens->[0] eq 'all' && $tokens->[1] eq 'from') {
        my $table_name = $tokens->[2];
        return ([['all', $table_name]], [@{$tokens}[3 .. $#$tokens]]);
    }

    my ($columns, $remaining1) = parse_columns($tokens);
    return unless $columns;

    if ($remaining1->[0] eq 'from') {
        my $table_name = $remaining1->[1];
        return ([[[$columns, $table_name]]], [@{$remaining1}[2 .. $#$remaining1]]);
    }

    return;
}

# Parse columns
sub parse_columns {
    my ($tokens) = @_;
    my @columns;

    while (1) {
        my $column = $tokens->[0];
        last unless defined $column && $column =~ /^[A-Za-z_]+$/;

        push @columns, $column;
        shift @$tokens;

        last unless $tokens->[0] =~ /^(,|and)$/;
        shift @$tokens;
    }

    return (\@columns, $tokens);
}

# Parse command operation
sub parse_command_operation {
    my ($tokens) = @_;

    if ($tokens->[0] =~ /^(linking|connecting)$/) {
        my $join_type = shift @$tokens;
        my $table_name = shift @$tokens;
        if ($tokens->[0] eq 'by' && $tokens->[1] eq 'their') {
            shift @$tokens;
            shift @$tokens;
            my $column_name = shift @$tokens;
            return ([join => $table_name, $column_name], $tokens);
        }
    }

    if ($tokens->[0] eq 'such' && $tokens->[1] eq 'that') {
        shift @$tokens;
        shift @$tokens;
        if ($tokens->[0] eq 'its' && $tokens->[1] eq 'values' && $tokens->[2] eq 'are' && $tokens->[3] eq 'either') {
            shift @$tokens for 1 .. 4;
            my ($values, $remaining) = parse_values($tokens);
            return ([matches => $values], $remaining);
        }
    }

    return ([], $tokens);
}

# Parse values
sub parse_values {
    my ($tokens) = @_;
    my @values;

    while (1) {
        my $value = $tokens->[0];
        last unless defined $value;

        push @values, $value;
        shift @$tokens;

        last unless $tokens->[0] =~ /^(,|or)$/;
        shift @$tokens;
    }

    return (\@values, $tokens);
}
