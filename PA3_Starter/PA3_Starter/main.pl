:- [facts]. % Load the facts file containing tables 
:- [helper]. % Load helper predicates for printing tables

% Parse and evaluate each line
parse_and_evaluate(_, [], []). % Base case: Stop when no lines are left.

parse_and_evaluate(part2, [[Line, LineSplit] | T], ResultTail) :-
    write('Processing Line: '), write(Line), nl,  % Debug original line
    write('Tokenized Line: '), write(LineSplit), nl, % Debug tokenized version
    (   nlp_parse(LineSplit, Query) ->            % Parse the tokenized line into a query
        write('Parsed Query: '), write(Query), nl,
        evaluate_logical(Query, FilteredTable),   % Evaluate the parsed query
        write('Filtered Table: '), write(FilteredTable), nl,
        print_tables(FilteredTable)              % Output the filtered table in tabular form
    ;   write('Failed to parse the line: '), write(LineSplit), nl
    ),
    parse_and_evaluate(part2, T, ResultTail).     % Recursively process the remaining lines

% Entry point for the program
main :-
    current_prolog_flag(argv, [DataFile, PrintOption | _]), % Read command-line arguments
    open(DataFile, read, Stream),                % Open the input file
    read_file(Stream, Lines),                    % Read and tokenize the lines from the file
    close(Stream),                               % Close the file after reading
    parse_and_evaluate(PrintOption, Lines, _).   % Process each line based on the specified mode

% NLP Parsing Rules
nlp_parse(['Get', all, from, Table, '.'], [command, [[all, Table]], []]).

% Handles multiple columns with 'and'
nlp_parse(['Get', Column1, from, Table, 'and', Column2, from, Table, '.'],
    [command, [[[Column1, Column2], Table]], []]).

% Handles 'such that its values are either ... or ...'
nlp_parse(['Get', Column, from, Table, 'such', 'that', 'its', 'values', 'are', 'either' | Rest],
    [command, [[[Column], Table]], [such_that, Values]]) :-
    extract_values(Rest, Values).

% Handles 'linking' for join operations
nlp_parse(['Get', Columns, from, Table1, 'linking', Table2, 'by', Column, '.'],
    [command, [[[Columns], Table1, linking, Table2, Column]], []]).

% Handles conditions with 'where' and comparisons
nlp_parse(['Get', Columns, from, Table, 'where', Column, 'is', 'greater', 'than', Value, '.'],
    [command, [[[Columns], Table]], [where, [condition, Column, '>', Value]]]).
nlp_parse(['Get', Columns, from, Table, 'where', Column, 'is', 'less', 'than', Value, '.'],
    [command, [[[Columns], Table]], [where, [condition, Column, '<', Value]]]).

% Helper to extract values for 'either ... or'
extract_values([], []).
extract_values([',', or, Value | T], [Value | Rest]) :- extract_values(T, Rest).
extract_values([Value, ',', or | T], [Value | Rest]) :- extract_values(T, Rest).
extract_values([Value | T], [Value | Rest]) :- extract_values(T, Rest).

% Evaluate Logical Queries
evaluate_logical([command, [[all, Table]], []], [[Table, Headers, Rows]]) :-
    table(Table, Headers),
    is_list(Headers), % Ensure Headers is a list
    findall(Row, row(Table, Row), Rows). % Fetch all rows.

evaluate_logical([command, [[Columns, Table]], []], [[Table, Columns, FilteredRows]]) :-
    table(Table, Headers),
    is_list(Columns),
    findall(FilteredRow,
        (row(Table, Row), extract_columns(Row, Headers, Columns, FilteredRow)),
        FilteredRows).

evaluate_logical([command, [[Columns, Table]], [where, [condition, Column, Op, Value]]], [[Table, Columns, FilteredRows]]) :-
    table(Table, Headers),
    nth0(Index, Headers, Column),
    findall(Row,
        (row(Table, Row),
         nth0(Index, Row, CellValue),
         custom_compare(Op, CellValue, Value)),
        FilteredRows),
    extract_columns_for_rows(FilteredRows, Headers, Columns).

evaluate_logical([command, [[[Columns], Table1, linking, Table2, Column]], []], [[Table1, Columns, Result]]) :-
    table(Table1, Headers1),
    table(Table2, Headers2),
    nth0(Index1, Headers1, Column),
    nth0(Index2, Headers2, Column),
    findall(Row1,
        (row(Table1, Row1),
         row(Table2, Row2),
         nth0(Index1, Row1, Value),
         nth0(Index2, Row2, Value)),
        Result).

evaluate_logical([command, [[[Column], Table]], [such_that, Values]], [[Table, [Column], FilteredRows]]) :-
    table(Table, Headers),
    nth0(Index, Headers, Column),
    findall(Row,
        (row(Table, Row),
         nth0(Index, Row, CellValue),
         member(CellValue, Values)),
        FilteredRows).

% Helper predicates for extracting columns and filtering rows
extract_columns_for_rows([], _, _, []).
extract_columns_for_rows([Row | Rows], Headers, Columns, [FilteredRow | FilteredRows]) :-
    extract_columns(Row, Headers, Columns, FilteredRow),
    extract_columns_for_rows(Rows, Headers, Columns, FilteredRows).

extract_columns(Row, Headers, Columns, FilteredRow) :-
    findall(Value,
        (nth0(Index, Headers, Column), member(Column, Columns), nth0(Index, Row, Value)),
        FilteredRow).

% Comparison helpers
custom_compare('>', A, B) :- A > B.
custom_compare('<', A, B) :- A < B.
custom_compare('=', A, B) :- A = B.
=======

:- [helper]. % Load helper predicates for printing tables



% Parse and evaluate each line

parse_and_evaluate(_, [], []). % Base case: Stop when no lines are left.



parse_and_evaluate(part2, [[Line, LineSplit] | T], ResultTail) :-

    write('Processing Line: '), write(Line), nl,  % Debug original line

    write('Tokenized Line: '), write(LineSplit), nl, % Debug tokenized version

    (   nlp_parse(LineSplit, Query) ->            % Parse the tokenized line into a query

        write('Parsed Query: '), write(Query), nl,

        evaluate_logical(Query, FilteredTable),   % Evaluate the parsed query

        write('Filtered Table: '), write(FilteredTable), nl,

        print_tables(FilteredTable)              % Output the filtered table in tabular form

    ;   write('Failed to parse the line: '), write(LineSplit), nl

    ),

    parse_and_evaluate(part2, T, ResultTail).     % Recursively process the remaining lines



% Entry point for the program

main :-

    current_prolog_flag(argv, [DataFile, PrintOption | _]), % Read command-line arguments

    open(DataFile, read, Stream),                % Open the input file

    read_file(Stream, Lines),                    % Read and tokenize the lines from the file

    close(Stream),                               % Close the file after reading

    parse_and_evaluate(PrintOption, Lines, _).   % Process each line based on the specified mode



% NLP Parsing Rules

nlp_parse(['Get', all, from, Table, '.'], [command, [[all, Table]], []]).



% Handles multiple columns with 'and'

nlp_parse(['Get', Column1, from, Table, 'and', Column2, from, Table, '.'],

    [command, [[[Column1, Column2], Table]], []]).



% Handles 'such that its values are either ... or ...'

nlp_parse(['Get', Column, from, Table, 'such', 'that', 'its', 'values', 'are', 'either' | Rest],

    [command, [[[Column], Table]], [such_that, Values]]) :-

    extract_values(Rest, Values).



% Handles 'linking' for join operations

nlp_parse(['Get', Columns, from, Table1, 'linking', Table2, 'by', Column, '.'],

    [command, [[[Columns], Table1, linking, Table2, Column]], []]).



% Handles conditions with 'where' and comparisons

nlp_parse(['Get', Columns, from, Table, 'where', Column, 'is', 'greater', 'than', Value, '.'],

    [command, [[[Columns], Table]], [where, [condition, Column, '>', Value]]]).

nlp_parse(['Get', Columns, from, Table, 'where', Column, 'is', 'less', 'than', Value, '.'],

    [command, [[[Columns], Table]], [where, [condition, Column, '<', Value]]]).



% Helper to extract values for 'either ... or'

extract_values([], []).

extract_values([',', or, Value | T], [Value | Rest]) :- extract_values(T, Rest).

extract_values([Value, ',', or | T], [Value | Rest]) :- extract_values(T, Rest).

extract_values([Value | T], [Value | Rest]) :- extract_values(T, Rest).



% Evaluate Logical Queries

evaluate_logical([command, [[all, Table]], []], [[Table, Headers, Rows]]) :-

    table(Table, Headers),

    is_list(Headers), % Ensure Headers is a list

    findall(Row, row(Table, Row), Rows). % Fetch all rows.



evaluate_logical([command, [[Columns, Table]], []], [[Table, Columns, FilteredRows]]) :-

    table(Table, Headers),

    is_list(Columns),

    findall(FilteredRow,

        (row(Table, Row), extract_columns(Row, Headers, Columns, FilteredRow)),

        FilteredRows).



evaluate_logical([command, [[Columns, Table]], [where, [condition, Column, Op, Value]]], [[Table, Columns, FilteredRows]]) :-

    table(Table, Headers),

    nth0(Index, Headers, Column),

    findall(Row,

        (row(Table, Row),

         nth0(Index, Row, CellValue),

         custom_compare(Op, CellValue, Value)),

        FilteredRows),

    extract_columns_for_rows(FilteredRows, Headers, Columns).



evaluate_logical([command, [[[Columns], Table1, linking, Table2, Column]], []], [[Table1, Columns, Result]]) :-

    table(Table1, Headers1),

    table(Table2, Headers2),

    nth0(Index1, Headers1, Column),

    nth0(Index2, Headers2, Column),

    findall(Row1,

        (row(Table1, Row1),

         row(Table2, Row2),

         nth0(Index1, Row1, Value),

         nth0(Index2, Row2, Value)),

        Result).



evaluate_logical([command, [[[Column], Table]], [such_that, Values]], [[Table, [Column], FilteredRows]]) :-

    table(Table, Headers),

    nth0(Index, Headers, Column),

    findall(Row,

        (row(Table, Row),

         nth0(Index, Row, CellValue),

         member(CellValue, Values)),

        FilteredRows).



% Helper predicates for extracting columns and filtering rows

extract_columns_for_rows([], _, _, []).

extract_columns_for_rows([Row | Rows], Headers, Columns, [FilteredRow | FilteredRows]) :-

    extract_columns(Row, Headers, Columns, FilteredRow),

    extract_columns_for_rows(Rows, Headers, Columns, FilteredRows).



extract_columns(Row, Headers, Columns, FilteredRow) :-

    findall(Value,

        (nth0(Index, Headers, Column), member(Column, Columns), nth0(Index, Row, Value)),

        FilteredRow).



% Comparison helpers

custom_compare('>', A, B) :- A > B.

custom_compare('<', A, B) :- A < B.

custom_compare('=', A, B) :- A = B.
