:- [facts].
:- [helper].

% Parse the natural language commands using the given grammar
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

% Evaluate the logical reasoning for the parsed query
evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    findall(
        [TableName, Headers, FilteredRows],
        (
            member(TableInfo, TableColumnInfo),
            evaluate_table(TableInfo, Conditions, TableName, Headers, FilteredRows)
        ),
        FilteredTable
    ).

% Evaluate a single table based on conditions
evaluate_table([all, TableName], Conditions, TableName, Headers, FilteredRows) :-
    table(TableName, Headers),
    filter_rows(TableName, Headers, Conditions, FilteredRows).

evaluate_table([[Columns], TableName], Conditions, TableName, Columns, FilteredRows) :-
    table(TableName, Headers),
    filter_rows(TableName, Headers, Conditions, FilteredRows),
    project_columns(FilteredRows, Headers, Columns, FilteredRows).

% Filter rows based on conditions
filter_rows(TableName, Headers, [], Rows) :-
    findall(Row, row(TableName, Row), Rows).

filter_rows(TableName, Headers, [condition(Column, Equality, Value)], FilteredRows) :-
    column_index(Column, Headers, Index),
    findall(
        Row,
        (
            row(TableName, Row),
            match_condition(Equality, Row, Index, Value)
        ),
        FilteredRows
    ).

% Match a condition against a value
match_condition(<, Row, Index, Value) :-
    nth1(Index, Row, Cell),
    Cell @< Value.
match_condition(>, Row, Index, Value) :-
    nth1(Index, Row, Cell),
    Cell @> Value.
match_condition(=, Row, Index, Value) :-
    nth1(Index, Row, Value).

% Get the index of a column
column_index(Column, Headers, Index) :-
    nth1(Index, Headers, Column).

% Project specific columns from rows
project_columns([], _, _, []).
project_columns([Row | Rows], Headers, Columns, [ProjectedRow | ProjectedRows]) :-
    maplist(column_index, Columns, Indexes),
    maplist(nth1, Indexes, Row, ProjectedRow),
    project_columns(Rows, Headers, Columns, ProjectedRows).

% Define the grammar for parsing natural language commands
command([command, TableColumnInfo, CommandOperation]) -->
    ["Get"],
    table_column_info(TableColumnInfo),
    command_operation(CommandOperation),
    ["."].

table_column_info([[all, TableName]]) -->
    ["all", "from"], table_name(TableName).
table_column_info([[[Column], TableName]]) -->
    column(Column), ["from"], table_name(TableName).

command_operation([]) --> [].
command_operation([join, TableName, ColumnName]) -->
    ["linking", "customer", "by", "their"], column(ColumnName).

table_name(TableName) -->
    [Atom], { atom_string(TableName, Atom) }.

column(Column) -->
    [Atom], { atom_string(Column, Atom) }.
