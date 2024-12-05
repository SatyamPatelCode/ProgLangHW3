% main.pl
% Include the helper and fact files which provide table definitions and helper predicates
:- [helper].
:- [facts].

% Entry point of the program. Reads command-line arguments, processes the input file, and runs the desired part (part1 or part2).
main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [FileName, PartOption] ->
        open(FileName, read, Stream),
        read_file(Stream, InputLines),
        close(Stream),
        process_lines(InputLines, PartOption)
    ; write('Invalid arguments.'), nl
    ).

% process_lines/2 recursively processes each input line.
% For each line, it attempts to parse it into a query and then executes the desired part option.
process_lines([], _).
process_lines([[OriginalLine, LineSplit]|Rest], PartOption) :-
    ( nlp_parse(LineSplit, Query) ->
        ( PartOption == 'part1' ->
            % part1 just prints out the parsed query structure
            writeln(Query)
        ; PartOption == 'part2' ->
            % part2 executes the query (logical evaluation, filtering tables) and prints the resulting tables
            write(OriginalLine), nl,
            evaluate_logical(Query, FilteredTable),
            print_tables(FilteredTable)
        ; write('Invalid part option.'), nl
        )
    ; % If parsing fails, report it
      write('Failed to parse line: '), write(OriginalLine), nl
    ),
    process_lines(Rest, PartOption).

% nlp_parse/2 uses a DCG (Definite Clause Grammar) phrase to parse the tokenized line into a query structure.
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

% The grammar rules below define the structure of the expected commands.
% command/3 parses a "Get" statement followed by table/column info and an optional command operation, ending with a period.
command([command, TableColumnInfo, CommandOperation]) -->
    ['Get'], table_column_info(TableColumnInfo), command_operation(CommandOperation), ['.'].

% table_column_info/3 parses one or more table_column_detail structures, possibly joined by "and".
table_column_info(TableColumnInfo) -->
    table_column_detail(Detail),
    ( ['and'], table_column_info(Rest) ->
        { TableColumnInfo = [Detail|Rest] }
    ; { TableColumnInfo = [Detail] }
    ).

% table_column_detail/3 parses either "all from TableName" or "Columns from TableName"
table_column_detail(Info) -->
    ( ['all', 'from'], table(TableName) ->
        { Info = [all, TableName] }
    ; columns(Columns), ['from'], table(TableName) ->
        { Info = [Columns, TableName] }
    ).

% command_operation/3 parses optional join, match, or where operations following the table/column info.
% If none match, returns an empty operation list.
command_operation(Operation) -->
    ( join_operation(Operation)
    ; match_operation(Operation)
    ; where_operation(Operation)
    ; { Operation = [] }
    ).

% join_operation/3 parses linking/connecting clauses: "linking TableName by their ColumnName"
join_operation([join, TableName, ColumnName]) -->
    ( ['linking']; ['connecting'] ), table(TableName), ['by', 'their'], col(ColumnName).

% match_operation/3 parses "such that its values are either ..." or "ColumnName matches values within ColumnName in Table ..."
match_operation(Operation) -->
    ['such', 'that'], match_condition(Operation).

% match_condition/3 parses conditions of "its values are either ..." or "ColumnName matches values within the Column in Table [where ...]"
match_condition([matches, Values]) -->
    ['its', 'values', 'are', 'either'], values(Values).

match_condition([matches, ColumnName, Query]) -->
    col(ColumnName), ['matches', 'values', 'within', 'the'], col(MatchColName), ['in'], table(MatchTableName),
    ( where_operation(WhereOp) ->
        { Query = [command, [[MatchColName, MatchTableName]], WhereOp] }
    ; { Query = [command, [[MatchColName, MatchTableName]], []] }
    ).

% where_operation/3 parses a "where" clause that introduces conditions
where_operation([where, Condition]) -->
    ['where'], or_condition(Condition).

% or_condition/3 and either_block/3 define the grammar for possibly nested "either/or" conditions combined with "and".
% This structure allows "either ... or ..." and then possibly another "and" sequence.
or_condition(Condition) -->
    either_block(Cond),
    ( ['and'], or_condition(Cond2) ->
        { Condition = [and, Cond, Cond2] }
    ; { Condition = Cond }
    ).

% either_block/3 parses either a simple condition or an "either condition1 or condition2" block.
either_block(Condition) -->
    ['either'], condition(Cond1), ['or'], condition(Cond2),
    { Condition = [or, Cond1, Cond2] }.
either_block(Cond) -->
    condition(Cond).

% condition/3 parses a single condition: ColumnName equals/less/greater than a Value.
condition([condition, ColumnName, Equality, Value]) -->
    col(ColumnName), equality(Equality), val(Value).

% equality/3 matches the equality tokens to internal representations '=' '<' '>'
equality('=') --> ['equals'].
equality('<') --> ['is', 'less', 'than'].
equality('>') --> ['is', 'greater', 'than'].

% table/3, columns/3, col_list/3, col/3, val/3, and values/3 define parsing of table names, columns, and values.
table(TableName) -->
    [TableName], { atom(TableName) }.

columns(Columns) -->
    col_list(Columns).

col_list([Col|Rest]) -->
    col(Col),
    ( [','], col_list(Rest) -> { true }
    ; ['and'], col_list(Rest) -> { true }
    ; { Rest = [] }
    ).

col(ColName) -->
    [ColName], { atom(ColName) }.

values([Val|Rest]) -->
    val(Val),
    ( [','], values(Rest) -> { true }
    ; ['or'], val(Val2),
      { Rest = [Val2] }
    ; { Rest = [] }
    ).

val(Value) -->
    [Value], { atom(Value) }.

% Evaluation of the parsed query
% evaluate_logical/2 takes a parsed query and retrieves the required data from tables, applying conditions.
evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    process_table_column_info(TableColumnInfo, ProcessedTableColumnInfo),
    get_tables_data(ProcessedTableColumnInfo, Conditions, FilteredTable).

% process_table_column_info/2 converts the parsed table/column info into a normalized form for retrieval.
process_table_column_info([], []).

process_table_column_info([[all, TableName]|Rest], [[TableName, all]|RestProcessed]) :-
    process_table_column_info(Rest, RestProcessed).

process_table_column_info([[Columns, TableName]|Rest], [[TableName, Columns]|RestProcessed]) :-
    Columns \= all,
    process_table_column_info(Rest, RestProcessed).

% get_tables_data/3 fetches data from each specified table and applies conditions.
get_tables_data([], _, []).

get_tables_data([[TableName, Columns]|Rest], Conditions, [[TableName, ColumnHeaders, Rows]|RestTables]) :-
    get_table_columns(TableName, Columns, ColumnHeaders),
    table(TableName, AllColumns),
    get_table_rows(TableName, Columns, Conditions, AllColumns, Rows),
    get_tables_data(Rest, Conditions, RestTables).

% get_table_columns/3 obtains the column headers based on whether "all" columns or a subset is requested.
get_table_columns(TableName, all, ColumnHeaders) :-
    table(TableName, ColumnHeaders).

get_table_columns(_TableName, Columns, ColumnHeaders) :-
    Columns \= all,
    ColumnHeaders = Columns.

% get_table_rows/5 uses findall to retrieve matching rows based on Conditions.
% If join/matches are present (not fully handled here), returns empty rows.
% Otherwise, filters rows by evaluate_condition/4 and selects the requested columns.
get_table_rows(TableName, Columns, Conditions, AllColumns, Rows) :-
    ( Conditions = [join|_] ; Conditions = [matches|_] ) ->
        Rows = []
    ; findall(RowValuesSelected,
        ( row(TableName, RowValuesAll),
          ( Conditions = [where, Condition] ->
              evaluate_condition(Condition, TableName, RowValuesAll)
          ; Conditions = [] ->
              true
          ),
          select_columns(RowValuesAll, Columns, AllColumns, RowValuesSelected)
        ),
        UnfilteredRows),
      remove_duplicates_preserving_order(UnfilteredRows, Rows).

% select_columns/4 extracts only the requested columns from the full row if columns are specified.
select_columns(RowValuesAll, all, _AllColumns, RowValuesAll).

select_columns(RowValuesAll, Columns, AllColumns, RowValuesSelected) :-
    Columns \= all,
    find_column_indices(Columns, AllColumns, Indices),
    extract_values_at_indices(RowValuesAll, Indices, RowValuesSelected).

% find_column_indices/3 finds the indices of specified columns in the full column list.
find_column_indices([], _, []).
find_column_indices([Col|RestCols], AllColumns, [Index|RestIndices]) :-
    nth0(Index, AllColumns, Col),
    find_column_indices(RestCols, AllColumns, RestIndices).

% extract_values_at_indices/3 picks the requested indices from a row.
extract_values_at_indices(RowValuesAll, Indices, RowValuesSelected) :-
    maplist(nth0_from_list(RowValuesAll), Indices, RowValuesSelected).

nth0_from_list(List, Index, Element) :-
    nth0(Index, List, Element).

% evaluate_condition/4 checks if a given row meets the specified condition(s).
evaluate_condition([condition, ColumnName, Operator, Value], TableName, RowValuesAll) :-
    table(TableName, AllColumns),
    nth0(Index, AllColumns, ColumnName),
    nth0(Index, RowValuesAll, CellValue),
    compare_values(CellValue, Operator, Value).

evaluate_condition([and, Cond1, Cond2], TableName, RowValuesAll) :-
    evaluate_condition(Cond1, TableName, RowValuesAll),
    evaluate_condition(Cond2, TableName, RowValuesAll).

evaluate_condition([or, Cond1, Cond2], TableName, RowValuesAll) :-
    ( evaluate_condition(Cond1, TableName, RowValuesAll)
    ; evaluate_condition(Cond2, TableName, RowValuesAll)
    ).

% compare_values/3 directs to the appropriate comparison: equals, less than, greater than
compare_values(CellValue, '=', Value) :-
    compare_values_eq(CellValue, Value).

compare_values(CellValue, '<', Value) :-
    compare_values_lt(CellValue, Value).

compare_values(CellValue, '>', Value) :-
    compare_values_gt(CellValue, Value).

% Helper predicates for type checking and comparison
% both_numbers/4 checks if both values are numbers, both_dates/4 checks if both parse as dates, both_strings/2 checks if both atoms.
both_numbers(Val1, Val2, Num1, Num2) :-
    attempt_number(Val1, Num1),
    attempt_number(Val2, Num2).

both_dates(Val1, Val2, Date1, Date2) :-
    attempt_date(Val1, Date1),
    attempt_date(Val2, Date2).

both_strings(Val1, Val2) :-
    atom(Val1), atom(Val2).

% compare_values_eq/2, compare_values_lt/2, compare_values_gt/2 handle equality/inequality.
compare_values_eq(CellValue, Value) :-
    ( both_numbers(CellValue, Value, NumCell, NumVal) ->
        NumCell =:= NumVal
    ; both_dates(CellValue, Value, DateCell, DateVal) ->
        date_compare('=', DateCell, DateVal)
    ; both_strings(CellValue, Value) ->
        CellValue = Value
    ).

compare_values_lt(CellValue, Value) :-
    ( both_numbers(CellValue, Value, NumCell, NumVal) ->
        NumCell < NumVal
    ; both_dates(CellValue, Value, DateCell, DateVal) ->
        date_compare('<', DateCell, DateVal)
    ; false
    ).

compare_values_gt(CellValue, Value) :-
    ( both_numbers(CellValue, Value, NumCell, NumVal) ->
        NumCell > NumVal
    ; both_dates(CellValue, Value, DateCell, DateVal) ->
        date_compare('>', DateCell, DateVal)
    ; false
    ).

% attempt_number/2 tries to parse a value as a number.
attempt_number(Value, NumValue) :-
    ( number(Value) ->
        NumValue = Value
    ; atom(Value),
      atom_number(Value, NumValue)
    ), !.

% attempt_date/2 tries to parse a value as a date of format "M-D-YYYY" and validates it.
attempt_date(Value, DateValue) :-
    ( atom(Value) -> atom_string(Value, StrValue) ; StrValue = Value ),
    split_string(StrValue, "-", "", [SM, SD, SY]),
    atom_number(SM, Month),
    atom_number(SD, Day),
    atom_number(SY, Year),
    valid_date(Day, Month, Year),
    DateValue = date(Year,Month,Day).

% date_compare/3 compares two date(Year,Month,Day) terms numerically.
date_compare(Operator, date(Y1,M1,D1), date(Y2,M2,D2)) :-
    date_to_number(date(Y1,M1,D1), N1),
    date_to_number(date(Y2,M2,D2), N2),
    ( Operator = '<', N1 < N2
    ; Operator = '>', N1 > N2
    ; Operator = '=', N1 =:= N2
    ).

% date_to_number/2 converts a date to a number YYYYMMDD for easier comparisons.
date_to_number(date(Y,M,D), NumDate) :-
    NumDate is Y*10000 + M*100 + D.

% remove_duplicates_preserving_order/2 removes duplicate rows while keeping the original order.
remove_duplicates_preserving_order(List, Result) :-
    remove_dup_helper(List, [], Result).

% remove_dup_helper/3 implements the actual logic to remove duplicates without sorting.
remove_dup_helper([], _, []).
remove_dup_helper([H|T], Seen, [H|R]) :-
    \+ memberchk(H, Seen),
    remove_dup_helper(T, [H|Seen], R).
remove_dup_helper([H|T], Seen, R) :-
    memberchk(H, Seen),
    remove_dup_helper(T, Seen, R).
