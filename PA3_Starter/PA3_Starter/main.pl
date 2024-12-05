% main.pl
% Include the helper and facts files
:- [helper].
:- [facts].

main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [FileName, PartOption] ->
        open(FileName, read, Stream),
        read_file(Stream, InputLines),
        close(Stream),
        process_lines(InputLines, PartOption)
    ; write('Invalid arguments.'), nl
    ).

process_lines([], _).
process_lines([[OriginalLine, LineSplit]|Rest], PartOption) :-
    ( nlp_parse(LineSplit, Query) ->
        ( PartOption == 'part1' ->
            writeln(Query)
        ; PartOption == 'part2' ->
            write(OriginalLine), nl,
            evaluate_logical(Query, FilteredTable),
            print_tables(FilteredTable)
        ; write('Invalid part option.'), nl
        )
    ; write('Failed to parse line: '), write(OriginalLine), nl
    ),
    process_lines(Rest, PartOption).

% Parsing the commands using DCG
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

command([command, TableColumnInfo, CommandOperation]) -->
    ['Get'], table_column_info(TableColumnInfo), command_operation(CommandOperation), ['.'].

table_column_info(TableColumnInfo) -->
    table_column_detail(Detail),
    ( ['and'], table_column_info(Rest) ->
        { TableColumnInfo = [Detail|Rest] }
    ; { TableColumnInfo = [Detail] }
    ).

table_column_detail(Info) -->
    ( ['all', 'from'], table(TableName) ->
        { Info = [all, TableName] }
    ; columns(Columns), ['from'], table(TableName) ->
        { Info = [Columns, TableName] }
    ).

command_operation(Operation) -->
    ( join_operation(Operation)
    ; match_operation(Operation)
    ; where_operation(Operation)
    ; { Operation = [] }
    ).

join_operation([join, TableName, ColumnName]) -->
    ( ['linking']; ['connecting'] ), table(TableName), ['by', 'their'], col(ColumnName).

match_operation(Operation) -->
    ['such', 'that'], match_condition(Operation).

match_condition([matches, Values]) -->
    ['its', 'values', 'are', 'either'], values(Values).

match_condition([matches, ColumnName, Query]) -->
    col(ColumnName), ['matches', 'values', 'within', 'the'], col(MatchColName), ['in'], table(MatchTableName),
    ( where_operation(WhereOp) ->
        { Query = [command, [[MatchColName, MatchTableName]], WhereOp] }
    ; { Query = [command, [[MatchColName, MatchTableName]], []] }
    ).

where_operation([where, Condition]) -->
    ['where'], or_condition(Condition).

or_condition(Condition) -->
    ( ['either'], condition(Cond1), ['or'], condition(Cond2) ->
        { Condition = [or, Cond1, Cond2] }
    ; condition(Cond1),
      ( ['and'], or_condition(Cond2) ->
          { Condition = [and, Cond1, Cond2] }
      ; { Condition = Cond1 }
      )
    ).

condition([condition, ColumnName, Equality, Value]) -->
    col(ColumnName), equality(Equality), val(Value).

equality('=') --> ['equals'].
equality('<') --> ['is', 'less', 'than'].
equality('>') --> ['is', 'greater', 'than'].

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

% Evaluating logical conditions and fetching data
evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    process_table_column_info(TableColumnInfo, ProcessedTableColumnInfo),
    get_tables_data(ProcessedTableColumnInfo, Conditions, FilteredTable).

process_table_column_info([], []).

process_table_column_info([[all, TableName]|Rest], [[TableName, all]|RestProcessed]) :-
    process_table_column_info(Rest, RestProcessed).

process_table_column_info([[Columns, TableName]|Rest], [[TableName, Columns]|RestProcessed]) :-
    Columns \= all,
    process_table_column_info(Rest, RestProcessed).

get_tables_data([], _, []).

get_tables_data([[TableName, Columns]|Rest], Conditions, [[TableName, ColumnHeaders, Rows]|RestTables]) :-
    get_table_columns(TableName, Columns, ColumnHeaders),
    table(TableName, AllColumns),
    get_table_rows(TableName, Columns, Conditions, AllColumns, Rows),
    get_tables_data(Rest, Conditions, RestTables).

get_table_columns(TableName, all, ColumnHeaders) :-
    table(TableName, ColumnHeaders).

get_table_columns(_TableName, Columns, ColumnHeaders) :-
    Columns \= all,
    ColumnHeaders = Columns.

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
        Rows).

select_columns(RowValuesAll, all, _AllColumns, RowValuesAll).

select_columns(RowValuesAll, Columns, AllColumns, RowValuesSelected) :-
    Columns \= all,
    find_column_indices(Columns, AllColumns, Indices),
    extract_values_at_indices(RowValuesAll, Indices, RowValuesSelected).

find_column_indices([], _, []).
find_column_indices([Col|RestCols], AllColumns, [Index|RestIndices]) :-
    nth0(Index, AllColumns, Col),
    find_column_indices(RestCols, AllColumns, RestIndices).

extract_values_at_indices(RowValuesAll, Indices, RowValuesSelected) :-
    maplist(nth0_from_list(RowValuesAll), Indices, RowValuesSelected).

nth0_from_list(List, Index, Element) :-
    nth0(Index, List, Element).

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

% Comparison logic with strict type checks
compare_values(CellValue, '=', Value) :-
    compare_values_eq(CellValue, Value).

compare_values(CellValue, '<', Value) :-
    compare_values_lt(CellValue, Value).

compare_values(CellValue, '>', Value) :-
    compare_values_gt(CellValue, Value).

% Helper predicates for type checking
both_numbers(Val1, Val2, Num1, Num2) :-
    attempt_number(Val1, Num1),
    attempt_number(Val2, Num2).

both_dates(Val1, Val2, Date1, Date2) :-
    attempt_date(Val1, Date1),
    attempt_date(Val2, Date2).

both_strings(Val1, Val2) :-
    atom(Val1), atom(Val2).

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

attempt_number(Value, NumValue) :-
    ( number(Value) ->
        NumValue = Value
    ; atom(Value),
      atom_number(Value, NumValue)
    ), !.

attempt_date(Value, DateValue) :-
    ( is_date(Value, DateValue) ->
        true
    ; atom(Value),
      is_date(Value, DateValue)
    ), !.

% Convert a date(Y,M,D) to a single integer YYYYMMDD for numeric comparison
date_to_number(date(Y,M,D), NumDate) :-
    NumDate is Y*10000 + M*100 + D.

date_compare(Operator, date(Y1,M1,D1), date(Y2,M2,D2)) :-
    date_to_number(date(Y1,M1,D1), N1),
    date_to_number(date(Y2,M2,D2), N2),
    ( Operator = '<', N1 < N2
    ; Operator = '>', N1 > N2
    ; Operator = '=', N1 =:= N2
    ).
