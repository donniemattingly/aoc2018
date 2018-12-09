-module(aoc8).

-export([partone/0]).
-export([testInput/0]).

-define(SimpleTestInput, [1, 1, 0, 1, 99, 2]).

testInput() ->
    getInput("../inputs/input-8.0.txt").

realInput() ->
    getInput("../inputs/input-8.1.txt").

log(Value) ->
    log(Value, "").

log(Value, Name) ->
    io:format(Name ++ ": "),
    io:write(Value),
    io:format("\n").

getInput(FileName) ->
    {ok, Result} = file:read_file(FileName),
    RawInput = binary_to_list(Result),
    parseInput(RawInput).

parseInput(Input) ->
    Trimmed = string:trim(Input),
    IntStrings = string:split(Trimmed, " ", all),
    Ints = lists:map(fun(X) -> {Int, _ } = string:to_integer(X), Int end, IntStrings),
    Ints.

parseNode1([0 | [MetaDataCount | Data]]) ->
    {MetaData, NewData} = lists:split(MetaDataCount, Data),
    {lists:sum(MetaData), NewData};

parseNode1([ChildCount | [MetaDataCount | Data]]) ->
    {Total, NewData} = lists:foldl(
                         fun(_, {RunningTotal, RunningData}) ->
                                 {ChildSum, NewData} = parseNode1(RunningData),
                                 {ChildSum + RunningTotal, NewData}
                         end, {0, Data}, lists:seq(0, ChildCount - 1)),
    {MetaData, NewerData} = lists:split(MetaDataCount, NewData),
    {lists:sum(MetaData) + Total, NewerData}.

parseNode([0 | [MetaDataCount | Data]]) ->
    {MetaData, NewData} = lists:split(MetaDataCount, Data),
    {lists:sum(MetaData), NewData};

parseNode([ChildCount | [MetaDataCount | Data]]) ->
    {ReversedTotals, NewData} = lists:foldl(
                          fun(_, {Totals, RunningData}) ->
                                  {ChildSum, NewData} = parseNode(RunningData),
                                  {[ChildSum | Totals], NewData}
                          end, {[], Data}, lists:seq(0, ChildCount - 1)),
    Totals = lists:reverse(ReversedTotals),
    {MetaData, NewerData} = lists:split(MetaDataCount, NewData),
    log(MetaData, "MetaData"),
    log(Totals, "Totals"),
    Sum = lists:foldl(
            fun(MetaDatum, Sum) -> 
                    log(MetaDatum, "Datum"),
                    log(Sum, "Sum"),
                    if
                        MetaDatum > length(Totals) ->
                            Sum;
                        true ->
                            lists:nth(MetaDatum, Totals) + Sum
                    end
            end,
            0, MetaData),
    {Sum, NewerData}.

partone() ->
    io:format("Advent of Code Day 8~n"),
    Input = realInput(),
    Result = parseNode(Input),
    io:format("Result:\n"),
    io:write(Result).

