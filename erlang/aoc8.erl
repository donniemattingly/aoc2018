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
    io:format(Name ++ ": \n"),
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

parseNode([0 | [MetaDataCount | Data]]) ->
    {MetaData, NewData} = lists:split(MetaDataCount, Data),
    {lists:sum(MetaData), NewData};

parseNode([ChildCount | [MetaDataCount | Data]]) ->
    {Total, NewData} = lists:foldl(
      fun(_, {RunningTotal, RunningData}) ->
              {ChildSum, NewData} = parseNode(RunningData),
              {ChildSum + RunningTotal, NewData}
      end, {0, Data}, lists:seq(0, ChildCount - 1)),
    {MetaData, NewerData} = lists:split(MetaDataCount, NewData),
    {lists:sum(MetaData) + Total, NewerData}.

partone() ->
    io:format("Advent of Code Day 8~n"),
    Input = realInput(),
    Result = parseNode(Input),
    io:format("Result:\n"),
    io:write(Result).

