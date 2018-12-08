-module(aoc8).

-export([partone/0]).
-export([testInput/0]).

-define(SimpleTestInput, [1, 1, 0, 1, 99, 2]).

testInput() ->
    getInput("../inputs/input-8.0.txt").

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

parseNode(All = [ChildCount | [MetaDataCount | Tail]]) ->
    log(All, "Parsing"),
    parseNode(ChildCount, MetaDataCount, {[], [], Tail}).

parseNode(0, MetaDataCount, Res = {ParsedChildren, MetaData, Remaining}) ->
    log({0, MetaDataCount, Res}, "ChildCount == 0"),
    {FinalMetaData, NewRemaining} = lists:split(MetaDataCount, Remaining),
    {[], FinalMetaData, NewRemaining};

parseNode(ChildCount, MetaDataCount, Res = {ParsedChildren, MetaData, Remaining}) 
  when length(ParsedChildren) == ChildCount->
    log({ChildCount, MetaDataCount, Res}, "ChildCount == ParsedChildren Length"),
    {MetaData, Remaining} = lists:split(length(Remaining) - MetaDataCount, Remaining),
    {ParsedChildren, MetaData, Remaining};

parseNode(ChildCount, MetaDataCount, Res = {ParsedChildren, MetaData, Remaining}) ->
    log({ChildCount, MetaDataCount, Res}, "No Guards/Matches"),
    {AdditionalChildren, NewMetaData, NewRemaining} = parseNode(Remaining),
    {ParsedChildren ++ AdditionalChildren, NewMetaData, NewRemaining}.


partone() ->
    io:format("Advent of Code Day 8~n"),
    Input = testInput(),
    Result = parseNode(?SimpleTestInput),
    io:format("Result:\n"),
    io:write(Result).

