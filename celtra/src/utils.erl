%%
%% This module contains common utilities.
%%
-module(utils).

-export([timestamp/0]).

%%
%% Returns UNIX timestamp by taking the number of gregorian seconds
%% and subtracting the unix time in gregorian seconds (62167219200).
%%
timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(os:timestamp()),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}) - 62167219200.
