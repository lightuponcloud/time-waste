%%
%% This file contains all the URI routing configuration.
%%
-module(pubsub_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("general.hrl").

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
	{'_', [
	    {"/tracking/[:account_id]", tracking_handler, []},
	    {"/websocket", ws_handler, []}
	]}
    ]),
    Settings = #general_settings{},
    {ok, _} = cowboy:start_clear(pubsub_http_listener,
        [{port, Settings#general_settings.listen_port}],
        #{env => #{dispatch => Dispatch}}
    ),
    pubsub_sup:start_link().

stop(_State) -> ok.
