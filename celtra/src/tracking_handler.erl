%%
%% Receives POST request and forwards account information to services 
%% over websocket connections.
%%
-module(tracking_handler).
-behavior(cowboy_handler).

-export([init/2, content_types_provided/2, to_json/2,
    allowed_methods/2, resource_exists/2, previously_existed/2,
    content_types_accepted/2, handle_post/2]).

-include("general.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

%%
%% Called first
%%
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
	{{<<"application">>, <<"json">>, '*'}, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    case cowboy_req:method(Req) of
	<<"POST">> ->
	    {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}
    end.

handle_post(Req0, State0) ->
    Account = proplists:get_value(account, State0),
    case Account#pubsub_account.active of
	false ->
	    Req1 = cowboy_req:reply(400, #{
		<<"content-type">> => <<"application/json">>
	    }, jsx:encode([{error, <<"Account is inactive">>}]), Req0),
	    {ok, Req1, []};
	true ->
	    {ok, Body, Req1} = cowboy_req:read_body(Req0),
	    FieldValues = jsx:decode(Body),
	    Data = proplists:get_value(<<"data">>, FieldValues),

	    Timestamp = utils:timestamp(),
	    Msg = jsx:encode([
		{account_id, erlang:list_to_binary(Account#pubsub_account.id)},
		{timestamp, Timestamp},
		{data, Data}
	    ]),
	    events_server:send_message(Msg),
	    {true, Req1, []}
    end.

to_json(Req0, _State) ->
    {jsx:encode([]), Req0, []}.

%%
%% Validates request parameters
%% ( called after 'content_types_provided()' )
%%
resource_exists(Req0, _State) ->
    AccountId = erlang:binary_to_list(cowboy_req:binding(account_id, Req0)),
    %% Checks if account exists
    case pubsub_db:get_account(AccountId) of
	undefined -> {false, Req0, []};
	Account -> {true, Req0, [{account, Account}]}
    end.

previously_existed(Req0, _State) ->
    {false, Req0, []}.
