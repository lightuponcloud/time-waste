%%
%% Websocket handler pushes async messages from events_server
%% to subscribed services.
%%
-module(ws_handler).

%% Cowboy callbacks.
-export([init/2, websocket_handle/2, websocket_info/2, websocket_init/1]).

%% Internal state
-record(state, {opts=[]}).

init(Req, Opts) ->
    {cowboy_websocket,Req,#state{opts=Opts}}.

websocket_init(State) ->
    events_server:new_service(),
    {ok, State}.

%% Callback on received websockets data.

websocket_handle({text, _Data}, State) ->
    {ok, State};
websocket_handle(_, State) ->
    {ok, State}.

%% Callback on message from erlang.

websocket_info({events_server, Data}, State) when erlang:is_binary(Data) ->
    {reply, {binary, Data}, State};
websocket_info(_, State) ->
    {ok, State}.
