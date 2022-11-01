%%
%% This asyncronous process broadcasts messages to websocket workers.
%%
-module(events_server).
-behaviour(gen_server).

%% API

-export([start_link/0, start/0, stop/0]).
-export([new_service/0, send_message/1]).

%% Behaviour callbacks.
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include("general.hrl").

%% The following defines server state
-record(state, {buf=new_buf(), services=[]}).

%% Management API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%
%% Registers a new service ( subscriber ).
%%
new_service() ->
    gen_server:call(?MODULE, {new_service, self()}).

%%
%% Send message to gen_server asyncronously.
%%
-spec send_message(binary()) -> ok.

send_message(Data) when erlang:is_binary(Data) ->
    gen_server:cast(?MODULE, {message, self(), Data}).

%% Callbacks.

init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{buf=new_buf(), services=[]}}.

terminate(_Reason, _State) -> ok.

%%
%% Stop the server
%%
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
%%
%% Registers a new service syncronously.
%%
handle_call({new_service, Pid}, _, State) ->
    link(Pid),
    add_service(Pid),
    send_msgs(Pid, State#state.buf),
    {reply, ok, State};
%% Other messages
handle_call(_Other, _, State) ->
    {reply, {error, request}, State}.

%%
%% Receives account info and sends it to websocket asyncronously.
%%
handle_cast({message, _Pid, Msg}, State) ->
    broadcast_msg(Msg),
    Buf = buffer_msg(Msg, State#state.buf),
    {noreply, State#state{buf=Buf}};
handle_cast(_, State) ->
    {noreply, State}.

%% Process has died
handle_info({'EXIT', Pid, _}, State) ->
    remove_service(Pid),
    {noreply, State};
%% Ignore unknown messages
handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%
%% The buffer is {Count, Back, Front}.
%%
new_buf() -> {0, [], []}.

get_msgs({_, B, F}) -> F ++ lists:reverse(B).

buffer_msg(Msg, {C, B, F}) when C < ?QUEUE_SIZE ->
    {C+1, [Msg|B], F};
buffer_msg(Msg, {?QUEUE_SIZE, B, [_|F]}) -> {?QUEUE_SIZE, [Msg|B], F};
buffer_msg(Msg, {?QUEUE_SIZE, B, []}) ->
    buffer_msg(Msg, {?QUEUE_SIZE, [], lists:reverse(B)}).

broadcast_msg(Msg) ->
    Services0 = pubsub_db:get_services(),
    [send_msg(S#pubsub_service.key, Msg) || S <- Services0, S#pubsub_service.value =:= websocket].

send_msgs(Pid, Buf) ->
    lists:foreach(fun (M) -> send_msg(Pid, M) end, get_msgs(Buf)).

send_msg(Pid, Msg) ->
    Pid ! {events_server, Msg}.

add_service(Pid) ->
    pubsub_db:create_service(Pid),
    pubsub_db:get_services().

%%
%% When client closes websocket connection, PID should be removed
%%
%% TODO
%%
remove_service(Pid) -> Pid.
