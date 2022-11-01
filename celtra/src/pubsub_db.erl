%%
%% Mnesia DB setup
%%
-module(pubsub_db).

-include_lib("stdlib/include/qlc.hrl").

-include("general.hrl").
-include("log.hrl").

%% Employee crud functions
-export([mnesia_setup/0, get_account/1, create_account/3, create_service/1,
	 get_services/0]).

%%
%% Starts mnesia, creates persistent table, populates it with data.
%%
mnesia_setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(pubsub_account,
	[{type, set},
	 {disc_only_copies, [node()]},  %%  Disc only, and as such the storage is limited to DETS' 2GB limit.
	 {attributes, record_info(fields, pubsub_account)}]),

    mnesia:create_table(pubsub_service,
	[{type, set},
	 {disc_only_copies, [node()]}, %% Stored in RAM, copied to disc
	 {attributes, record_info(fields, pubsub_service)}]),

    create_account("celtra", "Celtra", true),
    create_account("test", "Test", false),
    mnesia:dirty_write(#pubsub_service{key=self(), value=db}),
    ok.

%%
%% Retrieves account from mnesia DB
%%
-spec get_account(list()) -> account().

get_account(Id0) when erlang:is_list(Id0) ->
    F = fun() ->
    	    case mnesia:read({pubsub_account, Id0}) of
		[Item] -> Item;
		[] -> []
	    end
	end,
    case mnesia:activity(transaction, F) of
	[] -> undefined;
	Account -> Account
    end.

%%
%% Writes account to mnesia DB
%%
-spec create_account(list(), list(), list()) -> {ok, account()}.

create_account(Id, Name, Active)
	when erlang:is_list(Id), erlang:is_list(Name), erlang:is_boolean(Active) ->
    Timestamp = utils:timestamp(),

    F = fun() ->
    	    New = #pubsub_account{
		id = Id,
		name = Name,
		active = Active,
		modified = Timestamp
	    },
	    mnesia:write(New),
	    New
	end,
    mnesia:activity(transaction, F).

%%
%% Writes service PID to mnesia DB
%%
-spec create_service(pid()) -> {ok, pubsub_service()}.

create_service(Pid) when erlang:is_pid(Pid) ->
    F = fun() ->
    	    New = #pubsub_service{
		key = Pid,
		value = websocket
	    },
	    mnesia:write(New),
	    New
	end,
    mnesia:activity(transaction, F).

%%
%% Returns registered PIDs from mnesia DB
%%
get_services() ->
    F = fun() ->
	    mnesia:select(pubsub_service,[{'_',[],['$_']}])
	end,
    mnesia:activity(transaction, F).
