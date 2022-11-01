-type proplist() :: proplists:proplist().

-export_type([account/0, general_settings/0]).

-type general_settings() :: #{
    domain => string(),
    listen_port => integer()
}.

-record(general_settings, {
    listen_port=8082
}).

%%
%% The message queue size. I save maximum 20 messages in buffer
%% New messages are pushed into the back and drop off the front.
%%
-define(QUEUE_SIZE, 20).

-type account() :: #{
    id          => string(),
    name	=> string(),
    active	=> boolean(),
    modified	=> integer()
}.

-record(pubsub_account, {
    id          = ""::string(),
    name        = ""::string(),
    active	= true::boolean(),
    modified	= 0::integer()
}).

-type pubsub_service() :: #{
    pid          => pid()
}.

-record(pubsub_service, {
    key, value
}).
