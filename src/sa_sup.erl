-module(sa_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Consumer, AParams) ->
    supervisor:start_child(?SERVER, [Consumer, AParams]).

init([]) ->
    Element = {sa_server, {sa_server, start_link, []},
               transient, 2000, worker, [sa_server]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

