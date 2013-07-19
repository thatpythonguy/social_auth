-module(social_auth).

-export([start/0, 
        stop/0, 
        insert/3, 
        insert/4, 
        lookup/2, 
        delete/2]).

start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    application:start(social_auth).

stop() ->
    Res = application:stop(social_auth),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(inets),
    Res.

lookup(Network, UserID) ->
    try
        sa_dir:lookup({Network, UserID})
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

insert(Network, UserID, Consumer) ->
    insert(Network, UserID, Consumer, undefined).
    
insert(Network, UserID, Consumer, AParams) ->
    case lookup(Network, UserID) of
        {ok, Pid} ->
            {ok, Pid};
        {error, _} ->
            {ok, Pid} = sa_sup:start_child(Consumer, AParams),
            sa_dir:insert({Network, UserID}, Pid),
            {ok, Pid}
    end.

delete(Network, UserID) ->
    case lookup(Network, UserID) of
        {ok, Pid} ->
            sa_dir:delete(Pid),
            sa_server:stop(Pid);
        {error, _Reason} ->
            ok
    end.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
