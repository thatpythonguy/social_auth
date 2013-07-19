%%%-------------------------------------------------------------------
%%% @author Ahmed Al-Saadi <thaterlangguy@gmail.com>
%%% @doc OAuth Client
%%%  Based on code from Tim Fletcher's example OAuth client code
%%% @end
%%%-------------------------------------------------------------------

-module(sa_server).

-behaviour(gen_server).

-export([access_token_params/1, 
        deauthorize/1,
        get/2, get/3, get/4, 
        get_access_token/2, get_access_token/3, get_access_token/4, 
        get_request_token/2, get_request_token/3, get_request_token/4, 
        start/1, 
        start_link/2, 
        stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {consumer, request_params=undefined, access_params=undefined}).

%%============================================================================
%% API functions
%%============================================================================

start(Initial) ->
    Params = case Initial of 
                {Consumer, AParams} -> {Consumer, AParams};
                Consumer -> {Consumer, undefined}
            end,
    gen_server:start(?MODULE, Params, []).

%start(ServerName, Consumer) ->
%  gen_server:start(ServerName, ?MODULE, Consumer, []).

start_link(Consumer, AParams) ->
    gen_server:start_link(?MODULE, {Consumer, AParams}, []).

%start_link(ServerName, Consumer) ->
%  gen_server:start_link(ServerName, ?MODULE, Consumer, []).

get_request_token(Client, URL) ->
  get_request_token(Client, URL, [], header).

get_request_token(Client, URL, Params) ->
  gen_server:call(Client, {get_request_token, URL, Params, header}).

get_request_token(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get_request_token, URL, Params, ParamsMethod}).

get_access_token(Client, URL) ->
  get_access_token(Client, URL, [], header).

get_access_token(Client, URL, Params) ->
  gen_server:call(Client, {get_access_token, URL, Params, header}).

get_access_token(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get_access_token, URL, Params, ParamsMethod}).

get(Client, URL) ->
  get(Client, URL, [], header).

get(Client, URL, Params) ->
  gen_server:call(Client, {get, URL, Params, header}).

get(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get, URL, Params, ParamsMethod}).

access_token_params(Client) ->
  gen_server:call(Client, access_token_params).

deauthorize(Client) ->
  gen_server:cast(Client, deauthorize).

stop(Client) ->
  gen_server:cast(Client, stop).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init({Consumer, AParams}) ->
    State = #state{ consumer = Consumer,
                access_params = AParams},

    {ok, State}.

handle_call({get_request_token, URL, Params, ParamsMethod}, _From, #state{consumer=Consumer}=State) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, "", "") of
    {ok, Response={{_, 200, _}, _, _}} ->
      RParams = oauth:params_decode(Response),
      {reply, {ok, oauth:token(RParams)}, State#state{request_params=RParams}};
    {ok, Response} ->
      {reply, Response, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({get_access_token, URL, Params, ParamsMethod}, _From, #state{consumer=Consumer, request_params=RParams}=State) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, oauth:token(RParams), oauth:token_secret(RParams)) of
    {ok, Response={{_, 200, _}, _, _}} ->
      AParams = oauth:params_decode(Response),
      {reply, {ok, oauth:token(AParams)}, State#state{access_params=AParams}};
    {ok, Response} ->
      {reply, Response, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({get, URL, Params, ParamsMethod}, _From, #state{consumer=Consumer, access_params=AParams}=State) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, oauth:token(AParams), oauth:token_secret(AParams)) of
    {ok, {{_, 200, _}, Headers, Body}} ->
      case proplists:get_value("content-type", Headers) of
        undefined ->
          {reply, {ok, Headers, Body}, State};
        ContentType ->
          MediaType = hd(string:tokens(ContentType, ";")),
          case lists:suffix("/xml", MediaType) orelse lists:suffix("+xml", MediaType) of
            true ->
              {XML, []} = xmerl_scan:string(Body),
              {reply, {ok, Headers, XML}, State};
            false ->
              {reply, {ok, Headers, Body}, State}
          end
      end;
    {ok, Response} ->
      {reply, Response, State};
    Error ->
      {reply, Error, State}
  end;
handle_call(access_token_params, _From, State) ->
  {reply, State#state.access_params, State}.

handle_cast(deauthorize, #state{consumer=Consumer}) ->
  {noreply, #state{consumer=Consumer}};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.

%%============================================================================
%% Helper functions
%%============================================================================

oauth_get(header, URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("GET", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]},
  httpc:request(get, Request, [{autoredirect, false}], []);
oauth_get(querystring, URL, Params, Consumer, Token, TokenSecret) ->
  oauth:get(URL, Params, Consumer, Token, TokenSecret).

