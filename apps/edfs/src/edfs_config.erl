%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli.ypa@gmail.com>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2014 by Mawuli Adzaku <mawuli.ypa@gmail.com>
%%%-------------------------------------------------------------------
-module(edfs_config).
-include("edfs.hrl").

%% API
-export([
    get/1,
    is_user_allowed/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%% @doc Get value from config file. The config data is parsed into sys.config
%% during startup. If not found, the response is {error, unknown_config}, and this is
%% because the edfs.conf file is populated with default values when the file is generated.
%% So, if the key is not found, it is highly likley that config param is not supported or unknown.
get(tcp_listener) ->
    Listener = ?MODULE:get(listener),
    proplists:get_value(tcp, Listener);

get(ssl_listener) ->
    Listener = ?MODULE:get(listener),
    proplists:get_value(ssl, Listener);

get({auth, {Username, Password}}) ->
    case ?MODULE:get(auth) of
        userlist ->
            UserList = ?MODULE:get(userlist),
            case proplists:get_value(Username, UserList) of
                Password ->
                    true;
                _Error ->
                    false
            end;
        off ->
            true
    end;

get(Key) ->
    case application:get_env(?APP_NAME, Key) of
        undefined ->
            {error, unknown_config};
        {ok, Value} ->
            Value
    end.

%% @doc Checks if username and password is in the user list.
-spec is_user_allowed(Username, Password) -> true | false when
      Username :: string(),
      Password :: string().
is_user_allowed(Username, Password) ->
    ?MODULE:get({auth, {Username, Password}}).
