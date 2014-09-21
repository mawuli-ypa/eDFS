%%% @author Aman Mangal <mangalaman93@gmail.com>
%%% @author Mawuli Adzaku <mawuli.ypa@gmail.com>
%%% @copyright (C) 2014, Mawuli Adzaku, Aman Mangal
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2014 by Mawuli Adzaku <mawuli.ypa@gmail.com>

%%% DEFINITIONS
-define(APP_NAME, edfs).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).

% name of chunks, allowed Characters: 0..9, A..Z, a..z, ._ (64)
-define(ALLOWED_CHARS, [46|lists:seq(48, 57)] ++ lists:seq(65, 90) ++ [95|lists:seq(97, 122)]).
-define(LEN_AC, erlang:length(?ALLOWED_CHARS)).

% various parameters
-define(SHUTDOWNTIME, infinity).
-define(MAXR, 10).
-define(MAXT, 60).

% processes and gen_server
-define(EDFS_FILE_SERVER, edfs_file_server).
-define(EDFS_CHUNK_SERVER, edfs_chunk_server).
-define(EDFS_LOG_SERVER, edfs_log_server).
-define(EDFS_MAPREDUCE_SERVER, edfs_map_reduce).


% chunk settings
-define(CHUNK_SIZE, 32*1024*1024). % 32 MB
-define(OVERHEAD, 5/4).


%% Below is copied (and adapted) from Zotonic, which is copyright Zotonic

%%% LOGGING %%%
-define(DEBUG(Msg), edfs:debug_msg(?MODULE, ?LINE, Msg)).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(ERROR(Msg, Args), error_logger:error_msg("~p:~p "++Msg, [?MODULE, ?LINE|Args])).

-define(STDOUT(Str, Args), io:format(Str ++ "~n", Args)).
-define(FORMAT(Str, Args), io_lib:format(Str, Args)).

-define(STACKTRACE, erlang:display(try throw(a) of _ -> a catch _:_ -> erlang:get_stacktrace() end)).

%%% CONFIGURATIONS
-define(EDFS_SCHEMA_FILE, "edfs.schema").
-define(EDFS_CONFIG_FILE, "edfs.conf").

-define(EDFS_VMSCHEMA_FILE, "erlang_vm.schema").
-define(EDFS_VMCONFIG_FILE, "erlang_vm.args").

% other macros
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, ?SHUTDOWNTIME, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, ?SHUTDOWNTIME, Type, [I]}).

-define(PRIV_DIR,
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv");
        Dir ->
            Dir
    end).


%%% DATA TYPES
-type(proplists() :: [term()]).
-type uuid()     :: string().
-type nodeid() :: uuid().
-type filename() :: file:name().
-type filetype() :: regular | directory.
-type chunkid()  :: integer().
-type cluster()  :: undefined | uuid().
%% file permission
%%  r - read, w - write, x - execute (allowed to run operations/plugins on this file)
-type permission() :: r | w | x.


%%% RECORDS
%% links are Riak-style links for mapping relationships between files
-record(link, {tag :: string(), key :: uuid(), bucket :: uuid()}).

-record(file_metadata, {
          perms      :: permission(),
          uid        :: integer(),
          gid        :: integer(),
          atime      = undefined :: file:date_time(),
          mtime      = undefined :: file:date_time(),
          ctime      = undefined :: file:date_time(),
          %% link_count - how many files reference the file
          link_count = undefine :: integer(),
          %% readcount - number of open refs or how many clients are currently accessing this file
          read_count = undefined :: integer()
         }).

-record(file, {
          name      :: filename(),
          metadata  = #file_metadata{},
          size      = 0,
          repfactor = 3,
          % chunks list = [{chunk_id, size}] in the reverse order of their actual order in the file
          chunks    = []
         }).

-record(permission, {
          user   :: permission(),
          group  :: permission(),
          others :: permission()
         }).

% replicas = [{id, ip, port}] with first one being primary
-record(chunk, {
          id       :: integer(),
          filename :: filename(),
          size     :: undefined | integer(),
          replicas = []
         }).

-record(node_state, {
          cluster            :: cluster(),
          client_connections :: integer(),
          time_started       :: os:timestamp()
         }).

-record(node, {
          id         :: nodeid(),
          state      = #node_state{},
          ip         :: httpc:ip_address(),
          port       :: undefined | integer(),
          space_util = 0
         }).
