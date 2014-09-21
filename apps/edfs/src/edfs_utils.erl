%%%-------------------------------------------------------------------
%%% @author Aman Mangal <mangalaman93@gmail.com>
%%% @copyright (C) 2014, Aman Mangal
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2014 by Aman Mangal <mangalaman93@gmail.com>
%%%-------------------------------------------------------------------
-module(edfs_utils).

-include("edfs.hrl").

-export([gen_chunk_id/0,
         gen_sec_chunk_id/0,
         priv_dir/0]).

%% ====================================================================
%% API functions
%% ====================================================================

% @doc generate unique random number (not secure) of length 64 bit.
% It can generate unique numbers until year 2170 (for 200 years from
% the time when cpu counter began counting, currently 1970)
-spec gen_chunk_id() -> string().
gen_chunk_id() ->
    gen_chunk_id(48, curr_time_millis(), []).
gen_chunk_id(0, 0, Acc) ->
    lists:reverse(Acc);
gen_chunk_id(Len, Num, Acc) ->
    RestLength = Len - 6,
    << Pos:6, RestNum:RestLength >> = << Num:Len >>,
    gen_chunk_id(RestLength, RestNum, [lists:nth(Pos+1, ?ALLOWED_CHARS)|Acc]).

% @doc Generate unique <b>secure</b> random number of length 128 bit.
% It can generate unique numbers until year 2170 (for 200 years from
% the time when cpu counter began counting, currently 1970)
-spec gen_sec_chunk_id() -> string().
gen_sec_chunk_id() ->
    gen_chunk_id(48, curr_time_millis(), gen_rand_str(8)).


%% ====================================================================
%% Internal functions
%% ====================================================================

% @doc Generate random strings of given length
gen_rand_str(Len) ->
    gen_rand_str(Len, []).
gen_rand_str(0, Acc) ->
    Acc;
gen_rand_str(Len, Acc) ->
    Char = lists:nth(random:uniform(?LEN_AC), ?ALLOWED_CHARS),
    gen_rand_str(Len-1, [Char|Acc]).

% @doc Returns time in millisec
curr_time_millis() ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    1000000000000*MegaSec + Sec*1000000 + MicroSec.

%% @doc Returns path to priv_dir
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.
