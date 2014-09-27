%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli.ypa@gmail.com>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc Rebar plugin for executing tasks such as config generation
%%%
%%% @end
%%% Created : 20 Sep 2014 by Mawuli Adzaku <mawuli.ypa@gmail.com>
%%%-------------------------------------------------------------------
-module(edfs_rebar_plugin).

-export([
    parse_config/2,
    config_gen/2
]).

%% @doc Parses the edfs.conf and generates Erlang app.config
parse_config(Config, _Reltoolfile) ->
    SchemaFile = rebar_config:get_local(Config, edfs_schema,"edfs.schema"),
    ConfigFile = rebar_config:get_local(Config, edfs_conf, "edfs.conf"),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = cuttlefish_conf:files([ConfigFile]),

    %% now parse the conf file
    NewConfig = case cuttlefish_generator:map(Schema, Conf) of
        {error, Phase, {error, Errors}} ->
            lager:error("Error generating configuration in phase ~s", [Phase]),
            _ = [ cuttlefish_error:print(E) || E <- Errors],
            init:stop(1);
        ValidConfig -> ValidConfig
    end,

    Destination  = rebar_config:get_local(Config, edfs_app_configpath,"edfs.sys.config"),
    case file:write_file(Destination, io_lib:fwrite("~p.\n", [NewConfig])) of
        ok ->
            io:format("==> sys config written to ~p ~n", [Destination]);
        Error ->
            io:format("==> Could not parse configuration file: ~p ~n", [Error]),
            ok
    end.
    
%% @doc Run this via rebar (rebar config_gen) to generate the .conf files on ./priv
config_gen(Config, _Reltoolfile) ->
    generate_config_files(Config).

%% @doc Main configuration files generator. Translates the Schema files to .conf
%% files
generate_config_files(Config) ->       
    io:format("==> generating eDFS config file ~n", []),
    EDFS_schema_file = rebar_config:get_local(Config, edfs_schema,"edfs.schema"),
    EDFS_config_file = rebar_config:get_local(Config, edfs_conf, "edfs.conf"),
    generate_conf(EDFS_schema_file, EDFS_config_file),

    io:format("==> generating Erlang VM config file ~n", []),
    EDFS_vmschema_file = rebar_config:get_local(Config, edfs_vmschema, "erlang_vm.schema"),
    EDFS_vmconfig_file = rebar_config:get_local(Config, edfs_vmconf, "erlang_vm.conf"),
    generate_conf(EDFS_vmschema_file, EDFS_vmconfig_file),
    
    io:format("==> finished generating config files ~n", []),
    ok.
    
%% @doc Generate .conf files form the .schema files
-spec generate_conf(SchemaFile, ConfigFile) -> error | ok when
      SchemaFile :: file:filename(),
      ConfigFile :: file:filename().
generate_conf(SchemaFile, ConfigFile) ->
    case cuttlefish_schema:files([SchemaFile]) of
        {error, _Es} ->
            %% These errors were already printed
            error;
        {_Translations, Mappings, _Validators} = _Schema ->
            cuttlefish_conf:generate_file(Mappings, ConfigFile),
            ok
    end.

  
