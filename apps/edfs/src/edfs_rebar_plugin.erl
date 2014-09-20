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
    config_gen/2
]).

config_gen(Config, _Reltoolfile) ->
    generate_config_files(Config).

generate_config_files(Config) ->       
    io:format("===> generating eDFS config file ~n", []),
    EDFS_schema_file = rebar_config:get_local(Config, edfs_schema,"edfs.schema"),
    EDFS_config_file = rebar_config:get_local(Config, edfs_conf, "edfs.conf"),
    generate_conf(EDFS_schema_file, EDFS_config_file),

    io:format("===> generating Erlang VM config file ~n", []),
    EDFS_vmschema_file = rebar_config:get_local(Config, edfs_vmschema, "erlang_vm.schema"),
    EDFS_vmconfig_file = rebar_config:get_local(Config, edfs_vmconf, "erlang_vm.conf"),
    generate_conf(EDFS_vmschema_file, EDFS_config_file),
    
    io:format("===> finished generating config files ~n"),
    ok.
    
%% @doc Generate .conf files form the .schema files        
generate_conf(SchemaFile, ConfigFile) ->
    case cuttlefish_schema:files([SchemaFile]) of
        {error, _Es} ->
            %% These errors were already printed
            error;
        {_Translations, Mappings, _Validators} ->
            cuttlefish_conf:generate_file(Mappings, ConfigFile),
            ok
    end.

  
