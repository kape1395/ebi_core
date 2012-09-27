-module(bio_ers_queue_mgr_sup).
-behaviour(supervisor).
-export([init/1]).


init(_Args) ->
    {ok, {{one_for_all,1000,60},[
    ]}}.