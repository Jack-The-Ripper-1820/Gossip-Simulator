%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2022 3:15 AM
%%%-------------------------------------------------------------------
-module(push_sum).
-author("akhil").

%% API
-export([spawn_actors/0]).


spawn_actors()->
    receive {Message} ->
        io:format("Hi ~p", [Message])
    end.
