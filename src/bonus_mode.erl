%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 9:30 PM
%%%-------------------------------------------------------------------
-module(bonus_mode).
-author("akhil").

%% API
-export([kill_actors/0]).

kill_actors() ->
  receive {ActorList} ->
    io:format("Bonus Mode Triggered ~n~n"),
    Pick_Random_Index = rand:uniform(length(ActorList)),
    Alive_Process = lists:nth(Pick_Random_Index, ActorList),
    io:format("Bonus Mode --Killing Process ~p ~n", [Alive_Process]),
    Alive_Process ! {"KILL_CALL"},
    Is_alive = is_process_alive(Alive_Process),
    timer:sleep(500),
    io:format("Status of Process ~p is ~p ~n", [Alive_Process, Is_alive]),


    Rem_List = ActorList -- [Alive_Process],
    self() ! {Rem_List},
    kill_actors()
  end.