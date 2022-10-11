%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2022 6:40 PM
%%%-------------------------------------------------------------------
-module(test).
-author("akhil").

%% API
-export([start/0, init/0]).


init() ->
  receive
    {"Good boy"} -> io:format("Hey"),
      exit(normal)
  end.

start() ->
  Index = 3,
  Nei = [2, 3, [9, 15, 16] , 10, 11],
  Deleted = lists:nth(Index, Nei),
  Deleted_List = Deleted -- [15],
  Updated_List = lists:sublist(Nei, 2) ++ Deleted_List ++  lists:nthtail(Index, Nei),
  io:format("Updated List ~p ~n", [Updated_List]).



  %Deleted_Neig = Nei -- [lists:nth(2, Nei)],
  %io:format("len is ~w ~n", [length(Deleted_Neig)]).



