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
  Pid = spawn(?MODULE, init, []),
  Status = is_process_alive(Pid),
  io:format("Status is ~p" , [Status]),

  Pid ! {"Good boy"},
  io:format("Status is ~p" , [is_process_alive(Pid)]),
  io:format("Status is ~p" , [is_process_alive(Pid)]),
  io:format("Status is ~p" , [is_process_alive(Pid)]),
  io:format("Status is ~p" , [is_process_alive(Pid)]),
  io:format("Status is ~p" , [is_process_alive(Pid)]).



