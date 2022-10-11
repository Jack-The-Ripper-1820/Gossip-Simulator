%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2022 3:15 AM
%%%-------------------------------------------------------------------
-module(gossip).
-author("akhil").

%% API
-export([spawn_actors/2, pass_message_to_neighbours / 1, index_of/2, update_neighbours/ 2, get_alive_actor/ 2, gossip_supervisor / 3]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).



spawn_actors(Counter, Topology)->

  receive {"Gossip_Message", Index, NeighbourList, ActorList, Parent} ->
    Parent,
    if
      Counter == 0->
        io:format("Convergence achieved for ~p ~n", [self()]),
        convergence ! {"Actor Finished Work", self()},
        pass_message_to_neighbours ! {"Can't acknowledge any more messages", "Dead", Parent, ActorList, NeighbourList, self()},
        exit(normal);
      true ->
        pass_message_to_neighbours ! {Index, NeighbourList, ActorList, self()},
        spawn_actors(Counter - 1, Topology)
    end;
    {"KILL_CALL"} ->
      io:format("kill call triggered from Bonus Mode ~n"),
      convergence ! {"Actor Kill", self()},
      exit(kill)
  end.


pass_message_to_neighbours(Topology) ->
  receive
    {Index, NeighbourList, ActorList, PID} ->

      if Topology == "Full Network" ->
        Neighbours = ActorList -- [PID],

        Rand = rand:uniform(length(Neighbours)),
        Ele = lists:nth(Rand, Neighbours),
        Alive =  is_process_alive(Ele),
        if
          Alive -> X = Ele;
          true -> X  = get_alive_actor(length(Neighbours), Neighbours)
        end,
        if  X == no_alive_actor -> gossip_supervisor ! {"No_Neighbours_Found", "Help_For_Convergence",X};
          true -> X ! {"Gossip_Message", index_of(X, ActorList), NeighbourList, ActorList, PID}
        end;
        true ->  Neighbours = lists:nth(Index, NeighbourList)
      end,

      if length(Neighbours) == 0 ->
        io:format("No neighbours to communicate for ~p ~n", [PID]),
        gossip_supervisor ! {"No_Neighbours_Found", "Help_For_Convergence",PID};

        true ->
          Len = length(Neighbours),
          Rand_index = rand:uniform(Len),
          Neighbour_Pid = lists:nth(Rand_index, Neighbours),
          Is_Alive = is_process_alive(Neighbour_Pid),
          if
            Is_Alive == false ->
              Deleted_Neig = Neighbours -- [lists:nth(Rand_index, Neighbours)],
              Updated_Neighbours = lists:sublist(NeighbourList, Index - 1) ++  [Deleted_Neig] ++ lists:nthtail(Index, NeighbourList),
              self() ! {Index, Updated_Neighbours, ActorList, PID},
              pass_message_to_neighbours(Topology);
            true ->
              Neighbour_Pid ! {"Gossip_Message", index_of(Neighbour_Pid, ActorList), NeighbourList, ActorList, PID},
              self() ! {Index, NeighbourList, ActorList, PID},
              pass_message_to_neighbours(Topology)
          end,
          pass_message_to_neighbours(Topology)
      end;

    {"Can't acknowledge any more messages", State , PID, ActorList, NeighbourList, Neighbour_Pid} ->
      State, Neighbour_Pid,
      self() ! {index_of(PID, ActorList), NeighbourList, ActorList, PID},
      pass_message_to_neighbours(Topology)

  end.

update_neighbours(0, NeighbourList) ->
  NeighbourList;


update_neighbours(Index, NeighbourList) ->

  Neighbour  =  lists:nth(Index, NeighbourList),
  IsNeighbourAlive = is_process_alive(Neighbour),
  if IsNeighbourAlive ->
    update_neighbours(Index - 1, NeighbourList);

    true->
      NewList = lists:delete(Index, NeighbourList),
      update_neighbours(length(NewList), NewList)
  end.


gossip_supervisor(Actor_List, Times_Called, NeighbourList) ->

  receive {"No_Neighbours_Found", "Help_For_Convergence",Process_Id } ->

    io:format("Supervisor Counter ~w ~n", [Times_Called + 1]),
    Rem_Actors = Actor_List -- [Process_Id],
    Alive_Actor = get_alive_actor(length(Rem_Actors), Rem_Actors),
    if
      Alive_Actor == no_alive_actor ->
        io:format("NO ALIVE ACTORS, CONVERGE THE ACTOR AND KILL THE PROGRAM ~n"),
        convergence ! {"Actor Finished Work", Process_Id};
      true ->
        Ind  = index_of(Alive_Actor, Actor_List),
        io:format("Alive actor found ~p, and Index is ~p", [Alive_Actor, Ind]),
        Alive_Actor !  {"Gossip_Message", Ind, NeighbourList, Actor_List, Process_Id}
    end,

    gossip_supervisor(Actor_List, Times_Called + 1, NeighbourList)
  end.


get_alive_actor(0, Actor_List) ->
  Actor_List,
  no_alive_actor;

get_alive_actor(Len, Actor_List) ->


  Actor = lists:nth(Len, Actor_List),
  Is_Actor_Alive = is_process_alive(Actor),
  if
    Is_Actor_Alive == true ->
      Actor;
    true -> get_alive_actor(Len - 1, Actor_List)
  end.



