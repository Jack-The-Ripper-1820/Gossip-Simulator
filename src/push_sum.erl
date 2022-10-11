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
-export([spawn_actors/3, pass_message_to_neighbours / 0, index_of/2, push_sum_supervisor/ 3]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


spawn_actors(S, W, Counter)->
    receive {"Push_Sum", Index, NeighbourList, ActorList, Parent, Received_S, Received_W} ->

        if
           Counter > 3 ->

               io:format("Convergence Achieved for ~p ~n", [self()]),
               convergence ! {"Actor Finished Work", self()},
               pass_message_to_neighbours ! {"Can't acknowledge any more messages", "Dead", Parent, ActorList, NeighbourList, self(), Received_S, Received_W},
               exit(normal);
            true ->
                Old_Ratio = S / W,
                New_S = S + Received_S,
                New_W = W + Received_W,
                New_Ratio = New_S / New_W,

                Diff = abs(New_Ratio - Old_Ratio),
                Pow = math:pow(10, -10),

                if
                    Diff < Pow ->
                        NewCounter = Counter + 1;
                    true -> NewCounter = 0
                end,
                pass_message_to_neighbours ! {Index, NeighbourList, ActorList, self(), New_S / 2, New_W / 2},
                spawn_actors(New_S / 2, New_W / 2, NewCounter)
        end

    end.

pass_message_to_neighbours() ->
    receive
        {Index, NeighbourList, ActorList, PID, S, W} ->

            Neighbours = lists:nth(Index, NeighbourList),
            if length(Neighbours) == 0 ->
                push_sum_supervisor ! {"No_Neighbours_Found", "Help_For_Convergence",PID, S,W},
                pass_message_to_neighbours();

                true ->
                    Len = length(Neighbours),
                    Rand_index = rand:uniform(Len),
                    Neighbour_Pid = lists:nth(Rand_index, Neighbours),
                    Is_Alive = is_process_alive(Neighbour_Pid),
                    if
                        Is_Alive == false ->
                            Deleted_Neig = Neighbours -- [lists:nth(Rand_index, Neighbours)],
                            Updated_Neighbours = lists:sublist(NeighbourList, Index - 1) ++  [Deleted_Neig] ++ lists:nthtail(Index, NeighbourList),
                            self() ! {Index, Updated_Neighbours, ActorList, PID, S, W},
                            pass_message_to_neighbours();
                        true ->
                            Neighbour_Pid ! {"Push_Sum", index_of(Neighbour_Pid, ActorList), NeighbourList, ActorList, PID, S, W},
                            self() ! {Index, NeighbourList, ActorList, PID},
                            pass_message_to_neighbours()
                    end
            end;

        {"Can't acknowledge any more messages", State , PID, ActorList, NeighbourList, Neighbour_Pid, S, W} ->
            State, Neighbour_Pid,
            self() ! {index_of(PID, ActorList), NeighbourList, ActorList, PID, S, W},
            pass_message_to_neighbours()

    end.



push_sum_supervisor(Actor_List, Times_Called, NeighbourList) ->

    receive {"No_Neighbours_Found", "Help_For_Convergence",Process_Id, S,W } ->

        io:format("Supervisor Counter ~w ~n", [Times_Called + 1]),
        Rem_Actors = Actor_List -- [Process_Id],
        Alive_Actor = get_alive_actor(length(Rem_Actors), Rem_Actors),
        if
            Alive_Actor == no_alive_actor ->
                io:format("NO ALIVE ACTORS for ~p, CONVERGE THE ACTOR AND KILL THE PROGRAM ~n", [Process_Id]),
                convergence ! {"Actor Finished Work", Process_Id};
            true ->
                Ind  = index_of(Alive_Actor, Actor_List),
                io:format("Alive actor found ~p, and Index is ~p ~n", [Alive_Actor, Ind]),
                Alive_Actor !  {"Push_Sum", Ind, NeighbourList, Actor_List, Process_Id, S, W}
        end,

        push_sum_supervisor(Actor_List, Times_Called + 1, NeighbourList)
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

