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
-export([spawn_actors/3, pass_message_to_neighbours / 0, index_of/2]).

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
            Len = length(Neighbours),
            Rand_index = rand:uniform(Len),
            Neighbour_Pid = lists:nth(Rand_index, Neighbours),
            Is_Alive = is_process_alive(Neighbour_Pid),
            if
                Is_Alive == false ->
                    io:format("Dead Neighbour ~p for PId  ~p ~n", [Neighbour_Pid, PID]),
                    self() ! {Index, NeighbourList, ActorList, PID, S, W},
                    pass_message_to_neighbours();
                true ->
                    io:format("Triggering"),
                    Neighbour_Pid ! {"Push_Sum", index_of(Neighbour_Pid, ActorList), NeighbourList, ActorList, PID, S, W},
                    self() ! {Index, NeighbourList, ActorList, PID, S, W},
                    pass_message_to_neighbours()
            end;

        {"Can't acknowledge any more messages", State , PID, ActorList, NeighbourList, Neighbour_Pid, S, W} ->
            State, Neighbour_Pid,
            %Is_Alive = is_process_alive(Neighbour_Pid),
            io:format("Reached ~n"),
            self() ! {index_of(PID, ActorList), NeighbourList, ActorList, PID, S, W},
            pass_message_to_neighbours()

    end.

