%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <UF>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2022 2:49 AM
%%%-------------------------------------------------------------------
-module(main).
-author("akhil").

%% API
-export([start/0, hi/0]).
-export([init_topology/3,  grid_view/2, grid_view/4]).

-define(GOSSIP_MESSAGE, "gossip_message").

hi()->
  io:format("Hi").

start() ->
  {ok, Actor} = {ok, 16},
  {ok, Topology} = {ok, "2D"},
  {ok, Algorithm} = io:read("Input Algo ~n"),
  io:format("Actors ~w, Topology ~p, Algorithms ~p ~n", [Actor, Topology, Algorithm]),

  if
    Algorithm  == "Gossip Algorithm"->
      ActorList = [spawn(gossip, spawn_actors, []) || _ <- lists:seq(1, Actor)];

    Algorithm == "Push Sum Algorithm" ->
      ActorList = [spawn(push_sum, spawn_actors, []) || _ <- lists:seq(1, Actor)];

    true -> ActorList =[]
  end,

  NeighbourList = init_topology(Actor, Topology, ActorList),
  io:format("Neighbour List is ~p~n and Actor List is ~p~n", [NeighbourList, ActorList]).


init_topology(Actors, Topology, ActorList) ->

  %ActorList = [spawn(push_sum, spawn_actors, []) || _ <- lists:seq(1, Actors)],
  %[ActorId ! {?GOSSIP_MESSAGE, ActorId }  ||  ActorId <- ActorList],
  Neighbour_List = create_neighbours(ActorList, Topology, Actors),
  Neighbour_List.

create_neighbours(ActorList, Topology, Actors) ->
  io:format("creating neighbours for actors ~p  and Topology is ~p ~n", [Actors, Topology]),
  if
    Topology == "Full Network" ->
      Neighbours = full_network(Actors, ActorList);
      %io:format("Full Network Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, ActorList]);

    Topology == "Line" ->
      Neighbours = line(Actors, ActorList);
      %io:format("Line Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, ActorList]);

    Topology == "2D" ->
      Neighbours = grid_view(Actors, ActorList);
      %io:format("2D Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, ActorList]);

    true -> Neighbours = []
  end,
  Neighbours.


%%%% Construct Neighbours for all topologies

%%% Full Network Neighbours

full_network(Actors, ActorList) ->
  full_network(Actors, ActorList, []).

full_network(0, ActorList, Neighbours) ->
  ActorList,
  lists:reverse(Neighbours);

full_network(Index, ActorList, Neighbours) ->
  ActorNeighbours = ActorList -- [lists:nth(Index, ActorList)],
  full_network(Index - 1, ActorList, lists:append(Neighbours, [ActorNeighbours])).

%%% Line Neighbours

line(Actors, ActorList) ->
  line(Actors, Actors, ActorList, []).

line(0, Actors, ActorList, Neighbours) ->
  ActorList, Actors,
  lists:reverse(Neighbours);

line(Index, Actors,  ActorList, Neighbours) ->
  if
    Index == 1 ->
      Next_Ele = lists:nth(Index + 1, ActorList),
      line(Index - 1, Actors, ActorList, lists:append(Neighbours, [[Next_Ele]]));

    Index == Actors ->
      Prev_Ele = lists:nth(Index - 1, ActorList),
      line(Index - 1, Actors, ActorList, lists:append(Neighbours, [[Prev_Ele]]));
    true ->
      Next_Ele = lists:nth(Index + 1, ActorList),
      Prev_Ele = lists:nth(Index - 1, ActorList),
      line(Index - 1, Actors, ActorList, lists:append(Neighbours, [[Prev_Ele, Next_Ele]]))
  end.


%%% Grid Representation
populate_grid(Idx, Rows, Actors, ActorList, RowEle, Matrix) ->

  if
    Idx > Actors -> Matrix;
    true ->
      Ele = lists:nth(Idx, ActorList),
      TempRow = lists:append(RowEle, [Ele]),
      if
        Idx rem Rows == 0 ->
          populate_grid(Idx + 1, Rows, Actors, ActorList, [], lists:append(Matrix, [TempRow]));
        true ->
          populate_grid(Idx + 1, Rows, Actors, ActorList, TempRow, Matrix)
      end
  end.

%%% Grid View

grid_view(Actors , ActorList) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  populate_grid(1, Rows, Actors, ActorList, [], []),
  %io:format("Grid Matrix is ~p ~n", [Grid_Matrix]),
  grid_view(Actors, Grid_Matrix, Rows, []).


grid_view(0, Grid_Matrix, Rows, Neighbours) ->
  Grid_Matrix, Rows,
  lists:reverse(Neighbours);

grid_view(Index, Grid_Matrix, Rows, Neighbours) ->

  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Ele_col = Rows,
      Ele_rows  = round(Index / Rows);
    true ->
      Ele_col = Index rem Rows,
      Ele_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  if
    Ele_rows == 1 ->
      if
        Ele_col == 1 ->
          N1= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N2= lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        Ele_col == Rows ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          N2= lists:nth(Ele_col - 1, lists:nth(Ele_rows , Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        true ->
          N1 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N3 = lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]))
      end;
    Ele_rows == Rows ->
      if
        Ele_col == 1 ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        Ele_col == Rows ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        true ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]))
      end;
    true ->
      if
        Ele_col == 1 ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]));
        Ele_col == Rows ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]));
        true ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          N4 = lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3, N4]]))
      end
  end.



