-module(gol).
-compile(export_all).

lives(2,true) ->
  false;
lives(3,_) ->
  true;
lives(_,_) ->
  false.

intersect_lists(A,B) ->
  sets:to_list(sets:intersection(sets:from_list(A), sets:from_list(B))).

neighbors(Xin,Yin, World) ->
  Ns = [{X+Xin, Y+Yin, true} || X <- [-1,0,1], Y <- [-1,0,1] ],
  length(intersect_lists(World, Ns))
    - length([ true || {A,B,true} <- World, A == Xin, B == Yin ]).

transmute_world(World) ->
  [ {X, Y, gol:lives(gol:neighbors(X,Y, World), Alive)} || {X,Y,Alive} <- World ].