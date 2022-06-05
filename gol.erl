-module(gol).
-export([new/2, query/3, assign/4, print/1, draw/1, demo/0]).

% new: Neues Board erstellen
% query: Status einer Zelle abfragen
% assign: Status einer Zelle setzen
% print: Board ausgeben
% draw: Zellstatus formatieren
% step_cell: Einen Zelle weiterentwickeln
% simulation: Eine Generation weitergehen
% run: Mehrere Generationen weitergehen
% demo: Demo Board erstellen

-record(grid, {height=20, width=20, rows}).
-record(transition, {y, x, state}).

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _Y) -> 0.

new(H, W) ->
    C = array:new(H),
    A2D = array:map(fun(_X, _T) -> array:new([{size, W}, {fixed, true}, {default, empty}]) end, C),
    #grid{height=H, width=W, rows=A2D}.

query(G, Y, X) ->
    R = array:get(mod(Y, G#grid.height), G#grid.rows),
    array:get(mod(X, G#grid.width), R).

draw(V) ->
    case V of
        empty -> " ";
        alive -> "x"
    end.

assign(G, Y, X, S) ->
    R = array:get(mod(Y, G#grid.height), G#grid.rows),
    U = array:set(mod(X, G#grid.width), S, R),
    A = array:set(mod(Y, G#grid.height), U, G#grid.rows),
    G#grid{rows=A}.

print(G) ->
    CIndex = lists:seq(0, G#grid.width - 1),
    RIndex = lists:seq(0, G#grid.height - 1),
    lists:foreach(fun(Y) ->
        io:format("|"),
        lists:foreach(fun(X) ->
            io:format(" ~s |", [draw(query(G, Y, X))]) end, CIndex),
        io:format("~n")
    end, RIndex).

demo() ->
    G = new(5, 9),
    G1 = assign(G, 0, 3, alive),
    G2 = assign(G1, 1, 4, alive),
    G3 = assign(G2, 2, 2, alive),
    G4 = assign(G3, 2, 3, alive),
    assign(G4, 2, 4, alive).