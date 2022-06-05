-module(gol).
-export([new/2, query/3, assign/4, print/1, draw/1, demo/0, count/3, logic/2, step/3, simulate/1, clear/0, finish/0, run/2]).

% new: Neues Board erstellen
% query: Status einer Zelle abfragen
% assign: Status einer Zelle setzen
% print: Board ausgeben
% draw: Zellstatus formatieren
% step: Einen Zelle weiterentwickeln
% simulation: Eine Generation weitergehen
% run: Mehrere Generationen weitergehen
% demo: Demo Board erstellen
% count: Nachbarn zählen
% logic: Zelle verändern
% clear: Bildschirm leeren
% finish: Simulation fertigstellen

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

count(G, Y, X) ->
    N  = query(G, Y+1, X+0), 
    NE = query(G, Y+1, X+1), 
    E  = query(G, Y+0, X+1), 
    SE = query(G, Y-1, X+1), 
    S  = query(G, Y-1, X+0), 
    SW = query(G, Y-1, X-1), 
    W  = query(G, Y+0, X-1), 
    NW = query(G, Y+1, X-1),
    M = [N, NE, E, SE, S, SW, W, NW],
    length(lists:filter(fun(ST) -> ST == alive end, M)).

assign(G, Y, X, S) ->
    R = array:get(mod(Y, G#grid.height), G#grid.rows),
    U = array:set(mod(X, G#grid.width), S, R),
    A = array:set(mod(Y, G#grid.height), U, G#grid.rows),
    G#grid{rows=A}.

logic(S, N) ->
    case S of 
        alive -> if
            N < 2 -> empty;
            N > 3 -> empty;
            true -> S
        end;
        empty -> if
            N == 3 -> alive;
            true -> S
        end
    end.

step(G, Y, X) ->
    S = query(G, Y, X),
    N = count(G, Y, X),
    Next = logic(S, N),
    #transition{y=Y, x=X, state=Next}.

simulate(G) ->
    R = array:map(fun(Y, R) ->
        array:map(fun(X, _V) ->
            T = step(G, Y, X),
            T#transition.state
        end, R)
    end, G#grid.rows),
    G#grid{rows=R}.

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
    G = new(20, 20),
    G1 = assign(G, 0, 3, alive),
    G2 = assign(G1, 1, 4, alive),
    G3 = assign(G2, 2, 2, alive),
    G4 = assign(G3, 2, 3, alive),
    assign(G4, 2, 4, alive).

clear() ->
    io:format("\ec").

run(G, N) ->
    clear(),
    U = simulate(G),
    print(U),
    timer:sleep(500),
    case N > 0 of
        true -> run(U, N-1);
        false -> finish()
    end.

finish() ->
    io:format("~nFertig!").