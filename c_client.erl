-module(c_client).
-export([changecase/3]).

changecase(Server, Str, Command) ->
    Server ! {self(), {Str, Command}},
    receive
        {Server, ResultString} ->
            ResultString
    end.