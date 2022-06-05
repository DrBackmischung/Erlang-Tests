-module(helloworld).
-export([hello/0]).
hello() ->
    io:fwrite("Hello World!").