# Erlang-Tests

## Server/Client
c_client.erl requests a String to be sent back all lower/upper case

c_server.erl gets the request and sends the string back

## Game of Life
gol.erl lets you simulate the Game of Life:
``` Erlang
c(gol).
Grid = gol:demo().
gol:run(Grid, 10).
```
