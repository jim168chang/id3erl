%% Copyright
-module(misc).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([call/3, cast/2]).


call(Id, Message, Timeout) ->
    Id ! {self(), Message},
    receive
        {Id, Response} -> Response
    after
        Timeout -> {error, timeout}
    end.

cast(Id, Message) ->
    Id ! {self(), Message}.