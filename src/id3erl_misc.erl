%% Copyright
-module(id3erl_misc).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([call/3, cast/2, read_syncsafe_int/1, int_to_bool/1]).


call(Id, Message, Timeout) ->
    Id ! {self(), Message},
    receive
        {Id, Response} -> Response
    after
        Timeout -> {error, timeout}
    end.

cast(Id, Message) ->
    Id ! {self(), Message}.

%% Convert syncsafe integer to real integer
read_syncsafe_int(<<0:1,ForthByte:7, 0:1, ThirdByte:7, 0:1, SecondByte:7, 0:1,FirstByte:7>>) ->
    <<Int:32>> = <<0:4,ForthByte:7,ThirdByte:7,SecondByte:7,FirstByte:7>>, Int;
read_syncsafe_int(Int) -> read_syncsafe_int(<<Int:32>>).

%% Convert number to boolean atom
int_to_bool(1) -> true; int_to_bool(_) -> false.