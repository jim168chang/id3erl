%% Copyright
-module(id3v2_stream).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([create_by_file/1, read/2]).
-define(TIMEOUT, 3000).

%% todo: move to another application

loop(File) ->
    receive
        {From, {read, Length}} ->
            From ! {self(), file:read(File, Length)}
    end,
    loop(File).

create_by_file(FileName) ->
    {ok, File} = file:open(FileName, [read, binary]),
    Pid = spawn(fun() -> loop(File) end),
    {ok, Pid}.

read(Id, Length) -> id3v2_misc:call(Id, {read, Length}, ?TIMEOUT).