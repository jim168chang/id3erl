%% Copyright
-module(id3v2).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([test/1,file_full/1,file_stream/1,get_footer/1,get_tag/1,read_next_frame/1,get_frame/2,get_header/1,stop/1]).

test(FileName) ->
    io:format("Start~n", []),
    {ok, Srv} = id3v2:file_stream(FileName),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
     io:format("Get frame TPE1: ~p~n", [get_frame(Srv, "TPE1")]),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
%%     io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
%%     io:format("Get frame AAAA: ~p~n", [get_frame(Srv, "AAAA")]),
%%     io:format("Get frame TIT2: ~p~n", [get_frame(Srv, "TIT2")]),
%%     io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
%%     io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
    io:format("Footer: ~p~n", [get_footer(Srv)]),
%%     io:format("Tag: ~p~n", [get_tag(Srv)]),
    stop(Srv)
.


%% Read full tag from file
file_full(FileName) -> id3v2_file_reader:read_file(FileName).

%% Start process for read file on demand
file_stream(FileName) ->

    %% Start error handling intermediate process
    HandlingPid = spawn(fun() ->
        {ok, FileStream} = id3v2_stream:create_by_file(FileName),
        {ok, Srv} = id3v2_stream_reader:start(FileStream),
        {ok, {id3v2_stream, Srv}},
        handling_loop(Srv)
    end),
    {ok, {id3v2_stream, HandlingPid}}.


handling_loop_failed(Pid) ->
    receive
        {From, {stop}} ->
            From ! {self(), {ok, stopped}};
        {From, _Message} ->
            From ! {self(), {error, server_down}},
            handling_loop_failed(Pid);
        Other ->
            io:format("Message doesn't contains sender PID: ~p~n", [Other]),
            handling_loop(Pid)
    end.

handling_loop(Pid) ->
    receive
        {From, {stop} = Message} ->
            Pid ! {self(), Message},
            From ! {ok, stopped};
        {From, Message} ->
            Pid ! {self(), Message},
            receive
                {Pid, Response} ->
                    From ! {self(), Response},
                    handling_loop(Pid);
                {'EXIT', _SomePid, _Reason} ->
                    From ! {self(), {error, server_down}},
                    handling_loop_failed(Pid)
            end;
        {'EXIT', _SomePid, _Reason} ->
            handling_loop_failed(Pid);
        Other ->
            io:format("Message doesn't contains sender PID: ~p~n", [Other]),
            handling_loop(Pid)
    end.

get_header({id3v2_stream, Srv}) -> id3v2_stream_reader:get_header(Srv).
get_footer({id3v2_stream, Srv}) -> id3v2_stream_reader:get_footer(Srv).
get_frame({id3v2_stream, Srv}, Id) -> id3v2_stream_reader:get_frame(Srv, Id).
get_tag({id3v2_stream, Srv}) -> id3v2_stream_reader:get_tag(Srv).
read_next_frame({id3v2_stream, Srv}) -> id3v2_stream_reader:read_next_frame(Srv).
stop({id3v2_stream, Srv}) -> id3v2_stream_reader:stop(Srv).
