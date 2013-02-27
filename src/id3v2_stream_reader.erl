%% Copyright
-module(id3v2_stream_reader).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").
-include("../include/id3v2.hrl").
-define(MAJOR_VERSION, 32#04).
-define(REVISION, 32#00).
-define(TIMEOUT, 3000).


%% API
-export([test/1, start/1, get_header/1, get_frame/2, stop/1]).

test(FileName) ->
    io:format("Start~n", []),
    {ok, Stream} = stream:create_by_file(FileName),
    io:format("Stream~n", []),
    Srv = start(Stream),
    io:format("Read~n", []),
    Header = get_header(Srv),
    io:format("Header: ~p~n", [Header]),
    TPE1 = get_frame(Srv, "TPE1"),
    io:format("Frame TPE1: ~p~n", [TPE1]),
    TIT2 = get_frame(Srv, "TIT2"),
    io:format("Frame TIT2: ~p~n", [TIT2]),
    Wrong = get_frame(Srv, "WRNG"),
    io:format("Wrong: ~p~n", [Wrong]),
    TALB = get_frame(Srv, "TALB"),
    io:format("Frame TALB: ~p~n", [TALB]),
    stop(Srv)
.



start(Stream) -> spawn(fun() -> loop({Stream, #id3_tag{}}) end).

loop({Stream, Tag}) ->
    receive

        {From, {get_header}} ->
            NewTag = case Tag#id3_tag.header of
                undefined ->
                    Header = read_header(Stream),
                    Tag#id3_tag{header = Header};
                _ -> Tag
            end,
            From ! {self(), NewTag#id3_tag.header},
            loop({Stream, NewTag});

        {From, {get_frame, Id}} ->
            case find_readed(Tag#id3_tag.frames, Id) of
                {ok, Frame} ->
                    From ! {self(), Frame},
                    loop({Stream, Tag});
                _ ->
                    case find_frame(Stream, Tag#id3_tag.frames, Id) of
                        {ok, {Frame, NewFrames}} ->
                            From ! {self(), Frame},
                            NewTag = Tag#id3_tag{frames = NewFrames},
                            loop({Stream, NewTag});
                        {error, {Why, NewFrames}} ->
                            From ! {self(), {error, Why}},
                            NewTag = Tag#id3_tag{frames = NewFrames},
                            loop({Stream, NewTag})
                    end
            end;

        {From, {stop}} ->
             io:format("Bye!~n");

        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop({Stream, Tag})

    end,
    loop({Stream, Tag}).

%% Found tag within already readed
find_readed(undefined, _Id) -> {error,not_found};
find_readed([], _Id) ->  {error,not_found};
find_readed([Frame = #id3_frame{id=Id}|_], Id) -> {ok, Frame};
find_readed([_|Rest], Id) -> find_readed(Rest, Id).

%% Read frames from stream, until found needed frame
find_frame(Stream, undefined, Id) -> find_frame(Stream, [], Id);
find_frame(Stream, Readed, Id) ->
    Frame = read_frame(Stream),
    case Frame of
        padding -> {error, {not_found, Readed}};
        _ ->
            case Frame#id3_frame.id of
                Id -> {ok, {Frame, [Frame|Readed]}};
                _ -> find_frame(Stream, [Frame|Readed], Id)
            end
    end.



%%%%%%%%
%% Read routine
read_header(Stream) ->
    case stream:read(Stream, 10) of
        {ok, <<"ID3", ?MAJOR_VERSION:8, ?REVISION:8, Flags:1/binary, SyncsafeSize:32>>} ->
            case Flags of
                <<Unsync:1, Ext:1, Exp:1, Footer:1, 2#0000:4>> ->
                    #id3_header{
                            version = #id3_header_version{major = ?MAJOR_VERSION, revision = ?REVISION},
                            flags = #id3_header_flags{
                                    unsinc = int_to_bool(Unsync),
                                    ext_header = int_to_bool(Ext),
                                    experiment = int_to_bool(Exp),
                                    footer = int_to_bool(Footer)},
                            size = read_syncsafe_int(SyncsafeSize)
                    }
            end;
         Other -> io:format("Other output: ~p~n", [Other])
    end.

read_frame(Stream) ->
    {ok, <<BinId:4/binary,SyncsafeSize:32,Flags:2/binary>>} = stream:read(Stream, 10),
    Size = read_syncsafe_int(SyncsafeSize),
    case Size of
        0 -> padding;
        _ ->
            Id = binary_to_list(BinId),
            RawData = read_frame_data(Stream, Size),
            ParsedData = id3v2_native_frames:parse(Id, RawData),
            #id3_frame{
                    id = Id,
                    size = Size,
                    flags = Flags,
                    data = ParsedData,
                    raw_data = RawData
            }
    end.

read_frame_data(Stream, Size) ->
    {ok, Data} = stream:read(Stream, Size), Data.


%%%%%%%%
%% API
get_header(Srv) -> misc:call(Srv, {get_header}, ?TIMEOUT).
get_frame(Srv, Id) -> misc:call(Srv, {get_frame, Id}, ?TIMEOUT).
stop(Srv) -> misc:cast(Srv, {stop}).

%%
%% Common routine

%% Convert syncsafe integer to real integer
read_syncsafe_int(<<0:1, ForthByte:7, 0:1, ThirdByte:7, 0:1, SecondByte:7, 0:1, FirstByte:7>>) ->
    <<Int:32>> = <<0:4, ForthByte:7, ThirdByte:7, SecondByte:7, FirstByte:7>>, Int;
read_syncsafe_int(Int) -> read_syncsafe_int(<<Int:32>>).

%% Convert number to boolean atom
int_to_bool(1) -> true; int_to_bool(_) -> false.