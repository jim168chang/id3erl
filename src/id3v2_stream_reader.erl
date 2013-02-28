%% Copyright
-module(id3v2_stream_reader).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").
-include("../include/id3v2.hrl").
-define(MAJOR_VERSION, 32#04).
-define(REVISION, 32#00).
-define(TIMEOUT, 3000).

-define(ID3_FRAME_HEADER_SIZE, 10).
-define(ID3_EXT_HEADER_SIZE, 6).
-define(ID3_HEADER_SIZE, 10).

%% API
-export([test/1, start/1, get_header/1, get_frame/2, stop/1, get_tag/1]).

test(FileName) ->
    io:format("Start~n", []),
    {ok, Stream} = stream:create_by_file(FileName),
    io:format("Stream~n", []),
    Srv = start(Stream),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
     io:format("Get frame TPE1: ~p~n", [get_frame(Srv, "TPE1")]),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
    io:format("Get frame AAAA: ~p~n", [get_frame(Srv, "AAAA")]),
    io:format("Get frame TIT2: ~p~n", [get_frame(Srv, "TIT2")]),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
    io:format("Next frame: ~p~n", [read_next_frame(Srv)]),
    io:format("Tag: ~p~n", [get_tag(Srv)]),
    stop(Srv)
.



start(Stream) -> spawn(fun() -> loop({Stream, #id3_tag{}}) end).

loop({Stream, Tag}) ->
    receive

        {From, {get_header}} ->
            UpdTag = update_tag_fill_header(Tag, Stream),
            From ! {self(), UpdTag#id3_tag.header},
            loop({Stream, UpdTag});

        {From, {get_ext_header}} ->
            UpdTag = update_tag_fill_header(Tag, Stream), %% Make sure, that all previous information is readed
            UpdTag2 = update_tag_fill_ext_header(UpdTag, Stream),
            From ! {self(), UpdTag2#id3_tag.ext_header},
            loop({Stream, UpdTag2});

        {From, {get_frame, Id}} ->
            UpdTag = update_tag_fill_header(Tag, Stream), %% Make sure, that all previous information is readed
            UpdTag2 = update_tag_fill_ext_header(UpdTag, Stream),
            case UpdTag2#id3_tag.frames#id3_frames.state of
                full -> case find_readed(UpdTag2#id3_tag.frames#id3_frames.list, Id) of
                    {ok, Frame} ->
                        From ! {self(), Frame},
                        loop({Stream, UpdTag2});
                    _ ->
                        From ! {self(), {error, not_found}},
                        loop({Stream, UpdTag2})
                end;
                partly ->
                    case find_readed(UpdTag2#id3_tag.frames#id3_frames.list, Id) of
                        {ok, Frame} ->
                            From ! {self(), Frame},
                            loop({Stream, UpdTag2});
                        _ ->
                            case find_frame_in_stream(Stream, UpdTag2, UpdTag2#id3_tag.frames, Id) of
                                {ok, {Frame, NewFrames}} ->
                                    From ! {self(), Frame},
                                    loop({Stream, UpdTag2#id3_tag{frames = NewFrames}});
                                {error, {Why, NewFrames}} ->
                                    From ! {self(), {error, Why}},
                                    loop({Stream, UpdTag2#id3_tag{frames = NewFrames}})
                            end
                    end
            end;

        {From, {read_next_frame}} ->
            UpdTag = update_tag_fill_header(Tag, Stream), %% Make sure, that all previous information is readed
            UpdTag2 = update_tag_fill_ext_header(UpdTag, Stream),
            case UpdTag2#id3_tag.frames#id3_frames.state of
                full ->
                    From ! {self(), {error, no_more_frames}},
                    loop({Stream, UpdTag2});
                partly ->
                    Frame = read_frame(Stream),
                    From ! {self(), Frame},
                    NewFrames = case Frame of
                        padding -> update_frames_add_padding(UpdTag2, UpdTag2#id3_tag.frames);
                        _ -> update_frames_add_frame(UpdTag2, UpdTag2#id3_tag.frames, Frame)
                    end,
                    UpdTag3 = UpdTag2#id3_tag{frames = NewFrames},
                    loop({Stream, UpdTag3})
            end;


        {_From, {get_footer}} ->
            %% todo: implement
            not_implemented;

        {From, {get_tag}} ->
            From ! {self(), Tag},
            loop({Stream, Tag});

        {_From, {stop}} ->
            io:format("Bye!~n");

        {_From, {new_stream, NewStream}} ->
            loop({NewStream, #id3_tag{}});

        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop({Stream, Tag})

    end,
    loop({Stream, Tag}).


%%%%%%%%
%% Functions for update tag
update_tag_fill_header(Tag, Stream) ->
    case Tag#id3_tag.header of
        undefined ->
            Header = read_header(Stream),
            Tag#id3_tag{header = Header};
        _ -> Tag
    end.

update_tag_fill_ext_header(Tag, Stream) ->
    case Tag#id3_tag.ext_header of
        undefined ->
            ExtHeader = case Tag#id3_tag.header#id3_header.flags#id3_header_flags.ext_header of
                true -> read_ext_header(Stream);
                _ -> not_present
            end,
            Tag#id3_tag{ext_header = ExtHeader};
        _ -> Tag
    end.

%%%%%%%%
%% Functions for update frame list
update_frames_add_padding(Tag, Frames) ->
    PaddingSize = Tag#id3_tag.header#id3_header.size
        - Frames#id3_frames.size
        - get_ext_header_size(Tag#id3_tag.ext_header),
    NewFrames = Frames#id3_frames{
            list = [{padding, PaddingSize} | Frames#id3_frames.list],
            size = Frames#id3_frames.size + PaddingSize
    },
    update_frames_state(Tag, NewFrames).

update_frames_add_frame(Tag, Frames, Frame) ->
    NewSize = Frames#id3_frames.size + Frame#id3_frame.size + ?ID3_FRAME_HEADER_SIZE,
    NewFrames = Frames#id3_frames{
            list = [Frame | Frames#id3_frames.list],
            size = NewSize
    },
    update_frames_state(Tag, NewFrames).


update_frames_state(Tag, NewFrames) ->
    FramesSize = Tag#id3_tag.header#id3_header.size - get_ext_header_size(Tag#id3_tag.ext_header),
    RestSize = FramesSize - NewFrames#id3_frames.size,
    case RestSize of
        Zero when Zero =< 0 -> NewFrames#id3_frames{state = full, list = lists:reverse(NewFrames#id3_frames.list)};
        _ -> NewFrames
    end.


%% Found tag within already readed
find_readed(undefined, _Id) -> {error,not_found};
find_readed([], _Id) ->  {error,not_found};
find_readed([Frame = #id3_frame{id=Id}|_], Id) -> {ok, Frame};
find_readed([_|Rest], Id) -> find_readed(Rest, Id).

%% Read frames from stream, until found needed frame
find_frame_in_stream(Stream, Tag, undefined, Id) -> find_frame_in_stream(Stream, Tag, #id3_frames{}, Id);
find_frame_in_stream(Stream, Tag, Frames, Id) ->
    Frame = read_frame(Stream),
    case Frame of
        padding ->
            {error, {not_found, update_frames_add_padding(Tag, Frames)}};
        _ ->
            NewFrames = update_frames_add_frame(Tag, Frames, Frame),
            case Frame#id3_frame.id of
                Id -> {ok, {Frame, NewFrames}};
                _ ->
                    find_frame_in_stream(Stream, Tag, NewFrames, Id)
            end
    end.

get_ext_header_size(not_present) -> 0;
get_ext_header_size(ExtHeader) -> ExtHeader#id3_ext_header.size + 6.



%%%%%%%%
%% Read routine
read_header(Stream) ->
    case stream:read(Stream, ?ID3_HEADER_SIZE) of
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

read_ext_header(Stream) ->
    case stream:read(Stream,?ID3_EXT_HEADER_SIZE) of
        {ok, <<SyncsafeSize:32,32#01:8,BinFlags/binary>>} ->
            {Update,CRC,Rests} = case BinFlags of
                <<2#0:1,_Update:1,_CRC:1,_Rests:1,2#0000:4>> ->
                    {int_to_bool(_Update),int_to_bool(_CRC),int_to_bool(_Rests)}
            end,
            UpdateFlag = read_ext_header_flag(Stream, update, Update),
            CRCFlag = read_ext_header_flag(Stream, crc, CRC),
            RestsFlag = read_ext_header_flag(Stream, rests, Rests),
            #id3_ext_header{
                    size = read_syncsafe_int(SyncsafeSize),
                    flags = [UpdateFlag,CRCFlag,RestsFlag]
            }
    end.

read_ext_header_flag(File, Name, true) ->
    {ok, <<FlagLength>>} = file:read(File, 1),
    {ok, FlagData} = file:read(File, FlagLength),
    #id3_ext_header_flag{name = Name, value = true, data = FlagData};
read_ext_header_flag(_File, Name, _False) -> #id3_ext_header_flag{name = Name, value = false}.


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
get_tag(Srv) -> misc:call(Srv, {get_tag}, ?TIMEOUT).
read_next_frame(Srv) -> misc:call(Srv, {read_next_frame}, ?TIMEOUT).
stop(Srv) -> misc:cast(Srv, {stop}).

%%
%% Common routine

%% Convert syncsafe integer to real integer
read_syncsafe_int(<<0:1, ForthByte:7, 0:1, ThirdByte:7, 0:1, SecondByte:7, 0:1, FirstByte:7>>) ->
    <<Int:32>> = <<0:4, ForthByte:7, ThirdByte:7, SecondByte:7, FirstByte:7>>, Int;
read_syncsafe_int(Int) -> read_syncsafe_int(<<Int:32>>).

%% Convert number to boolean atom
int_to_bool(1) -> true; int_to_bool(_) -> false.