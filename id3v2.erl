%% Copyright
-module(id3v2).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([start/0,read_tag/1,read_syncsafe_int/1]).
-include("id3v2.hrl").
-define(MAJOR_VERSION,32#04).
-define(REVISION,32#00).

start() ->
    FileName = "test.txt",
    Mp3FileName = "2.mp3",
    id3v2_test:start(FileName),
    read_tag(Mp3FileName)
.



read_tag(FileName) ->
    {ok, File} = file:open(FileName, [read,binary]),
    Header = read_header(File),

    ExtHeader = case Header#header.flags#header_flags.ext_header of
        true -> read_ext_header(File);
        _ -> not_present
    end,

    FramesSize = Header#header.size - get_ext_header_size(ExtHeader),
    Frames = read_frames(File, FramesSize),

    Footer = case Header#header.flags#header_flags.footer of
        true -> read_footer(File);
        _ -> not_present
    end,

    #tag{
            header = Header,
            ext_header = ExtHeader,
            frames = Frames,
            footer = Footer
    }.


%%%%%%%%
%% Header routine
read_header(File) ->
    case file:read(File,10) of
        {ok, <<"ID3",?MAJOR_VERSION:8 ,?REVISION:8,Flags:1/binary,SyncsafeSize:32>>} ->
            case Flags of
                <<Unsync:1,Ext:1,Exp:1,Footer:1,2#0000:4>> ->
                    #header{
                            version=#header_version{major=?MAJOR_VERSION,revision=?REVISION},
                            flags=#header_flags{
                                    unsinc=int_to_bool(Unsync),
                                    ext_header =int_to_bool(Ext),
                                    experiment =int_to_bool(Exp),
                                    footer=int_to_bool(Footer)},
                            size=read_syncsafe_int(SyncsafeSize)
                    }
            end
    end.

%%%%%%%%
%% Ext header routine
read_ext_header(File) ->
    case file:read(File,6) of
        {ok, <<SyncsafeSize:32,32#01:8,BinFlags/binary>>} ->
            {Update,CRC,Rests} = case BinFlags of
                <<2#0:1,_Update:1,_CRC:1,_Rests:1,2#0000:4>> ->
                    {int_to_bool(_Update),int_to_bool(_CRC),int_to_bool(_Rests)}
            end,
            UpdateFlag = read_ext_header_flag(File, update, Update),
            CRCFlag = read_ext_header_flag(File, crc, CRC),
            RestsFlag = read_ext_header_flag(File, rests, Rests),
            #ext_header{
                size = read_syncsafe_int(SyncsafeSize),
                flags = [UpdateFlag,CRCFlag,RestsFlag]
            }
    end.

read_ext_header_flag(File, Name, true) ->
    {ok, <<FlagLength>>} = file:read(File, 1),
    {ok, FlagData} = file:read(File, FlagLength),
    #ext_header_flag{name = Name, value = true, data = FlagData};
read_ext_header_flag(_File, Name, _False) -> #ext_header_flag{name = Name, value = false}.

get_ext_header_size(not_present) -> 0;
get_ext_header_size(ExtHeader) -> ExtHeader#ext_header.size + 6.


%%%%%%%%
%% Frames routine

read_frames(File, FramesSize) -> read_frames1(File,FramesSize,[]).

read_frames1(_File, SubZero, Frames) when SubZero < 0 -> lists:reverse(Frames);
read_frames1(_File, 0, Frames) -> lists:reverse(Frames);
read_frames1(File, FramesSize, Frames) ->
    Frame = read_frame(File),
    case Frame of
        padding -> read_frames1(File, 0, [padding|Frames]);
        _ ->
            Size = Frame#frame.size + 10,
            read_frames1(File, FramesSize - Size, [Frame|Frames])
    end.

read_frame(File) ->
    {ok, <<BinId:4/binary,SyncsafeSize:32,Flags:2/binary>>} = file:read(File, 10),
    Size = read_syncsafe_int(SyncsafeSize),
    case Size of
        0 -> padding;
        _ ->
            Id = binary_to_list(BinId),
            RawData = read_frame_data(File, Size),
            ParsedData = id3v2_native_frames:parse(Id, RawData),
            #frame{
                id = Id,
                size = Size,
                flags = Flags,
                data = ParsedData,
                raw_data = RawData
            }
    end.

read_frame_data(File, Size) ->
    {ok, Data} = file:read(File, Size), Data.


%%%%%%%%
%% Footer routine
read_footer(File) ->
    case file:read(File,10) of
        {ok, <<"3DI",?MAJOR_VERSION:8 ,?REVISION:8,Flags:1/binary,Size:32>>} ->
            case Flags of
                <<Unsync:1,Ext:1,Exp:1,Footer:1,2#0000:4>> ->
                    #footer{
                            version=#footer_version{major=?MAJOR_VERSION,revision=?REVISION},
                            flags=#footer_flags{
                                    unsinc=int_to_bool(Unsync),
                                    ext_header =int_to_bool(Ext),
                                    experiment =int_to_bool(Exp),
                                    footer=int_to_bool(Footer)},
                            size=Size
                    }
            end
    end.


%%
%% Common routine

%% Convert syncsafe integer to real integer
read_syncsafe_int(<<0:1,ForthByte:7, 0:1, ThirdByte:7, 0:1, SecondByte:7, 0:1,FirstByte:7>>) ->
    <<Int:32>> = <<0:4,ForthByte:7,ThirdByte:7,SecondByte:7,FirstByte:7>>, Int;
read_syncsafe_int(Int) -> read_syncsafe_int(<<Int:32>>).

%% Convert number to boolean atom
int_to_bool(1) -> true; int_to_bool(_) -> false.